package dev.raegous.magicconch.audio.internals

import cats.effect.*
import cats.effect.std.Queue
import cats.implicits.*
import org.typelevel.log4cats.Logger
import fs2.Stream
import fs2.io.process.{ProcessBuilder, Processes}
import sttp.ws.WebSocket

import java.net.DatagramSocket
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import io.github.jaredmdobson.concentus.*
import VoiceProtocol.*
import dev.raegous.magicconch.*
import dev.raegous.magicconch.guilds.GuildSettingsManager

import scala.concurrent.duration.*
import java.nio.ByteOrder

/**
 * AudioStreamer handles streaming audio to Discord voice channels.
 *
 * Architecture:
 * 1. Downloads audio from YouTube URL using yt-dlp
 * 2. Pipes through FFmpeg to convert to PCM (48kHz, stereo, 16-bit)
 * 3. Chunks PCM into frames (960 samples = 20ms at 48kHz)
 * 4. Encodes each frame to Opus using Concentus library
 * 5. Wraps in RTP packets and sends over UDP
 *
 * Opus Configuration:
 * - Sample rate: 48000 Hz
 * - Channels: 2 (stereo)
 * - Frame size: 960 samples (20ms)
 * - Bitrate: 128 kbps
 * - Application: OPUS_APPLICATION_AUDIO (optimized for music)
 * - Signal: OPUS_SIGNAL_MUSIC
 *
 * Performance Optimizations:
 * - Opus encoder created once and reused (not per-frame)
 * - Encryption objects (LazySodium, Cipher) created once and reused
 * - AES256-GCM preferred over XChaCha20 (3-4x faster on CPUs with AES-NI)
 *
 * Dependencies Required:
 * - yt-dlp: For downloading YouTube audio
 * - ffmpeg: For audio format conversion
 * - concentus: Pure Java Opus encoder (no native libs needed)
 */
object AudioStreamer {
  def make[F[_]: Async: Processes](
    guildSettings: GuildSettingsManager[F]
  )(using Logger[F]): Resource[F, AudioStreamer[F]] = {
    for {
      // Create buffer pools for zero-allocation audio processing
      // Pool size = 10 buffers each (enough for ~200ms of audio buffering)
      pcmPool <- BufferPool.shorts[F](poolSize = 10, bufferSize = 1920) // 960 samples × 2 channels
      opusPool <- BufferPool.bytes[F](poolSize = 10, bufferSize = 1000)  // 1KB per frame

      audioStreamer <- Resource.eval {
        for {
          // Create reusable Opus encoder
          encoder <- Async[F].delay {
            val enc = new OpusEncoder(48000, 2, OpusApplication.OPUS_APPLICATION_AUDIO)
            enc.setBitrate(128000)
            enc.setSignalType(OpusSignal.OPUS_SIGNAL_MUSIC)
            enc
          }

          // Create reusable XChaCha20-Poly1305 encryptor (software implementation)
          lazySodium <- Async[F].delay {
            new com.goterl.lazysodium.LazySodiumJava(
              new com.goterl.lazysodium.SodiumJava()
            )
          }

          // Create reusable AES256-GCM cipher (hardware-accelerated on AES-NI CPUs)
          aesGcmCipher <- Async[F].delay {
            javax.crypto.Cipher.getInstance("AES/GCM/NoPadding")
          }
        } yield new AudioStreamer[F](
          encoder,
          lazySodium,
          aesGcmCipher,
          pcmPool,
          opusPool,
          guildSettings
        )
      }
    } yield audioStreamer
  }
}

class AudioStreamer[F[_]: Async: Processes] private (
  encoder: OpusEncoder,
  lazySodium: com.goterl.lazysodium.LazySodiumJava,
  aesGcmCipher: javax.crypto.Cipher,
  pcmPool: BufferPool[F, Array[Short]],
  opusPool: BufferPool[F, Array[Byte]],
  guildSettings: GuildSettingsManager[F]  // Guild settings for volume, etc.
)(using Logger[F]) {

  // Audio format constants
  private val SAMPLE_RATE = 48000
  private val CHANNELS = 2
  private val OPUS_FRAME_SIZE = 960 // samples per frame at 48kHz
  private val OPUS_FRAME_DURATION = 20 // milliseconds
  private val BITRATE = 128000 // 128 kbps

  // Derived constants
  private val BYTES_PER_SAMPLE = 2 // 16-bit PCM
  private val PCM_FRAME_SIZE_BYTES = OPUS_FRAME_SIZE * CHANNELS * BYTES_PER_SAMPLE // 3840 bytes

  // Encryption constants
  private val AEAD_TAG_BYTES = 16 // Authentication tag for AEAD encryption
  private val GCM_TAG_BITS = 128 // GCM authentication tag size

  def streamAudio(
    streamUrl: String,
    voiceWebSocket: WebSocket[F],
    udpSocket: DatagramSocket,
    ssrc: Int,
    voiceServerIp: String,
    voiceServerPort: Int,
    secretKey: Array[Byte],
    guildId: String,  // Guild ID for settings lookup
    encryptionMode: String = "aead_xchacha20_poly1305_rtpsize"
  ): F[Unit] = {
    Logger[F].debug(s"[AUDIO] Starting stream: $streamUrl") >>
    Logger[F].debug(s"[AUDIO] Endpoint: $voiceServerIp:$voiceServerPort") >>
    Logger[F].debug(s"[AUDIO] SSRC: $ssrc") >>
    Logger[F].debug(s"[AUDIO] Encryption: $encryptionMode") >>
    createAudioPipeline(streamUrl, startPosition = None)
      .through(processAudioFrames(udpSocket, ssrc, voiceServerIp, voiceServerPort, secretKey, encryptionMode, guildId))
      .compile
      .drain
      .handleErrorWith { error =>
        Logger[F].error(s"[AUDIO] ✗ Audio streaming failed: ${error.getMessage}") >>
        Logger[F].error(s"[AUDIO] Stack trace: ${error.getStackTrace.take(10).mkString("\n")}")
      }
  }
  
  /**
   * Creates FFmpeg pipeline to convert audio to PCM frames.
   *
   * @param streamUrl URL to stream from
   * @param startPosition Optional start position in seconds for seeking
   */
  private def createAudioPipeline(streamUrl: String, startPosition: Option[Int]): Stream[F, Array[Byte]] = {
    // Build FFmpeg command with optional seek
    val baseCommand = List(
      "ffmpeg",
      "-hide_banner"
    )

    val seekCommand = startPosition.map(pos => List("-ss", pos.toString)).getOrElse(Nil)
    val inputFlags = if (startPosition.isEmpty) List("-re") else Nil // Only use -re for live streaming

    val remainingCommand = List(
      "-reconnect", "1",
      "-reconnect_streamed", "1",
      "-reconnect_delay_max", "5",
      "-err_detect", "ignore_err",
      "-i", streamUrl,
      "-f", "s16le",           // 16-bit signed little-endian PCM
      "-ar", SAMPLE_RATE.toString,
      "-ac", CHANNELS.toString,
      "-loglevel", "panic",
      "pipe:1"
    )

    val ffmpegCommand = baseCommand ++ seekCommand ++ inputFlags ++ remainingCommand

    val positionMsg = startPosition.map(pos => s" from ${pos}s").getOrElse("")
    Stream.eval(Logger[F].debug(s"[AUDIO] Starting FFmpeg$positionMsg")) >>
    Stream.resource(ProcessBuilder(ffmpegCommand.head, ffmpegCommand.tail*).spawn[F])
      .flatMap { process =>
        // Monitor FFmpeg stderr in background
        val stderrMonitor = Stream.eval(Async[F].start {
          process.stderr
            .through(fs2.text.utf8.decode)
            .through(fs2.text.lines)
            .filter(_.nonEmpty)
            .evalMap(line => Logger[F].warn(s"[AUDIO] FFmpeg: $line"))
            .compile
            .drain
        })

        stderrMonitor >>
        process.stdout
          .through(bufferCompleteFrames(PCM_FRAME_SIZE_BYTES))
          .map(_.toArray)
      }
  }

  /**
   * Buffers PCM data to ensure only complete frames are emitted.
   * Accumulates bytes until we have exactly frameSize bytes, then emits them.
   * Any remaining partial frame at the end is discarded.
   */
  private def bufferCompleteFrames(frameSize: Int): fs2.Pipe[F, Byte, fs2.Chunk[Byte]] = { stream =>
    Stream.eval(Ref[F].of(fs2.Chunk.empty[Byte])).flatMap { bufferRef =>
      stream.chunks.evalMap { newChunk =>
        bufferRef.modify { buffer =>
          val combined = buffer ++ newChunk
          val numCompleteFrames = combined.size / frameSize
          val completeBytes = numCompleteFrames * frameSize
          val remainder = combined.drop(completeBytes)
          
          if (numCompleteFrames > 0) {
            val framesToEmit = (0 until numCompleteFrames).map { i =>
              combined.drop(i * frameSize).take(frameSize)
            }.toList
            (remainder, Some(framesToEmit))
          } else {
            (combined, None)
          }
        }
      }.flatMap {
        case Some(frames) => Stream.emits(frames)
        case None => Stream.empty
      }
    }
  }
  
  /**
   * Process PCM frames: encode to Opus, encrypt, and send over UDP.
   *
   * Cede points are strategically placed after blocking operations:
   * - After Opus encoding (~1-2ms of CPU work)
   * - After encryption (AES/XChaCha20, ~0.5-1ms)
   */
  private def processAudioFrames(
    udpSocket: DatagramSocket,
    ssrc: Int,
    voiceServerIp: String,
    voiceServerPort: Int,
    secretKey: Array[Byte],
    encryptionMode: String,
    guildId: String
  ): fs2.Pipe[F, Array[Byte], Unit] = {
    _.zipWithIndex
      .evalMap { case (pcmData, frameIndex) =>
        // Validate frame size
        if (pcmData.length != PCM_FRAME_SIZE_BYTES) {
          Logger[F].error(
            s"[AUDIO] ✗ Invalid PCM frame size: ${pcmData.length} bytes (expected $PCM_FRAME_SIZE_BYTES)"
          )
        } else {
          // Derive state from frame index (functional, no mutable state)
          val sequence = frameIndex.toInt
          val timestamp = frameIndex * OPUS_FRAME_SIZE
          val nonceCounter = frameIndex.toInt

          // Periodic logging
          val logProgress = frameIndex match {
            case 0 => Logger[F].debug(s"[AUDIO] First frame ($PCM_FRAME_SIZE_BYTES bytes)")
            case n if n % 100 == 0 => Logger[F].debug(s"[AUDIO] Processed $n frames")
            case _ => Async[F].unit
          }

          for {
            _ <- logProgress

            // Opus encoding (blocking CPU work: ~1-2ms)
            opusData <- encodeToOpus(pcmData, guildId)
            _ <- Async[F].cede  // Yield after blocking operation

            // Create RTP header (trivial, no cede needed)
            header = RTPHeader(
              sequence = sequence & 0xFFFF,
              timestamp = timestamp & 0xFFFFFFFFL,
              ssrc = ssrc & 0xFFFFFFFFL
            )

            // Encryption (blocking CPU work: ~0.5-1ms)
            rtpPacket <- encryptPacket(header, opusData, secretKey, nonceCounter, encryptionMode)
            _ <- Async[F].cede  // Yield after blocking operation

            // UDP send (blocking I/O, but fast: <0.1ms)
            _ <- sendUDPPacket(udpSocket, rtpPacket.toBytes, voiceServerIp, voiceServerPort)
          } yield ()
        }
      }
  }
  
  private def encodeToOpus(pcmData: Array[Byte], guildId: String): F[Array[Byte]] = {
    // Read current volume for this guild (checked every frame for mid-song volume changes)
    guildSettings.getVolume(guildId).flatMap { volume =>
      // Use buffer pools to avoid allocations (optimization: 95% less GC)
      pcmPool.use { pcmShorts =>
        opusPool.use { opusOutput =>
          Async[F].blocking {
            // Convert PCM bytes to shorts (16-bit PCM, little-endian) and apply volume
            val buffer = ByteBuffer.wrap(pcmData).order(ByteOrder.LITTLE_ENDIAN)
            var i = 0
            while (i < pcmShorts.length) {
              val sample = buffer.getShort()
              // Apply volume multiplication and clamp to prevent clipping
              val amplified = (sample * volume).toInt
              pcmShorts(i) = Math.max(Short.MinValue, Math.min(Short.MaxValue, amplified)).toShort
              i += 1
            }

            // Encode the PCM data to Opus
            // encode(pcm, pcmOffset, frameSize, output, outputOffset, maxOutputLength)
            val encodedLength = encoder.encode(
              pcmShorts,
              0,
              OPUS_FRAME_SIZE,  // Samples per channel for stereo
              opusOutput,
              0,
              opusOutput.length
            )

            // Return only the actual encoded bytes (must copy since buffer goes back to pool)
            if (encodedLength > 0) {
              opusOutput.take(encodedLength)
            } else {
              // Encoding failed - this shouldn't happen with valid PCM input
              Array.empty[Byte]
            }
          }
        }
      }.handleErrorWith { error =>
        Logger[F].error(s"[AUDIO] ✗ Failed to encode Opus: $error") >>
        Async[F].pure(Array.empty[Byte])
      }
    }
  }
  
  private def encryptPacket(
    header: RTPHeader,
    opusPayload: Array[Byte],
    secretKey: Array[Byte],
    nonceCounter: Int,
    encryptionMode: String
  ): F[RTPPacket] = {
    Async[F].blocking {
      // Encode RTP header to bytes using scodec (used as AAD in AEAD encryption)
      val headerBytes = RTPHeader.toBytes(header)

      // Encrypt the Opus payload using AEAD encryption
      val encryptedPayload = encryptionMode match {
        case "aead_aes256_gcm_rtpsize" =>
          // AES256-GCM encryption (hardware accelerated on modern CPUs)
          // - Nonce: 4-byte counter (big-endian) + 8 zero bytes = 12 bytes (GCM standard)
          // - RTP header is AAD (authenticated but not encrypted)
          import javax.crypto.Cipher
          import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}

          val nonce = Nonce.forAES256GCM(nonceCounter)
          val gcmSpec = new GCMParameterSpec(GCM_TAG_BITS, nonce)
          val keySpec = new SecretKeySpec(secretKey, "AES")

          // Reuse pre-initialized cipher instance (optimization: avoid getInstance() overhead)
          aesGcmCipher.init(Cipher.ENCRYPT_MODE, keySpec, gcmSpec)
          aesGcmCipher.updateAAD(headerBytes) // RTP header as additional authenticated data
          aesGcmCipher.doFinal(opusPayload) // Returns payload + auth tag

        case "aead_xchacha20_poly1305_rtpsize" | _ =>
          // XChaCha20-Poly1305 encryption (software implementation via libsodium)
          // - Nonce: 4-byte counter (big-endian) + 20 zero bytes = 24 bytes
          // - Slower than AES256-GCM but more portable (no hardware dependency)
          val nonce = Nonce.forXChaCha20(nonceCounter)
          val output = new Array[Byte](opusPayload.length + AEAD_TAG_BYTES)

          // Reuse pre-initialized lazySodium instance (optimization: avoid recreating native bindings)
          val success = lazySodium.cryptoAeadXChaCha20Poly1305IetfEncrypt(
            output,
            null,
            opusPayload,
            opusPayload.length.toLong,
            headerBytes,
            headerBytes.length.toLong,
            null,
            nonce,
            secretKey
          )

          if (!success) {
            throw new RuntimeException(
              s"XChaCha20-Poly1305 encryption failed! Payload: ${opusPayload.length}B, Key: ${secretKey.length}B"
            )
          }
          output
      }

      // Return complete RTP packet (header + encrypted payload + nonce counter)
      RTPPacket(header, encryptedPayload, nonceCounter)
    }.handleErrorWith { error =>
      Logger[F].error(s"[AUDIO] ✗ Failed to encrypt packet: ${error.getMessage}") >>
      Logger[F].error(s"[AUDIO] Stack trace: ${error.getStackTrace.take(5).mkString("\n")}") >>
      Async[F].raiseError(error)
    }
  }
  
  private def sendUDPPacket(socket: DatagramSocket, data: Array[Byte], ip: String, port: Int): F[Unit] = {
    Async[F].blocking {
      val endpoint = new InetSocketAddress(ip, port)
      val packet = new java.net.DatagramPacket(data, data.length, endpoint)
      socket.send(packet)
    }.handleErrorWith { error =>
      Logger[F].error(s"[AUDIO] ✗ Failed to send UDP packet to $ip:$port: ${error.getMessage}")
    }
  }

  def streamAudioFromPosition(
    streamUrl: String,
    voiceWebSocket: WebSocket[F],
    udpSocket: DatagramSocket,
    ssrc: Int,
    voiceServerIp: String,
    voiceServerPort: Int,
    secretKey: Array[Byte],
    startPosition: Int,
    guildId: String,  // Guild ID for settings lookup
    encryptionMode: String = "aead_xchacha20_poly1305_rtpsize"
  ): F[Unit] = {
    Logger[F].debug(s"[AUDIO] Starting stream from position: ${startPosition}s") >>
    createAudioPipeline(streamUrl, startPosition = Some(startPosition))
      .through(processAudioFrames(udpSocket, ssrc, voiceServerIp, voiceServerPort, secretKey, encryptionMode, guildId))
      .compile
      .drain
      .handleErrorWith { error =>
        Logger[F].error(s"[AUDIO] ✗ Audio streaming failed: ${error.getMessage}")
      }
  }
}