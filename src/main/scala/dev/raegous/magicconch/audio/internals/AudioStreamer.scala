package dev.raegous.magicconch.audio.internals

import cats.effect.*
import cats.effect.syntax.all.*
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
 * - Encryption objects created lazily and reused
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

        } yield new AudioStreamer[F](
          encoder,
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
  pcmPool: BufferPool[F, Array[Short]],
  opusPool: BufferPool[F, Array[Byte]],
  guildSettings: GuildSettingsManager[F]  // Guild settings for volume, etc.
)(using Logger[F]) {

  // Audio format constants
  private val SAMPLE_RATE = 48000
  private val CHANNELS = 2
  private val OPUS_FRAME_SIZE = 960 // samples per frame at 48kHz
  private val OPUS_FRAME_DURATION = 20 // milliseconds
  private val OPUS_MAX_FRAME_SIZE = 5760 // 120ms at 48kHz
  private val BITRATE = 128000 // 128 kbps
  private val FfmpegReadRate = 1.05
  private val TargetQueueFrames = 8 // 160ms of encoded Opus audio
  private val MaxQueueFrames = 25 // 500ms hard cap
  private val PacedQueueFrames = 8
  private val PacketQueueFrames = 8
  private val MetricsLogFrameInterval = 250L

  private final case class AudioPacingMetrics(
    producedFrames: Long = 0L,
    sentFrames: Long = 0L,
    underflowFrames: Long = 0L,
    emptyOpusFrames: Long = 0L,
    highWaterDepth: Int = 0,
    pacedQueueHighWaterDepth: Int = 0,
    packetQueueHighWaterDepth: Int = 0,
    droppedPacedFrames: Long = 0L,
    droppedPacketFrames: Long = 0L,
    maxInterSendGapMs: Long = 0L,
    maxPipelineMs: Long = 0L,
    maxEncodeMs: Long = 0L,
    maxDaveMs: Long = 0L,
    maxRtpMs: Long = 0L,
    maxUdpMs: Long = 0L
  )

  private final case class PacedFrame(
    opusData: Array[Byte],
    underflow: Boolean,
    transportState: RtpTransportState,
    pacedAtNanos: Long
  )

  private final case class PacketizedFrame(
    bytes: Array[Byte],
    underflow: Boolean,
    emptyOpus: Boolean,
    pacedAtNanos: Long,
    daveMs: Long,
    rtpMs: Long
  )

  // Derived constants
  private val BYTES_PER_SAMPLE = 2 // 16-bit PCM
  private val PCM_FRAME_SIZE_BYTES = OPUS_FRAME_SIZE * CHANNELS * BYTES_PER_SAMPLE // 3840 bytes

  // Encryption constants
  private val AEAD_TAG_BYTES = 16 // Authentication tag for AEAD encryption
  private val GCM_TAG_BITS = 128 // GCM authentication tag size
  private lazy val lazySodium: com.goterl.lazysodium.LazySodiumJava =
    new com.goterl.lazysodium.LazySodiumJava(
      new com.goterl.lazysodium.SodiumJava()
    )

  def streamAudio(
    streamUrl: String,
    voiceWebSocket: WebSocket[F],
    udpSocket: DatagramSocket,
    ssrc: Int,
    voiceServerIp: String,
    voiceServerPort: Int,
    secretKey: Array[Byte],
    guildId: String,  // Guild ID for settings lookup
    encryptionMode: String = "aead_xchacha20_poly1305_rtpsize",
    daveAudioEncryptor: Option[Array[Byte] => F[Array[Byte]]] = None
  ): F[Unit] = {
    Logger[F].debug(s"[AUDIO] Starting audio stream") >>
    Logger[F].debug(s"[AUDIO] Endpoint: $voiceServerIp:$voiceServerPort") >>
    Logger[F].debug(s"[AUDIO] SSRC: $ssrc") >>
    Logger[F].debug(s"[AUDIO] Encryption: $encryptionMode") >>
    createAudioPipeline(streamUrl, startPosition = None)
      .through(processAudioFrames(udpSocket, ssrc, voiceServerIp, voiceServerPort, secretKey, encryptionMode, guildId, daveAudioEncryptor))
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
    createFfmpegAudioPipeline(streamUrl, startPosition)
  }

  private def createFfmpegAudioPipeline(streamUrl: String, startPosition: Option[Int]): Stream[F, Array[Byte]] = {
    // Build FFmpeg command with optional seek
    val baseCommand = List(
      "ffmpeg",
      "-hide_banner",
      "-loglevel", "warning"
    )

    val seekCommand = startPosition.map(pos => List("-ss", pos.toString)).getOrElse(Nil)
    val readRateCommand = List(
      "-readrate", FfmpegReadRate.toString
    )

    val remainingCommand = List(
      "-nostdin",
      "-reconnect", "1",
      "-reconnect_streamed", "1",
      "-reconnect_on_network_error", "1",
      "-reconnect_delay_max", "5",
      "-rw_timeout", "15000000",
      "-err_detect", "ignore_err",
      "-i", streamUrl,
      "-vn",
      "-map", "0:a:0",
      "-f", "s16le",           // 16-bit signed little-endian PCM
      "-ar", SAMPLE_RATE.toString,
      "-ac", CHANNELS.toString,
      "pipe:1"
    )

    val ffmpegCommand = baseCommand ++ seekCommand ++ readRateCommand ++ remainingCommand

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
          
          Option.when(numCompleteFrames > 0) {
            val framesToEmit = (0 until numCompleteFrames).map { i =>
              combined.drop(i * frameSize).take(frameSize)
            }.toList
            (remainder, Some(framesToEmit))
          }.getOrElse((combined, None))
        }
      }.flatMap {
        case Some(frames) => Stream.emits(frames)
        case None => Stream.empty
      }
    }
  }
  
  /**
    * Process PCM frames through the paced Opus queue, ordered packetizer, and isolated UDP sender.
    */
  private def processAudioFrames(
    udpSocket: DatagramSocket,
    ssrc: Int,
    voiceServerIp: String,
    voiceServerPort: Int,
    secretKey: Array[Byte],
    encryptionMode: String,
    guildId: String,
    daveAudioEncryptor: Option[Array[Byte] => F[Array[Byte]]]
  ): fs2.Pipe[F, Array[Byte], Unit] = { input =>
    Stream.eval {
      for {
        frameQueue <- Queue.bounded[F, Option[Array[Byte]]](MaxQueueFrames)
        pacedFrameQueue <- Queue.bounded[F, Option[PacedFrame]](PacedQueueFrames)
        packetQueue <- Queue.bounded[F, Option[PacketizedFrame]](PacketQueueFrames)
        depthRef <- Ref.of[F, Int](0)
        pacedDepthRef <- Ref.of[F, Int](0)
        packetDepthRef <- Ref.of[F, Int](0)
        sourceDoneRef <- Ref.of[F, Boolean](false)
        metricsRef <- Ref.of[F, AudioPacingMetrics](AudioPacingMetrics())
        lastSendNanosRef <- Ref.of[F, Option[Long]](None)
        rtpTransportStateRef <- Ref.of[F, RtpTransportState](RtpTransportState.initial)
        _ <- {
          val program = for {
            producer <- produceAudioFrames(input, frameQueue, depthRef, sourceDoneRef, metricsRef, guildId).start
            packetizer <- packetizerLoop(
              pacedFrameQueue,
              pacedDepthRef,
              packetQueue,
              packetDepthRef,
              metricsRef,
              ssrc,
              secretKey,
              encryptionMode,
              daveAudioEncryptor
            ).start
            sender <- udpSenderLoop(
              udpSocket,
              voiceServerIp,
              voiceServerPort,
              packetQueue,
              packetDepthRef,
              metricsRef,
              lastSendNanosRef
            ).start
            _ <- (
              awaitInitialBuffer(depthRef, sourceDoneRef) >>
                pacedSenderLoop(
                  frameQueue,
                  depthRef,
                  sourceDoneRef,
                  pacedFrameQueue,
                  pacedDepthRef,
                  metricsRef,
                  rtpTransportStateRef
                ) >>
                producer.joinWithNever >>
                packetizer.joinWithNever >>
                sender.joinWithNever
            ).guaranteeCase {
              case Outcome.Succeeded(_) => Async[F].unit
              case _ => producer.cancel >> packetizer.cancel >> sender.cancel
            }
          } yield ()

          program.handleErrorWith { error =>
            Logger[F].error(s"[AUDIO] Audio frame processing failed: ${error.getMessage}") >>
              Async[F].raiseError(error)
          }
        }
      } yield ()
    }
  }

  private def produceAudioFrames(
    input: Stream[F, Array[Byte]],
    frameQueue: Queue[F, Option[Array[Byte]]],
    depthRef: Ref[F, Int],
    sourceDoneRef: Ref[F, Boolean],
    metricsRef: Ref[F, AudioPacingMetrics],
    guildId: String
  ): F[Unit] =
    input
      .evalMap { frame =>
        encodeFrameForQueue(frame, metricsRef, guildId).flatMap { opusFrame =>
          frameQueue.offer(Some(opusFrame)) >>
            depthRef.updateAndGet(depth => (depth + 1).min(MaxQueueFrames)).flatMap { depth =>
              metricsRef.update(metrics => metrics.copy(
                producedFrames = metrics.producedFrames + 1L,
                highWaterDepth = metrics.highWaterDepth.max(depth)
              ))
            }
        }
      }
      .compile
      .drain
      .guarantee(sourceDoneRef.set(true) >> frameQueue.tryOffer(None).void)

  private def encodeFrameForQueue(
    pcmData: Array[Byte],
    metricsRef: Ref[F, AudioPacingMetrics],
    guildId: String
  ): F[Array[Byte]] = {
    val silenceFrame = DaveSessionManager.OpusSilenceFrame.clone()
    val invalidPcmLog = Option.when(pcmData.length != PCM_FRAME_SIZE_BYTES)(
      Logger[F].warn(s"[AUDIO] Invalid PCM frame size: ${pcmData.length} bytes (expected $PCM_FRAME_SIZE_BYTES); sending Opus silence")
    ).getOrElse(Async[F].unit)

    Option.when(pcmData.length == PCM_FRAME_SIZE_BYTES) {
      for {
        encodeStart <- Async[F].delay(System.nanoTime())
        encoded <- encodeToOpus(pcmData, guildId)
        encodeEnd <- Async[F].delay(System.nanoTime())
        encodeMs = nanosToMillis(encodeEnd - encodeStart)
        _ <- metricsRef.update(metrics => metrics.copy(maxEncodeMs = metrics.maxEncodeMs.max(encodeMs)))
      } yield Option.when(encoded.nonEmpty)(encoded).getOrElse(silenceFrame)
    }.getOrElse(invalidPcmLog >> Async[F].pure(silenceFrame))
  }

  private def awaitInitialBuffer(depthRef: Ref[F, Int], sourceDoneRef: Ref[F, Boolean]): F[Unit] =
    (depthRef.get, sourceDoneRef.get).flatMapN { (depth, sourceDone) =>
      Either.cond(depth >= TargetQueueFrames || sourceDone, (), ()).fold(
        _ => Async[F].sleep(10.millis) >> awaitInitialBuffer(depthRef, sourceDoneRef),
        _ => Async[F].unit
      )
    }

  private def pacedSenderLoop(
    frameQueue: Queue[F, Option[Array[Byte]]],
    depthRef: Ref[F, Int],
    sourceDoneRef: Ref[F, Boolean],
    pacedFrameQueue: Queue[F, Option[PacedFrame]],
    pacedDepthRef: Ref[F, Int],
    metricsRef: Ref[F, AudioPacingMetrics],
    rtpTransportStateRef: Ref[F, RtpTransportState]
  ): F[Unit] =
    Clock[F].monotonic.flatMap(now => pacedSenderTick(
      nextDeadline = now,
      frameQueue,
      depthRef,
      sourceDoneRef,
      pacedFrameQueue,
      pacedDepthRef,
      metricsRef,
      rtpTransportStateRef
    ))

  private def pacedSenderTick(
    nextDeadline: FiniteDuration,
    frameQueue: Queue[F, Option[Array[Byte]]],
    depthRef: Ref[F, Int],
    sourceDoneRef: Ref[F, Boolean],
    pacedFrameQueue: Queue[F, Option[PacedFrame]],
    pacedDepthRef: Ref[F, Int],
    metricsRef: Ref[F, AudioPacingMetrics],
    rtpTransportStateRef: Ref[F, RtpTransportState]
  ): F[Unit] =
    Clock[F].monotonic.flatMap { now =>
      val wait = nextDeadline - now
      Option.when(wait > Duration.Zero)(Async[F].sleep(wait)).getOrElse(Async[F].unit) >>
        paceNextFrame(
          frameQueue,
          depthRef,
          sourceDoneRef,
          pacedFrameQueue,
          pacedDepthRef,
          metricsRef,
          rtpTransportStateRef
        ).flatMap { done =>
          Option.when(!done)(
            pacedSenderTick(
              nextDeadline + OPUS_FRAME_DURATION.millis,
              frameQueue,
              depthRef,
              sourceDoneRef,
              pacedFrameQueue,
              pacedDepthRef,
              metricsRef,
              rtpTransportStateRef
            )
          ).getOrElse(Async[F].unit)
        }
    }

  private def paceNextFrame(
    frameQueue: Queue[F, Option[Array[Byte]]],
    depthRef: Ref[F, Int],
    sourceDoneRef: Ref[F, Boolean],
    pacedFrameQueue: Queue[F, Option[PacedFrame]],
    pacedDepthRef: Ref[F, Int],
    metricsRef: Ref[F, AudioPacingMetrics],
    rtpTransportStateRef: Ref[F, RtpTransportState]
  ): F[Boolean] =
    frameQueue.tryTake.flatMap {
      case Some(Some(opusData)) =>
        depthRef.update(depth => (depth - 1).max(0)) >>
          enqueuePacedFrame(
            opusData,
            underflow = false,
            pacedFrameQueue,
            pacedDepthRef,
            metricsRef,
            rtpTransportStateRef
          ).as(false)

      case Some(None) =>
        Logger[F].debug("[AUDIO] Audio source completed and jitter buffer drained") >>
          enqueueTerminal(
            pacedFrameQueue,
            pacedDepthRef,
            PacedQueueFrames,
            metricsRef,
            metrics => metrics.copy(droppedPacedFrames = metrics.droppedPacedFrames + 1L),
            (metrics, depth) => metrics.copy(pacedQueueHighWaterDepth = metrics.pacedQueueHighWaterDepth.max(depth))
          ) >>
          Async[F].pure(true)

      case None =>
        sourceDoneRef.get.flatMap { sourceDone =>
          Option.when(!sourceDone)(
            enqueuePacedFrame(
              DaveSessionManager.OpusSilenceFrame.clone(),
              underflow = true,
              pacedFrameQueue,
              pacedDepthRef,
              metricsRef,
              rtpTransportStateRef
            ).as(false)
          ).getOrElse(
            enqueueTerminal(
              pacedFrameQueue,
              pacedDepthRef,
              PacedQueueFrames,
              metricsRef,
              metrics => metrics.copy(droppedPacedFrames = metrics.droppedPacedFrames + 1L),
              (metrics, depth) => metrics.copy(pacedQueueHighWaterDepth = metrics.pacedQueueHighWaterDepth.max(depth))
            ) >>
              Async[F].pure(true)
          )
        }
    }

  private def enqueuePacedFrame(
    opusData: Array[Byte],
    underflow: Boolean,
    pacedFrameQueue: Queue[F, Option[PacedFrame]],
    pacedDepthRef: Ref[F, Int],
    metricsRef: Ref[F, AudioPacingMetrics],
    rtpTransportStateRef: Ref[F, RtpTransportState]
  ): F[Unit] =
    for {
      pacedAtNanos <- Async[F].delay(System.nanoTime())
      transportState <- rtpTransportStateRef.modify(state => (state.next(OPUS_FRAME_SIZE), state))
      pacedFrame = PacedFrame(opusData, underflow, transportState, pacedAtNanos)
      _ <- offerWithDropOldest(
        pacedFrameQueue,
        Some(pacedFrame),
        pacedDepthRef,
        PacedQueueFrames,
        metricsRef,
        (metrics, depth) => metrics.copy(pacedQueueHighWaterDepth = metrics.pacedQueueHighWaterDepth.max(depth)),
        metrics => metrics.copy(droppedPacedFrames = metrics.droppedPacedFrames + 1L)
      )
    } yield ()

  private def packetizerLoop(
    pacedFrameQueue: Queue[F, Option[PacedFrame]],
    pacedDepthRef: Ref[F, Int],
    packetQueue: Queue[F, Option[PacketizedFrame]],
    packetDepthRef: Ref[F, Int],
    metricsRef: Ref[F, AudioPacingMetrics],
    ssrc: Int,
    secretKey: Array[Byte],
    encryptionMode: String,
    daveAudioEncryptor: Option[Array[Byte] => F[Array[Byte]]]
  ): F[Unit] =
    pacedFrameQueue.take.flatMap {
      case Some(pacedFrame) =>
        pacedDepthRef.update(depth => (depth - 1).max(0)) >>
          packetizeFrame(
            pacedFrame,
            packetQueue,
            packetDepthRef,
            metricsRef,
            ssrc,
            secretKey,
            encryptionMode,
            daveAudioEncryptor
          ) >>
          packetizerLoop(
            pacedFrameQueue,
            pacedDepthRef,
            packetQueue,
            packetDepthRef,
            metricsRef,
            ssrc,
            secretKey,
            encryptionMode,
            daveAudioEncryptor
          )

      case None =>
        pacedDepthRef.update(depth => (depth - 1).max(0)) >>
          enqueueTerminal(
            packetQueue,
            packetDepthRef,
            PacketQueueFrames,
            metricsRef,
            metrics => metrics.copy(droppedPacketFrames = metrics.droppedPacketFrames + 1L),
            (metrics, depth) => metrics.copy(packetQueueHighWaterDepth = metrics.packetQueueHighWaterDepth.max(depth))
          )
    }

  private def packetizeFrame(
    pacedFrame: PacedFrame,
    packetQueue: Queue[F, Option[PacketizedFrame]],
    packetDepthRef: Ref[F, Int],
    metricsRef: Ref[F, AudioPacingMetrics],
    ssrc: Int,
    secretKey: Array[Byte],
    encryptionMode: String,
    daveAudioEncryptor: Option[Array[Byte] => F[Array[Byte]]]
  ): F[Unit] = {
    val header = RTPHeader(
      sequence = pacedFrame.transportState.sequence,
      timestamp = pacedFrame.transportState.timestamp,
      ssrc = ssrc & 0xFFFFFFFFL
    )

    for {
      daveStart <- Async[F].delay(System.nanoTime())
      mediaPayload <- daveAudioEncryptor.fold(Async[F].pure(pacedFrame.opusData))(_(pacedFrame.opusData))
      daveEnd <- Async[F].delay(System.nanoTime())
      rtpStart <- Async[F].delay(System.nanoTime())
      rtpPacket <- encryptPacket(
        header,
        mediaPayload,
        secretKey,
        pacedFrame.transportState.nonceCounter,
        encryptionMode
      )
      rtpEnd <- Async[F].delay(System.nanoTime())
      packetizedFrame = PacketizedFrame(
        bytes = rtpPacket.toBytes,
        underflow = pacedFrame.underflow,
        emptyOpus = pacedFrame.opusData.isEmpty,
        pacedAtNanos = pacedFrame.pacedAtNanos,
        daveMs = nanosToMillis(daveEnd - daveStart),
        rtpMs = nanosToMillis(rtpEnd - rtpStart)
      )
      _ <- offerWithDropOldest(
        packetQueue,
        Some(packetizedFrame),
        packetDepthRef,
        PacketQueueFrames,
        metricsRef,
        (metrics, depth) => metrics.copy(packetQueueHighWaterDepth = metrics.packetQueueHighWaterDepth.max(depth)),
        metrics => metrics.copy(droppedPacketFrames = metrics.droppedPacketFrames + 1L)
      )
    } yield ()
  }

  private def udpSenderLoop(
    udpSocket: DatagramSocket,
    voiceServerIp: String,
    voiceServerPort: Int,
    packetQueue: Queue[F, Option[PacketizedFrame]],
    packetDepthRef: Ref[F, Int],
    metricsRef: Ref[F, AudioPacingMetrics],
    lastSendNanosRef: Ref[F, Option[Long]]
  ): F[Unit] =
    packetQueue.take.flatMap {
      case Some(packetizedFrame) =>
        packetDepthRef.update(depth => (depth - 1).max(0)) >>
          sendPacketizedFrame(
            udpSocket,
            voiceServerIp,
            voiceServerPort,
            packetizedFrame,
            metricsRef,
            lastSendNanosRef
          ) >>
          udpSenderLoop(
            udpSocket,
            voiceServerIp,
            voiceServerPort,
            packetQueue,
            packetDepthRef,
            metricsRef,
            lastSendNanosRef
          )

      case None =>
        packetDepthRef.update(depth => (depth - 1).max(0))
    }

  private def sendPacketizedFrame(
    udpSocket: DatagramSocket,
    voiceServerIp: String,
    voiceServerPort: Int,
    packetizedFrame: PacketizedFrame,
    metricsRef: Ref[F, AudioPacingMetrics],
    lastSendNanosRef: Ref[F, Option[Long]]
  ): F[Unit] =
    sendUDPPacket(udpSocket, packetizedFrame.bytes, voiceServerIp, voiceServerPort).flatMap { udpTiming =>
      recordFrameMetrics(
        metricsRef,
        lastSendNanosRef,
        underflow = packetizedFrame.underflow,
        emptyOpus = packetizedFrame.emptyOpus,
        pipelineMs = nanosToMillis(udpTiming.completedAtNanos - packetizedFrame.pacedAtNanos),
        encodeMs = 0L,
        daveMs = packetizedFrame.daveMs,
        rtpMs = packetizedFrame.rtpMs,
        udpMs = nanosToMillis(udpTiming.innerDurationNanos),
        sendNanos = udpTiming.completedAtNanos
      )
    }

  private def offerWithDropOldest[A](
    queue: Queue[F, Option[A]],
    item: Option[A],
    depthRef: Ref[F, Int],
    capacity: Int,
    metricsRef: Ref[F, AudioPacingMetrics],
    onDepth: (AudioPacingMetrics, Int) => AudioPacingMetrics,
    onDrop: AudioPacingMetrics => AudioPacingMetrics
  ): F[Unit] =
    queue.tryOffer(item).flatMap {
      case true =>
        depthRef.updateAndGet(depth => (depth + 1).min(capacity)).flatMap { depth =>
          metricsRef.update(metrics => onDepth(metrics, depth))
        }

      case false =>
        queue.tryTake.flatMap {
          case Some(_) =>
            depthRef.update(depth => (depth - 1).max(0)) >>
              metricsRef.update(onDrop) >>
              offerWithDropOldest(queue, item, depthRef, capacity, metricsRef, onDepth, onDrop)

          case None =>
            Async[F].cede >> offerWithDropOldest(queue, item, depthRef, capacity, metricsRef, onDepth, onDrop)
        }
    }

  private def enqueueTerminal[A](
    queue: Queue[F, Option[A]],
    depthRef: Ref[F, Int],
    capacity: Int,
    metricsRef: Ref[F, AudioPacingMetrics],
    onDrop: AudioPacingMetrics => AudioPacingMetrics,
    onDepth: (AudioPacingMetrics, Int) => AudioPacingMetrics
  ): F[Unit] =
    offerWithDropOldest(queue, None, depthRef, capacity, metricsRef, onDepth, onDrop)

  private def recordFrameMetrics(
    metricsRef: Ref[F, AudioPacingMetrics],
    lastSendNanosRef: Ref[F, Option[Long]],
    underflow: Boolean,
    emptyOpus: Boolean,
    pipelineMs: Long,
    encodeMs: Long,
    daveMs: Long,
    rtpMs: Long,
    udpMs: Long,
    sendNanos: Long
  ): F[Unit] =
    lastSendNanosRef.modify(last => (Some(sendNanos), last.map(previous => nanosToMillis(sendNanos - previous)).getOrElse(OPUS_FRAME_DURATION.toLong))).flatMap { interSendMs =>
      metricsRef.modify { metrics =>
        val next = metrics.copy(
          sentFrames = metrics.sentFrames + 1L,
          underflowFrames = metrics.underflowFrames + Option.when(underflow)(1L).getOrElse(0L),
          emptyOpusFrames = metrics.emptyOpusFrames + Option.when(emptyOpus)(1L).getOrElse(0L),
          maxInterSendGapMs = metrics.maxInterSendGapMs.max(interSendMs),
          maxPipelineMs = metrics.maxPipelineMs.max(pipelineMs),
          maxEncodeMs = metrics.maxEncodeMs.max(encodeMs),
          maxDaveMs = metrics.maxDaveMs.max(daveMs),
          maxRtpMs = metrics.maxRtpMs.max(rtpMs),
          maxUdpMs = metrics.maxUdpMs.max(udpMs)
        )
        val shouldLog = next.sentFrames % MetricsLogFrameInterval == 0L || underflow || interSendMs > 30L || pipelineMs > 18L || udpMs > 10L
        (next, Option.when(shouldLog)(next))
      }.flatMap {
        case Some(snapshot) =>
          Logger[F].info(
            s"[AUDIO] Pacing metrics sent=${snapshot.sentFrames}, produced=${snapshot.producedFrames}, underflows=${snapshot.underflowFrames}, emptyOpus=${snapshot.emptyOpusFrames}, highWater=${snapshot.highWaterDepth}f, pacedHighWater=${snapshot.pacedQueueHighWaterDepth}f, packetHighWater=${snapshot.packetQueueHighWaterDepth}f, droppedPaced=${snapshot.droppedPacedFrames}, droppedPacket=${snapshot.droppedPacketFrames}, maxSendGap=${snapshot.maxInterSendGapMs}ms, maxPipeline=${snapshot.maxPipelineMs}ms, maxEncode=${snapshot.maxEncodeMs}ms, maxDave=${snapshot.maxDaveMs}ms, maxRtp=${snapshot.maxRtpMs}ms, maxUdp=${snapshot.maxUdpMs}ms"
          )
        case None => Async[F].unit
      }
    }

  private def nanosToMillis(nanos: Long): Long =
    (nanos / 1000000L).max(0L)

  private def encodeToOpus(pcmData: Array[Byte], guildId: String): F[Array[Byte]] = {
    // Read current volume for this guild (checked every frame for mid-song volume changes)
    guildSettings.getVolume(guildId).flatMap { volume =>
      // Use buffer pools to avoid allocations (optimization: 95% less GC)
      pcmPool.use { pcmShorts =>
        opusPool.use { opusOutput =>
          Async[F].delay {
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

            Option.when(encodedLength > 0)(opusOutput.take(encodedLength)).getOrElse(Array.empty[Byte])
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
    nonceCounter: Long,
    encryptionMode: String
  ): F[RTPPacket] = {
    Async[F].delay {
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

          val aesGcmCipher = Cipher.getInstance("AES/GCM/NoPadding")
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
  
  private final case class UdpSendTiming(completedAtNanos: Long, innerDurationNanos: Long)

  private def sendUDPPacket(socket: DatagramSocket, data: Array[Byte], ip: String, port: Int): F[UdpSendTiming] = {
    Async[F].blocking {
      val endpoint = new InetSocketAddress(ip, port)
      val packet = new java.net.DatagramPacket(data, data.length, endpoint)
      val sendStart = System.nanoTime()
      socket.send(packet)
      val sendEnd = System.nanoTime()
      UdpSendTiming(completedAtNanos = sendEnd, innerDurationNanos = sendEnd - sendStart)
    }.handleErrorWith { error =>
      Logger[F].error(s"[AUDIO] Failed to send UDP packet to $ip:$port: ${error.getMessage}") >>
        Async[F].raiseError(error)
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
    encryptionMode: String = "aead_xchacha20_poly1305_rtpsize",
    daveAudioEncryptor: Option[Array[Byte] => F[Array[Byte]]] = None
  ): F[Unit] = {
    Logger[F].debug(s"[AUDIO] Starting stream from position: ${startPosition}s") >>
    createAudioPipeline(streamUrl, startPosition = Some(startPosition))
      .through(processAudioFrames(udpSocket, ssrc, voiceServerIp, voiceServerPort, secretKey, encryptionMode, guildId, daveAudioEncryptor))
      .compile
      .drain
      .handleErrorWith { error =>
        Logger[F].error(s"[AUDIO] ✗ Audio streaming failed: ${error.getMessage}")
      }
  }
}
