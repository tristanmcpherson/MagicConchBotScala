package dev.raegous.magicconch

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
import DiscordModels.*

import scala.concurrent.duration.DurationInt

/**
 * AudioStreamer handles streaming audio to Discord voice channels.
 *
 * Architecture:
 * 1. Downloads audio from YouTube URL using yt-dlp
 * 2. Pipes through FFmpeg to convert to PCM (48kHz, stereo)
 * 3. Chunks PCM into frames (960 samples = 20ms at 48kHz)
 * 4. Encodes each frame to Opus (TODO: NOT YET IMPLEMENTED)
 * 5. Wraps in RTP packets and sends over UDP
 *
 * Current Limitations:
 * - Opus encoding is NOT implemented (see encodeToOpus method)
 * - This means audio streaming will NOT work with Discord
 * - UDP endpoint discovery is incomplete
 * - No error recovery or reconnection logic
 *
 * Dependencies Required:
 * - yt-dlp: For downloading YouTube audio
 * - ffmpeg: For audio format conversion
 * - libopus: For Opus encoding (when implemented)
 */
class AudioStreamer[F[_]: Async: Processes](using Logger[F]) {

  private val OPUS_FRAME_SIZE = 960 // samples per frame at 48kHz
  private val OPUS_FRAME_DURATION = 20 // milliseconds
  
  def streamAudio(
    streamUrl: String, 
    voiceWebSocket: WebSocket[F],
    udpSocket: DatagramSocket,
    ssrc: Int
  ): F[Unit] = {
    Logger[F].info(s"Starting audio stream from: $streamUrl") >>
    createAudioPipeline(streamUrl)
      .through(processAudioFrames(voiceWebSocket, udpSocket, ssrc))
      .compile
      .drain
  }
  
  private def createAudioPipeline(streamUrl: String): Stream[F, Array[Byte]] = {
    val ffmpegCommand = List(
      "ffmpeg",
      "-i", streamUrl,
      "-f", "s16le",           // 16-bit signed little-endian PCM
      "-ar", "48000",          // 48kHz sample rate (Discord requirement)
      "-ac", "2",              // Stereo
      "-loglevel", "error",    // Suppress verbose output
      "-"                      // Output to stdout
    )
    
    Stream.resource(ProcessBuilder(ffmpegCommand.head, ffmpegCommand.tail*).spawn[F])
      .flatMap { process =>
        process.stdout
          .chunkN(OPUS_FRAME_SIZE * 4) // 2 channels * 2 bytes per sample * 960 samples
          .map(_.toArray)
      }
  }
  
  private def processAudioFrames(
    voiceWebSocket: WebSocket[F],
    udpSocket: DatagramSocket,
    ssrc: Int
  ): fs2.Pipe[F, Array[Byte], Unit] = { audioStream =>
    Stream.eval(Ref[F].of((0.toShort, 0))).flatMap { stateRef =>
      audioStream.evalMap { pcmData =>
        stateRef.modify { case (sequence, timestamp) =>
          val newSequence = (sequence + 1).toShort
          val newTimestamp = timestamp + OPUS_FRAME_SIZE
          ((newSequence, newTimestamp), (sequence, timestamp))
        }.flatMap { case (sequence, timestamp) =>
          for {
            opusData <- encodeToOpus(pcmData)
            rtpPacket = createRTPPacket(opusData, sequence, timestamp, ssrc)
            _ <- sendUDPPacket(udpSocket, rtpPacket)
            _ <- Async[F].sleep(OPUS_FRAME_DURATION.millis)
          } yield ()
        }
      }
    }
  }
  
  private def encodeToOpus(pcmData: Array[Byte]): F[Array[Byte]] = {
    // TODO: CRITICAL - Opus encoding is required for Discord voice to work!
    //
    // Current Status: This method returns raw PCM data, which Discord will NOT accept.
    // Discord requires audio to be encoded in Opus format before sending over RTP.
    //
    // To implement proper Opus encoding, you have several options:
    //
    // Option 1: Use a Java/Scala Opus library
    //   - tomp2p-opus: https://github.com/tomp2p/tomp2p-opus
    //   - concentus (Java port): https://github.com/lostromb/concentus
    //
    // Option 2: Use JNI (Java Native Interface) to call libopus directly
    //   - More complex but gives you direct access to libopus
    //   - Example: https://github.com/nwaldispuehl/java-opus-codec
    //
    // Option 3: Use FFmpeg to encode to Opus and pipe the output
    //   - Change the FFmpeg command to output Opus directly
    //   - Command: ffmpeg -i <url> -f opus -ar 48000 -ac 2 -
    //
    // Opus Requirements for Discord:
    //   - Sample rate: 48000 Hz (48 kHz)
    //   - Channels: 2 (stereo)
    //   - Frame size: 960 samples (20ms at 48kHz)
    //   - Bitrate: 64-128 kbps recommended
    //
    Logger[F].warn("⚠️  Opus encoding not implemented! Audio will not work with Discord.") >>
    Async[F].pure(pcmData)
  }
  
  private def createRTPPacket(
    opusData: Array[Byte], 
    sequence: Short, 
    timestamp: Int, 
    ssrc: Int
  ): Array[Byte] = {
    val header = ByteBuffer.allocate(12)
    header.put(0x80.toByte)           // Version (2), Padding (0), Extension (0), CC (0)
    header.put(0x78.toByte)           // Marker (0), Payload Type (120 for Opus)
    header.putShort(sequence)         // Sequence number
    header.putInt(timestamp)          // Timestamp
    header.putInt(ssrc)               // SSRC
    
    val packet = new Array[Byte](12 + opusData.length)
    System.arraycopy(header.array(), 0, packet, 0, 12)
    System.arraycopy(opusData, 0, packet, 12, opusData.length)
    packet
  }
  
  private def sendUDPPacket(socket: DatagramSocket, data: Array[Byte]): F[Unit] = {
    Async[F].blocking {
      // You'd need the actual Discord voice server endpoint here
      val endpoint = new InetSocketAddress("discord-voice-server", 50000)
      val packet = new java.net.DatagramPacket(data, data.length, endpoint)
      socket.send(packet)
    }.handleErrorWith { error =>
      Logger[F].error(s"Failed to send UDP packet: $error")
    }
  }
  
  def createVoiceConnection(
    guildId: String,
    voiceToken: String,
    sessionId: String,
    endpoint: String
  ): F[WebSocket[F]] = {
    // This would establish a WebSocket connection to Discord's voice gateway
    // and handle the voice protocol handshake
    Logger[F].info(s"Creating voice connection for guild $guildId to endpoint $endpoint") >>
    Async[F].raiseError(new NotImplementedError("Voice WebSocket connection not implemented"))
  }
}