package dev.raegous.magicconch.audio.internals

import cats.effect.*
import cats.effect.std.{Queue, Supervisor}
import cats.implicits.*
import org.typelevel.log4cats.Logger
import sttp.client4.*
import sttp.ws.WebSocket
import io.circe.parser.*
import io.circe.syntax.*
import dev.raegous.magicconch.discord.DiscordModels.*
import cats.Applicative
import cats.effect.implicits.*
import dev.raegous.magicconch.discord.EncryptionMode
import sttp.client4.ws.async.asWebSocket
import fs2.Stream

import java.net.DatagramSocket
import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.time.Instant
import java.time.temporal.ChronoUnit
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*

object VoiceGateway {
  def make[F[_]: Logger: Async: fs2.io.process.Processes](
    backend: WebSocketStreamBackend[F, ?],
    audioStreamer: AudioStreamer[F]): Resource[F, VoiceGateway[F]] = {
    val allocate: F[VoiceGateway[F]] =
      for {
        udpSocketRef              <- Ref.of[F, Option[DatagramSocket]](None)
        ssrcRef                   <- Ref.of[F, Option[Int]](None)
        voiceReadyRef             <- Ref.of[F, Boolean](false)
        voiceServerIpRef          <- Ref.of[F, Option[String]](None)
        voiceServerPortRef        <- Ref.of[F, Option[Int]](None)
        encryptionModesRef        <- Ref.of[F, List[String]](List.empty)
        selectedEncryptionModeRef <- Ref.of[F, Option[EncryptionMode]](None)
        secretKeyRef              <- Ref.of[F, Option[Array[Byte]]](None)
        seqNum                    <- Ref.of[F, Int](0)
        sendQueue                 <- Queue.unbounded[F, String]
        sendQueueRef              <- Ref.of[F, Option[Queue[F, String]]](Some(sendQueue))
        shutdownSignal            <- Deferred[F, Unit]
        shutdownSignalRef         <- Ref.of[F, Option[Deferred[F, Unit]]](Some(shutdownSignal))
        webSocketRef              <- Ref.of[F, Option[WebSocket[F]]](None)
        volumeRef                 <- Ref.of[F, Option[Double]](None)
        isConnectedRef            <- Ref.of[F, Boolean](false)
      } yield new VoiceGateway[F](
        backend = backend,
        audioStreamer = audioStreamer,
        udpSocketRef = udpSocketRef,
        ssrcRef = ssrcRef,
        voiceReadyRef = voiceReadyRef,
        voiceServerIpRef = voiceServerIpRef,
        voiceServerPortRef = voiceServerPortRef,
        encryptionModesRef = encryptionModesRef,
        selectedEncryptionModeRef = selectedEncryptionModeRef,
        secretKeyRef = secretKeyRef,
        seqNum = seqNum,
        sendQueueRef = sendQueueRef,
        shutdownSignalRef = shutdownSignalRef,
        webSocketRef = webSocketRef,
        volumeRef = volumeRef,
        isConnectedRef = isConnectedRef
      )

    // Wrap in Resource to ensure proper cleanup
    Resource.make(allocate)(_.closeVoiceConnection().guaranteeCase(_ => Applicative[F].unit))

  }
}

/**
 * VoiceGateway handles the Discord voice WebSocket connection and protocol.
 *
 * Discord Voice Connection Flow:
 * 1. Receive VOICE_STATE_UPDATE and VOICE_SERVER_UPDATE from main gateway
 * 2. Connect to voice gateway WebSocket (wss://...)
 * 3. Send IDENTIFY payload with session_id and token
 * 4. Receive READY with SSRC, IP, and port
 * 5. Perform UDP IP discovery to find external IP/port
 * 6. Send SELECT_PROTOCOL with encryption mode
 * 7. Receive SESSION_DESCRIPTION - now ready to stream audio
 * 8. Send SPEAKING payload before streaming audio
 * 9. Stream RTP packets over UDP with Opus-encoded audio
 * 10. Send SPEAKING(0) when done
 *
 * Current Status:
 * - WebSocket connection: ✓ Implemented
 * - Voice protocol handshake: ✓ Implemented
 * - Opus encoding: ✓ Implemented (using Concentus)
 * - UDP IP discovery: ⚠️  Partially implemented
 * - RTP packet creation: ✓ Implemented
 *
 * See: https://discord.com/developers/docs/topics/voice-connections
 */
class VoiceGateway[F[_]: Async: fs2.io.process.Processes] private (
  backend: WebSocketStreamBackend[F, ?],
  audioStreamer: AudioStreamer[F],
  private val udpSocketRef: Ref[F, Option[DatagramSocket]],
  private val ssrcRef: Ref[F, Option[Int]],
  private val voiceReadyRef: Ref[F, Boolean],
  private val voiceServerIpRef: Ref[F, Option[String]],
  private val voiceServerPortRef: Ref[F, Option[Int]],
  private val encryptionModesRef: Ref[F, List[String]],
  private val selectedEncryptionModeRef: Ref[F, Option[EncryptionMode]],
  private val secretKeyRef: Ref[F, Option[Array[Byte]]],

  private val seqNum: Ref[F, Int],

  // Queue for serializing WebSocket sends (prevents concurrent access)
  private val sendQueueRef: Ref[F, Option[Queue[F, String]]],

  // Shutdown signal - complete this to close the voice connection
  private val shutdownSignalRef: Ref[F, Option[Deferred[F, Unit]]],

  // Store the WebSocket so we can access it outside the handler
  private val webSocketRef: Ref[F, Option[WebSocket[F]]],

  private val volumeRef: Ref[F, Option[Double]],

  // Track whether the voice WebSocket is connected
  private val isConnectedRef: Ref[F, Boolean]
)(using Logger[F]) {

  def connectToVoiceGateway(
    endpoint: String,
    guildId: String,
    userId: String,
    sessionId: String,
    token: String
  ): F[WebSocket[F]] = {
    val voiceGatewayUrl = s"wss://${endpoint.replace(":80", "")}/?v=8"
    val request = basicRequest.get(uri"$voiceGatewayUrl")

    for {
      // Reset ready state for new connection
      _ <- voiceReadyRef.set(false)
      _ <- isConnectedRef.set(false)

      // Create shutdown signal
      shutdownSignal <- Deferred[F, Unit]
      _ <- shutdownSignalRef.set(Some(shutdownSignal))

      // Create a Deferred that will be completed when the WebSocket is ready
      wsReady <- Deferred[F, WebSocket[F]]

      // Start the WebSocket connection in a background fiber
      // This fiber will keep running, keeping the connection alive
      _ <- Async[F].start {
        backend.send(
          request.response(asWebSocket { (ws: WebSocket[F]) =>
            for {
              _ <- Logger[F].info(s"[VOICE] Connected to voice gateway: $voiceGatewayUrl")
              _ <- webSocketRef.set(Some(ws))
              _ <- isConnectedRef.set(true)  // Mark as connected

              _ <- Async[F].start(handleVoiceEvents(ws))
              _ <- sendVoiceIdentify(ws, guildId, userId, sessionId, token)

              _ <- Logger[F].info("[VOICE] Waiting for voice connection to be ready...")
              _ <- waitForVoiceReady.timeoutTo(
                100.seconds,
                Logger[F].error("[VOICE] ✗ Voice connection timeout - took more than 10 seconds") >>
                Async[F].raiseError(new RuntimeException("Voice connection timeout"))
              )
              _ <- Logger[F].info("[VOICE] ✓ Voice connection is ready!")

              // Signal that the WebSocket is ready to use
              _ <- wsReady.complete(ws)
              _ <- Logger[F].info("[VOICE] WebSocket ready signal sent")

              // Now BLOCK here to keep the handler alive
              // This keeps the WebSocket connection open
              _ <- Logger[F].info("[VOICE] Handler blocking on shutdown signal (keeps connection alive)...")
              _ <- shutdownSignal.get

              _ <- Logger[F].info("[VOICE] Shutdown signal received, handler will exit and close connection")
              _ <- isConnectedRef.set(false)  // Mark as disconnected
            } yield ()
          })
        ).flatMap { response =>
          response.body match {
            case Right(_) => Logger[F].info("[VOICE] WebSocket handler completed")
            case Left(error) => Logger[F].error(s"[VOICE] WebSocket error: $error")
          }
        }
      }

      // Wait for the WebSocket to be ready
      ws <- wsReady.get.timeoutTo(
        15.seconds,
        Async[F].raiseError(new RuntimeException("Timeout waiting for voice WebSocket to be ready"))
      )

      _ <- Logger[F].info(s"[VOICE] Voice connection established and staying alive")
    } yield ws
  }

  private def startVoiceHeartbeat(heartbeatIntervalMs: Int, ws: WebSocket[F]): F[Unit] = {
    Logger[F].info(s"[VOICE] Starting voice heartbeat (interval: ${heartbeatIntervalMs}ms)") >>
    Async[F].start {
      fs2.Stream.fixedRateStartImmediately[F](heartbeatIntervalMs.millis)
        .evalMap { _ =>
          // Check if still connected before attempting to send heartbeat
          isConnectedRef.get.flatMap { isConnected =>
            if (isConnected) {
              sendHeartbeat(ws).handleErrorWith { error =>
                val errorMsg = Option(error.getMessage).getOrElse("unknown error")
                if (errorMsg.contains("closed") || errorMsg.contains("Closed") || errorMsg.contains("Output closed")) {
                  Logger[F].warn(s"[VOICE] Heartbeat stopped - WebSocket closed") >>
                  isConnectedRef.set(false) >>  // Mark as disconnected
                  Async[F].raiseError(error)  // Stop the stream by raising the error
                } else {
                  Logger[F].warn(s"[VOICE] Failed to send voice heartbeat: $errorMsg")
                }
              }
            } else {
              // Connection closed, stop the heartbeat loop gracefully
              Logger[F].debug(s"[VOICE] Heartbeat loop exiting - connection closed") >>
              Async[F].raiseError(new Exception("Voice connection closed"))
            }
          }
        }
        .compile
        .drain
        .handleErrorWith { error =>
          Logger[F].debug(s"[VOICE] Voice heartbeat loop stopped")
        }
    }.void
  }

  private def waitForVoiceReady: F[Unit] = {
    voiceReadyRef.get.flatMap { ready =>
      if (ready) Async[F].unit
      else Async[F].sleep(100.millis) >> waitForVoiceReady
    }
  }
  
  private def sendVoiceIdentify(
    ws: WebSocket[F],
    guildId: String,
    userId: String,
    sessionId: String,
    token: String
  ): F[Unit] = {
    val identify = Map(
      "op" -> 0.asJson,
      "d" -> Map(
        "server_id" -> guildId.asJson,
        "user_id" -> userId.asJson,
        "session_id" -> sessionId.asJson,
        "token" -> token.asJson
      ).asJson
    ).asJson
    
    ws.sendText(identify.noSpaces) >>
    Logger[F].info(f"Sent: ${identify.noSpaces}")
  }
  
  private def handleVoiceEvents(ws: WebSocket[F]): F[Unit] = {
    def processVoiceEvent(message: String): F[Unit] = {
      Async[F].start {
        for {
          _ <- Logger[F].debug(s"[VOICE] Received: $message")
          _ <- parse(message) match {
            case Right(json) =>
              val op = json.hcursor.get[Int]("op").getOrElse(-1)
              val seq = json.hcursor.get[Int]("seq").getOrElse(-1)
              seqNum.update(_ => seq) >>
                (op match {
                case 2 => handleReady(json, ws)
                // Send immediate heartbeat
                case 3 => sendHeartbeat(ws)
                case 4 => handleSessionDescription(json)
                case 6 => Logger[F].debug("[VOICE] Heartbeat ack'd")
                case 8 => startVoiceHeartbeat(json.hcursor.downField("d").get[Int]("heartbeat_interval").getOrElse(-1), ws)
                case _ => Logger[F].debug(s"[VOICE] Unhandled voice event: op=$op")
              })
            case Left(error) =>
              Logger[F].error(s"[VOICE] Failed to parse voice event: $error")
          }
        } yield ()
      }.void
    }

    val eventLoop = (ws.receiveText()
      .flatMap(processVoiceEvent) >>
      Async[F].cede  // Yield after processing each message to prevent CPU starvation
    ).foreverM
      .handleErrorWith { error =>
        val errorMsg = Option(error.getMessage).getOrElse("WebSocket closed")
        if (errorMsg.contains("closed") || errorMsg.contains("Closed")) {
          isConnectedRef.set(false) >>  // Mark as disconnected
          Logger[F].debug(s"[VOICE] Voice WebSocket closed - audio streaming completed $error")
        } else {
          Logger[F].error(s"[VOICE] Voice event loop error: $errorMsg") >>
          Logger[F].debug(s"[VOICE] Stack trace: ${error.getStackTrace.take(3).mkString("\n")}")
        }
      }

    eventLoop
  }
  
  private def handleReady(json: io.circe.Json, ws: WebSocket[F]): F[Unit] = {
    val data = json.hcursor.downField("d")
    val extractedSsrc = data.get[Int]("ssrc").getOrElse(0)
    val ip = data.get[String]("ip").getOrElse("")
    val port = data.get[Int]("port").getOrElse(0)
    val modes = data.get[List[String]]("modes").getOrElse(List.empty)

    // Select encryption mode by trying preferred modes in order
    val selectedMode = EncryptionMode.preferredModes
      .flatMap(preferred => modes.find(_ == preferred.value).flatMap(EncryptionMode.fromString))
      .headOption
      .getOrElse(EncryptionMode.AeadXChaCha20Poly1305RtpSize)

    ssrcRef.set(Some(extractedSsrc)) >>
    voiceServerIpRef.set(Some(ip)) >>
    voiceServerPortRef.set(Some(port)) >>
    encryptionModesRef.set(modes) >>
    selectedEncryptionModeRef.set(Some(selectedMode)) >>
    Logger[F].info(s"[VOICE] Connected - encryption: ${selectedMode.value}") >>
    Logger[F].debug(s"[VOICE] Voice ready: SSRC=$extractedSsrc, IP=$ip, Port=$port") >>
    Logger[F].debug(s"[VOICE] Available encryption modes: ${modes.mkString(", ")}") >>
    performIPDiscovery(ws, ip, port, extractedSsrc)
  }
  
  private def performIPDiscovery(ws: WebSocket[F], ip: String, port: Int, ssrc: Int): F[Unit] = {
    for {
      socket <- Async[F].blocking {
        val socket = new DatagramSocket()
        socket.setSoTimeout(5000) // 5 second timeout for response
        socket
      }
      _ <- udpSocketRef.set(Some(socket))
      _ <- Logger[F].debug(s"[VOICE] Performing UDP IP discovery to $ip:$port...")
      
      // Create IP discovery packet
      // Format: 0x0001 [2 bytes type] 0x0046 [2 bytes length=70] [4 bytes SSRC] [66 bytes zeros]
      discoveryPacket <- Async[F].blocking {
        val packet = java.nio.ByteBuffer.allocate(74) // 74 bytes total
        packet.putShort(0x0001.toShort)      // Request type (big-endian)
        packet.putShort(70.toShort)          // Length (big-endian)
        packet.putInt(ssrc)                  // SSRC (big-endian)
        // Remaining 66 bytes are zeros
        packet.array()
      }
      
      // Send discovery packet
      _ <- Async[F].blocking {
        val endpoint = new InetSocketAddress(ip, port)
        val datagram = new java.net.DatagramPacket(discoveryPacket, discoveryPacket.length, endpoint)
        socket.send(datagram)
      }
      _ <- Logger[F].debug(s"[VOICE] Sent IP discovery packet (74 bytes)")
      
      // Receive response
      response <- Async[F].blocking {
        val receiveBuffer = new Array[Byte](74)
        val receivePacket = new java.net.DatagramPacket(receiveBuffer, receiveBuffer.length)
        socket.receive(receivePacket)
        receiveBuffer
      }
      
      // Parse response to extract our external IP and port
      result <- Async[F].blocking {
        val buffer = java.nio.ByteBuffer.wrap(response)
        buffer.position(8) // Skip to address field (after type, length, SSRC)
        
        // Read null-terminated IP string (up to 64 bytes)
        val ipBytes = new Array[Byte](64)
        buffer.get(ipBytes)
        val nullTerminator = ipBytes.indexOf(0)
        val externalIp = new String(ipBytes, 0, if (nullTerminator >= 0) nullTerminator else 64, "UTF-8").trim
        
        // Read port (unsigned short, little-endian at position 72)
        buffer.position(72)
        val externalPort = (buffer.get() & 0xFF) | ((buffer.get() & 0xFF) << 8)
        
        (externalIp, externalPort)
      }
      
      externalIp = result._1
      externalPort = result._2
      
      _ <- Logger[F].debug(s"[VOICE] IP Discovery complete: external IP=$externalIp, port=$externalPort")
      
      // Now send SELECT_PROTOCOL with OUR external IP/port (not Discord's)
      _ <- sendSelectProtocol(ws, externalIp, externalPort)
    } yield ()
  }
  
  private def sendSelectProtocol(ws: WebSocket[F], ip: String, port: Int): F[Unit] = {
    selectedEncryptionModeRef.get.flatMap { modeOpt =>
      val mode = modeOpt.getOrElse(EncryptionMode.AeadXChaCha20Poly1305RtpSize)

      val selectProtocol = Map(
        "op" -> 1.asJson,
        "d" -> Map(
          "protocol" -> "udp".asJson,
          "data" -> Map(
            "address" -> ip.asJson,
            "port" -> port.asJson,
            "mode" -> mode.value.asJson
          ).asJson
//        "codecs" -> Map(
//          "name" -> "opus".asJson,
//          "type" -> "audio".asJson,
//          "priority" -> 1000.asJson,
//          "payload_type" -> 120.asJson
//        ).asJson
        ).asJson
      ).asJson

      ws.sendText(selectProtocol.noSpaces) >>
      Logger[F].debug(s"[VOICE] Sent SELECT_PROTOCOL with encryption mode: ${mode.value}")
    }
  }
  
  private def handleSessionDescription(json: io.circe.Json): F[Unit] = {
    val data = json.hcursor.downField("d")
    val secretKeyList = data.get[List[Int]]("secret_key").getOrElse(List.empty)
    val secretKey = secretKeyList.map(_.toByte).toArray
    val mode = data.get[String]("mode").getOrElse("unknown")

    Logger[F].debug(s"[VOICE] Session description - mode: $mode, secret key length: ${secretKey.length}") >>
    secretKeyRef.set(Some(secretKey)) >>
    voiceReadyRef.set(true)
  }
  
  private def sendHeartbeat(ws: WebSocket[F]): F[Unit] = {
    for {
      hbNum <- seqNum.get
      heartbeat = Map(
        "op" -> 3.asJson,
        "d" -> Map(
          "t" -> Instant.now().toEpochMilli.asJson,
          "seq_ack" -> hbNum.asJson
        ).asJson
      ).asJson
      _ <- Logger[F].debug(s"[VOICE] Sending heartbeat")
      _ <- ws.sendText(heartbeat.noSpaces)
    } yield ()
  }
  
  def startPlaying(ws: WebSocket[F]): F[Unit] = {
    ssrcRef.get.flatMap { ssrcOpt =>
      val speaking = Map(
        "op" -> 5.asJson,
        "d" -> Map(
          "speaking" -> 1.asJson,
          "delay" -> 0.asJson,
          "ssrc" -> ssrcOpt.getOrElse(0).asJson
        ).asJson
      ).asJson

      Logger[F].info(s"[VOICE] Sending SPEAKING=1 payload...") >>
      ws.sendText(speaking.noSpaces).handleErrorWith { error =>
        Logger[F].error(s"[VOICE] ✗ Failed to send SPEAKING payload: ${error.getMessage}") >>
        Logger[F].error(s"[VOICE] WebSocket might be closed. Error type: ${error.getClass.getName}") >>
        Async[F].raiseError(error)
      } >>
      Logger[F].info("[VOICE] ✓ SPEAKING=1 sent successfully")
    }
  }

  def stopPlaying(ws: WebSocket[F]): F[Unit] = {
    ssrcRef.get.flatMap { ssrcOpt =>
      val speaking = Map(
        "op" -> 5.asJson,
        "d" -> Map(
          "speaking" -> 0.asJson,
          "delay" -> 0.asJson,
          "ssrc" -> ssrcOpt.getOrElse(0).asJson
        ).asJson
      ).asJson

      ws.sendText(speaking.noSpaces) >>
      Logger[F].debug("[VOICE] Stopped speaking")
    }
  }

  def streamAudio(streamUrl: String, ws: WebSocket[F], guildId: String): F[Unit] = {
    for {
      socketOpt <- udpSocketRef.get
      ssrcOpt <- ssrcRef.get
      ipOpt <- voiceServerIpRef.get
      portOpt <- voiceServerPortRef.get
      secretKeyOpt <- secretKeyRef.get
      encryptionModeOpt <- selectedEncryptionModeRef.get
      _ <- (socketOpt, ssrcOpt, ipOpt, portOpt, secretKeyOpt) match {
        case (Some(socket), Some(ssrcValue), Some(ip), Some(port), Some(secretKey)) =>
          val encryptionMode = encryptionModeOpt.getOrElse(EncryptionMode.AeadXChaCha20Poly1305RtpSize)
          Logger[F].info(s"[VOICE] Streaming audio...") >>
          Logger[F].debug(s"[VOICE] URL: $streamUrl") >>
          Logger[F].debug(s"[VOICE] Endpoint: $ip:$port, SSRC: $ssrcValue") >>
          Logger[F].debug(s"[VOICE] Encryption: ${encryptionMode.value}") >>
          startPlaying(ws) >>
          audioStreamer.streamAudio(streamUrl, ws, socket, ssrcValue, ip, port, secretKey, guildId, encryptionMode = encryptionMode.value).handleErrorWith { error =>
            Logger[F].error(s"[VOICE] ✗ Streaming error: ${error.getMessage}") >>
            stopPlaying(ws) >>
            closeVoiceConnection()
          } >>
          stopPlaying(ws) >>
          Logger[F].info(s"[VOICE] Playback completed") >>
          closeVoiceConnection()
        case _ =>
          Logger[F].error(s"[VOICE] ✗ Voice connection not properly initialized") >>
          Logger[F].error(s"[VOICE]   socket=$socketOpt, ssrc=$ssrcOpt, ip=$ipOpt, port=$portOpt, secretKey=$secretKeyOpt")
      }
    } yield ()
  }

  def closeVoiceConnection(): F[Unit] = {
    shutdownSignalRef.get.flatMap {
      case Some(signal) =>
        Logger[F].info("[VOICE] Sending shutdown signal to close voice connection") >>
        isConnectedRef.set(false) >>  // Mark as disconnected
        signal.complete(()).void
      case None =>
        Logger[F].debug("[VOICE] No active voice connection to close")
    }
  }

  def streamAudioFromPosition(streamUrl: String, ws: WebSocket[F], startPosition: Int, guildId: String): F[Unit] = {
    for {
      socketOpt <- udpSocketRef.get
      ssrcOpt <- ssrcRef.get
      ipOpt <- voiceServerIpRef.get
      portOpt <- voiceServerPortRef.get
      secretKeyOpt <- secretKeyRef.get
      encryptionModeOpt <- selectedEncryptionModeRef.get
      _ <- (socketOpt, ssrcOpt, ipOpt, portOpt, secretKeyOpt) match {
        case (Some(socket), Some(ssrcValue), Some(ip), Some(port), Some(secretKey)) =>
          val encryptionMode = encryptionModeOpt.getOrElse(EncryptionMode.AeadXChaCha20Poly1305RtpSize)
          Logger[F].info(s"[VOICE] Streaming audio from position ${startPosition}s...") >>
          Logger[F].debug(s"[VOICE] URL: $streamUrl") >>
          startPlaying(ws) >>
          audioStreamer.streamAudioFromPosition(streamUrl, ws, socket, ssrcValue, ip, port, secretKey, startPosition, guildId, encryptionMode = encryptionMode.value).handleErrorWith { error =>
            Logger[F].error(s"[VOICE] ✗ Streaming error: ${error.getMessage}") >>
            stopPlaying(ws) >>
            closeVoiceConnection()
          } >>
          stopPlaying(ws) >>
          Logger[F].info(s"[VOICE] Playback completed") >>
          closeVoiceConnection()
        case _ =>
          Logger[F].error(s"[VOICE] ✗ Voice connection not properly initialized")
      }
    } yield ()
  }
}