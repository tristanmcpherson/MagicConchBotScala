package dev.raegous.magicconch.audio.internals

import cats.effect.*
import cats.effect.std.{Queue, Supervisor}
import cats.implicits.*
import org.typelevel.log4cats.Logger
import sttp.client4.*
import sttp.ws.{WebSocket, WebSocketClosed, WebSocketFrame}
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
import java.time.Instant
import scala.concurrent.duration.*

object VoiceGateway {
  private final case class DaveOutboundDiagnostics(
    keyPackagesSent: Long,
    commitWelcomesSent: Long,
    readyTransitionsSent: Long,
    invalidCommitWelcomesSent: Long
  )

  private object DaveOutboundDiagnostics {
    val initial: DaveOutboundDiagnostics = DaveOutboundDiagnostics(0L, 0L, 0L, 0L)
  }

  def make[F[_]: Logger: Async: fs2.io.process.Processes](
    backend: WebSocketStreamBackend[F, ?],
    audioStreamer: AudioStreamer[F]): Resource[F, VoiceGateway[F]] = {
    val allocate: F[VoiceGateway[F]] =
      for {
        udpSocketRef              <- Ref.of[F, Option[DatagramSocket]](None)
        ssrcRef                   <- Ref.of[F, Option[Int]](None)
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
        isConnectedRef            <- Ref.of[F, Boolean](false)
        daveSessionManagerRef     <- Ref.of[F, Option[DaveSessionManager[F]]](None)
        daveProtocolVersionRef    <- Ref.of[F, Int](0)
        daveOutboundDiagnosticsRef <- Ref.of[F, DaveOutboundDiagnostics](DaveOutboundDiagnostics.initial)
      } yield new VoiceGateway[F](
        backend = backend,
        audioStreamer = audioStreamer,
        udpSocketRef = udpSocketRef,
        ssrcRef = ssrcRef,
        voiceServerIpRef = voiceServerIpRef,
        voiceServerPortRef = voiceServerPortRef,
        encryptionModesRef = encryptionModesRef,
        selectedEncryptionModeRef = selectedEncryptionModeRef,
        secretKeyRef = secretKeyRef,
        seqNum = seqNum,
        sendQueueRef = sendQueueRef,
        shutdownSignalRef = shutdownSignalRef,
        webSocketRef = webSocketRef,
        isConnectedRef = isConnectedRef,
        daveSessionManagerRef = daveSessionManagerRef,
        daveProtocolVersionRef = daveProtocolVersionRef,
        daveOutboundDiagnosticsRef = daveOutboundDiagnosticsRef,
      )

    Resource.make(allocate)(_.closeVoiceConnection("resource finalizer").attempt.void)
  }
}

class VoiceGateway[F[_]: Async: fs2.io.process.Processes] private (
  backend: WebSocketStreamBackend[F, ?],
  audioStreamer: AudioStreamer[F],
  private val udpSocketRef: Ref[F, Option[DatagramSocket]],
  private val ssrcRef: Ref[F, Option[Int]],
  private val voiceServerIpRef: Ref[F, Option[String]],
  private val voiceServerPortRef: Ref[F, Option[Int]],
  private val encryptionModesRef: Ref[F, List[String]],
  private val selectedEncryptionModeRef: Ref[F, Option[EncryptionMode]],
  private val secretKeyRef: Ref[F, Option[Array[Byte]]],
  private val seqNum: Ref[F, Int],
  private val sendQueueRef: Ref[F, Option[Queue[F, String]]],
  private val shutdownSignalRef: Ref[F, Option[Deferred[F, Unit]]],
  private val webSocketRef: Ref[F, Option[WebSocket[F]]],
  private val isConnectedRef: Ref[F, Boolean],
  private val daveSessionManagerRef: Ref[F, Option[DaveSessionManager[F]]],
  private val daveProtocolVersionRef: Ref[F, Int],
  private val daveOutboundDiagnosticsRef: Ref[F, VoiceGateway.DaveOutboundDiagnostics]
)(using Logger[F]) {
  private val daveMediaReadyPollInterval = 500.millis
  private val daveMediaReadyTimeout = 4.seconds

  def connectToVoiceGateway(
    endpoint: String,
    guildId: String,
    userId: String,
    sessionId: String,
    token: String,
    channelId: String
  ): F[WebSocket[F]] = {
    val voiceGatewayUrl = s"wss://${endpoint.replace(":80", "")}/?v=8"
    val request = basicRequest.get(uri"$voiceGatewayUrl")

    for {
      _ <- closeVoiceConnection("replacing existing voice connection before connect")
      daveSessionManager <- DaveSessionManager.create[F](userId, channelId)
      _ <- daveSessionManagerRef.set(Some(daveSessionManager))
      shutdownSignal <- Deferred[F, Unit]
      _ <- shutdownSignalRef.set(Some(shutdownSignal))

      // Completed with Right(ws) when handshake succeeds, Left(error) on any failure.
      // This propagates errors immediately rather than waiting for a timeout.
      wsResult <- Deferred[F, Either[Throwable, WebSocket[F]]]

      _ <- Async[F].start {
        backend.send(request.response(asWebSocket { (ws: WebSocket[F]) =>
          val connect: F[Unit] = for {
            _ <- Logger[F].info(s"[VOICE] Connected to voice gateway: $voiceGatewayUrl")
            _ <- webSocketRef.set(Some(ws))
            _ <- isConnectedRef.set(true)
            handshakeDeferred <- Deferred[F, Either[Throwable, Unit]]
            helloDeferred <- Deferred[F, Unit]
            _ <- Async[F].start(handleVoiceEvents(ws, handshakeDeferred, helloDeferred))
            _ <- helloDeferred.get.timeoutTo(
              5.seconds,
              Async[F].raiseError(new RuntimeException("Timeout waiting for voice hello after 5 seconds"))
            )
            _ <- sendVoiceIdentify(ws, guildId, userId, sessionId, token, daveSessionManager.maxSupportedProtocolVersion)
            _ <- Logger[F].info("[VOICE] Waiting for voice connection to be ready...")
            _ <- handshakeDeferred.get.rethrow.timeoutTo(
              15.seconds,
              Async[F].raiseError(new RuntimeException("Timeout waiting for voice handshake after 15 seconds"))
            )
            _ <- Logger[F].info("[VOICE] Voice connection is ready!")
            _ <- wsResult.complete(Right(ws)).void
            _ <- Logger[F].info("[VOICE] Handler blocking on shutdown signal (keeps connection alive)...")
            _ <- shutdownSignal.get
            _ <- Logger[F].info("[VOICE] Shutdown signal received, closing connection")
            _ <- isConnectedRef.set(false)
          } yield ()

          connect.handleErrorWith { error =>
            val msg = errorMessage(error)
            wsResult.complete(Left(error)).void >>
            Logger[F].error(s"[VOICE] Voice gateway connection failed: $msg")
          }
        })).flatMap { response =>
          response.body match {
            case Left(error) => wsResult.complete(Left(new RuntimeException(error.toString))).attempt.void >> Logger[F].error(s"[VOICE] WebSocket error: $error")
            case Right(_) => Logger[F].info("[VOICE] WebSocket handler completed")
          }
        }.handleErrorWith { error =>
          wsResult.complete(Left(error)).attempt.void >>
          Logger[F].error(s"[VOICE] Voice WebSocket fiber error: ${errorMessage(error)}")
        }
      }

      ws <- wsResult.get.rethrow
      _ <- Logger[F].info("[VOICE] Voice connection established and staying alive")
    } yield ws
  }

  private def startVoiceHeartbeat(heartbeatIntervalMs: Int, ws: WebSocket[F]): F[Unit] = {
    Logger[F].info(s"[VOICE] Starting voice heartbeat (interval: ${heartbeatIntervalMs}ms)") >>
    Async[F].start {
      (Stream.eval(Async[F].sleep(heartbeatIntervalMs.millis)) ++
        fs2.Stream.fixedRateStartImmediately[F](heartbeatIntervalMs.millis))
        .evalMap { _ =>
          isConnectedRef.get.flatMap { isConnected =>
            Option.when(isConnected)(
              sendHeartbeat(ws).handleErrorWith { error =>
                val errorMsg = Option(error.getMessage).getOrElse("unknown error")
                Option.when(
                  errorMsg.contains("closed") || errorMsg.contains("Closed") || errorMsg.contains("Output closed")
                )(
                  Logger[F].warn(s"[VOICE] Heartbeat stopped - WebSocket closed") >>
                  isConnectedRef.set(false) >>
                  Async[F].raiseError[Unit](error)
                ).getOrElse(
                  Logger[F].warn(s"[VOICE] Failed to send voice heartbeat: $errorMsg")
                )
              }
            ).getOrElse(
              Logger[F].debug(s"[VOICE] Heartbeat loop exiting - connection closed") >>
              Async[F].raiseError[Unit](new Exception("Voice connection closed"))
            )
          }
        }
        .compile
        .drain
        .handleErrorWith { _ =>
          Logger[F].debug(s"[VOICE] Voice heartbeat loop stopped")
        }
    }.void
  }

  private def sendVoiceIdentify(
    ws: WebSocket[F],
    guildId: String,
    userId: String,
    sessionId: String,
    token: String,
    maxDaveProtocolVersion: Int
  ): F[Unit] = {
    val identify = Map(
      "op" -> 0.asJson,
      "d" -> Map(
        "server_id" -> guildId.asJson,
        "user_id" -> userId.asJson,
        "session_id" -> sessionId.asJson,
        "token" -> token.asJson,
        "max_dave_protocol_version" -> maxDaveProtocolVersion.asJson
      ).asJson
    ).asJson

    ws.sendText(identify.noSpaces) >>
    Logger[F].info(
      s"[VOICE] Sent identify event: guildId=$guildId userHash=${shortHash(userId)} sessionHash=${shortHash(sessionId)} maxDaveProtocolVersion=$maxDaveProtocolVersion"
    )
  }

  private def shortHash(userId: String) = userId

  private def handleVoiceEvents(
    ws: WebSocket[F],
    handshakeDeferred: Deferred[F, Either[Throwable, Unit]],
    helloDeferred: Deferred[F, Unit]
  ): F[Unit] = {
    def processVoiceEvent(message: String): F[Unit] = {
      (for {
        _ <- logVoiceEventSummary(message)
        _ <- parse(message) match {
          case Right(json) =>
            val op = json.hcursor.get[Int]("op").getOrElse(-1)
            val seq = json.hcursor.get[Int]("seq").getOrElse(-1)
            seqNum.update(_ => seq) >>
              (op match {
              case 2 => handleReady(json, ws)
              case 3 => sendHeartbeat(ws)
              case 4 => handleSessionDescription(json, ws, handshakeDeferred)
              case 6 => Logger[F].debug("[VOICE] Heartbeat ack'd")
              case 8 =>
                startVoiceHeartbeat(json.hcursor.downField("d").get[Int]("heartbeat_interval").getOrElse(-1), ws) >>
                helloDeferred.complete(()).void
              case 11 => handleDaveClientsConnect(json)
              case 13 => handleDaveClientDisconnect(json)
              case 21 => handleDavePrepareTransition(json, ws)
              case 22 => handleDaveExecuteTransition(json)
              case 24 => handleDavePrepareEpoch(json, ws)
              case 31 => Logger[F].warn("[DAVE] MLS invalid commit/welcome received")
              case _ => Logger[F].debug(s"[VOICE] Unhandled voice event: op=$op")
            })
          case Left(error) =>
            Logger[F].error(s"[VOICE] Failed to parse voice event: $error")
        }
      } yield ()).handleErrorWith { error =>
        handshakeDeferred.complete(Left(error)).attempt.void >>
        Logger[F].error(s"[VOICE] Error processing voice event: ${errorMessage(error)}") >>
        Async[F].raiseError(error)
      }
    }

    def processVoiceFrame(frame: WebSocketFrame): F[Unit] = frame match {
      case WebSocketFrame.Text(payload, _, _) =>
        processVoiceEvent(payload)
      case WebSocketFrame.Binary(payload, finalFragment, _) =>
        handleDaveBinaryMessage(payload, finalFragment)
      case WebSocketFrame.Ping(payload) =>
        ws.send(WebSocketFrame.Pong(payload))
      case WebSocketFrame.Pong(_) =>
        Logger[F].debug("[VOICE] Voice WebSocket pong received")
      case close: WebSocketFrame.Close =>
        Async[F].raiseError(WebSocketClosed(Some(close)))
    }

    val eventLoop: F[Unit] = (ws.receive()
      .flatMap(processVoiceFrame) >>
      Async[F].cede
    ).foreverM[Unit]
      .handleErrorWith { error =>
        // Signal handshake failure so connectToVoiceGateway fails immediately rather than timing out
        handshakeDeferred.complete(Left(error)).attempt.void >>
        (error match {
          case e: WebSocketClosed =>
            isConnectedRef.set(false) >>
            Logger[F].warn(s"[VOICE] Voice WebSocket closed by Discord: ${e.frame.fold("no close frame")(f => s"code=${f.statusCode}, reason='${f.reasonText}'")}")
          case _ =>
            Logger[F].error(s"[VOICE] Voice event loop error: ${errorMessage(error)}") >>
            Logger[F].debug(s"[VOICE] Stack trace: ${error.getStackTrace.take(3).mkString("\n")}")
        })
      }

    eventLoop
  }

  private def handleReady(json: io.circe.Json, ws: WebSocket[F]): F[Unit] = {
    val data = json.hcursor.downField("d")
    val extractedSsrc = data.get[Int]("ssrc").getOrElse(0)
    val ip = data.get[String]("ip").getOrElse("")
    val port = data.get[Int]("port").getOrElse(0)
    val modes = data.get[List[String]]("modes").getOrElse(List.empty)

    val selectedMode = EncryptionMode.preferredModes
      .flatMap(preferred => modes.find(_ == preferred.value).flatMap(EncryptionMode.fromString))
      .headOption
      .getOrElse(EncryptionMode.AeadXChaCha20Poly1305RtpSize)

    ssrcRef.set(Some(extractedSsrc)) >>
    voiceServerIpRef.set(Some(ip)) >>
    voiceServerPortRef.set(Some(port)) >>
    encryptionModesRef.set(modes) >>
    selectedEncryptionModeRef.set(Some(selectedMode)) >>
    Logger[F].info(s"[VOICE] Connected - encryption: ${selectedMode.value}, voiceReadySsrc=$extractedSsrc") >>
    Logger[F].debug(s"[VOICE] Voice ready: SSRC=$extractedSsrc, IP=$ip, Port=$port") >>
    Logger[F].debug(s"[VOICE] Available encryption modes: ${modes.mkString(", ")}") >>
    performIPDiscovery(ws, ip, port, extractedSsrc)
  }

  private def performIPDiscovery(ws: WebSocket[F], ip: String, port: Int, ssrc: Int): F[Unit] = {
    val maxDiscoveryAttempts = 3

    def attemptDiscovery(socket: DatagramSocket, discoveryPacket: Array[Byte], attempt: Int): F[Array[Byte]] = {
      val sendAndReceive =
        Async[F].blocking {
          val endpoint = new InetSocketAddress(ip, port)
          val datagram = new java.net.DatagramPacket(discoveryPacket, discoveryPacket.length, endpoint)
          socket.send(datagram)
        } >>
        Logger[F].debug(s"[VOICE] Sent IP discovery packet (74 bytes) on attempt $attempt/$maxDiscoveryAttempts") >>
        Async[F].blocking {
          val receiveBuffer = new Array[Byte](74)
          val receivePacket = new java.net.DatagramPacket(receiveBuffer, receiveBuffer.length)
          socket.receive(receivePacket)
          receiveBuffer
        }

      sendAndReceive.attempt.flatMap {
        case Right(response) => response.pure[F]
        case Left(error) =>
          val failureLog = Logger[F].warn(
            s"[VOICE] UDP IP discovery attempt $attempt/$maxDiscoveryAttempts failed: ${errorMessage(error)}"
          )

          Option.when(attempt < maxDiscoveryAttempts)(attempt + 1).fold(
            failureLog >>
            Logger[F].error(
              s"[VOICE] UDP IP discovery failed after $maxDiscoveryAttempts/$maxDiscoveryAttempts attempts; aborting voice handshake"
            ) >>
            Async[F].raiseError[Array[Byte]](error)
          ) { nextAttempt =>
            failureLog >>
            Logger[F].info(s"[VOICE] Retrying UDP IP discovery (attempt $nextAttempt/$maxDiscoveryAttempts)") >>
            attemptDiscovery(socket, discoveryPacket, nextAttempt)
          }
      }
    }

    for {
      socket <- Async[F].blocking {
        val socket = new DatagramSocket()
        socket.setSoTimeout(5000)
        socket
      }
      _ <- udpSocketRef.set(Some(socket))
      _ <- Logger[F].debug(s"[VOICE] Performing UDP IP discovery to $ip:$port...")

      discoveryPacket <- Async[F].blocking {
        val packet = java.nio.ByteBuffer.allocate(74)
        packet.putShort(0x0001.toShort)
        packet.putShort(70.toShort)
        packet.putInt(ssrc)
        packet.array()
      }

      response <- Logger[F].debug(s"[VOICE] Starting UDP IP discovery attempt 1/$maxDiscoveryAttempts") >>
        attemptDiscovery(socket, discoveryPacket, 1)

      result <- Async[F].blocking {
        val buffer = java.nio.ByteBuffer.wrap(response)
        buffer.position(8)

        val ipBytes = new Array[Byte](64)
        buffer.get(ipBytes)
        val nullTerminator = ipBytes.indexOf(0)
        val externalIp = new String(ipBytes, 0, Option.when(nullTerminator >= 0)(nullTerminator).getOrElse(64), "UTF-8").trim

        buffer.position(72)
        val externalPort = buffer.getShort() & 0xFFFF

        (externalIp, externalPort)
      }

      externalIp = result._1
      externalPort = result._2

      _ <- Logger[F].debug(s"[VOICE] IP Discovery complete: external IP=$externalIp, port=$externalPort")
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
        ).asJson
      ).asJson

      ws.sendText(selectProtocol.noSpaces) >>
      Logger[F].debug(s"[VOICE] Sent SELECT_PROTOCOL with encryption mode: ${mode.value}")
    }
  }

  private def handleSessionDescription(json: io.circe.Json, ws: WebSocket[F], handshakeDeferred: Deferred[F, Either[Throwable, Unit]]): F[Unit] = {
    val data = json.hcursor.downField("d")
    val secretKeyList = data.get[List[Int]]("secret_key").getOrElse(List.empty)
    val secretKey = secretKeyList.map(_.toByte).toArray
    val mode = data.get[String]("mode").getOrElse("unknown")
    val daveProtocolVersion = data.get[Int]("dave_protocol_version").getOrElse(0)

    Logger[F].debug(s"[VOICE] Session description - mode: $mode, secret key length: ${secretKey.length}, dave=$daveProtocolVersion") >>
    daveProtocolVersionRef.set(daveProtocolVersion) >>
    Option.when(daveProtocolVersion > 0)(
      Logger[F].info(s"[DAVE] Discord selected DAVE protocol v$daveProtocolVersion; initializing MLS session") >>
      withDaveManager(_.onSelectProtocolAck(daveProtocolVersion)).flatMap(sendDaveActions(ws)) >>
      logDaveState("select protocol ack")
    ).getOrElse(Async[F].unit) >>
    secretKeyRef.set(Some(secretKey)) >>
    handshakeDeferred.complete(Right(())).void
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
      val speakingSsrc = ssrcOpt.getOrElse(0)
      val speaking = Map(
        "op" -> 5.asJson,
        "d" -> Map(
          "speaking" -> 1.asJson,
          "delay" -> 0.asJson,
          "ssrc" -> speakingSsrc.asJson
        ).asJson
      ).asJson

      daveSessionManagerRef.get.flatMap(_.fold(Async[F].unit)(_.debugState.flatMap(state => Logger[F].info(s"[VOICE] SPEAKING=1 correlation: voiceReadySsrc=$speakingSsrc daveState=$state")))) >>
      Logger[F].info(s"[VOICE] Sending SPEAKING=1 payload for ssrc=$speakingSsrc...") >>
      ws.sendText(speaking.noSpaces).handleErrorWith { error =>
        Logger[F].error(s"[VOICE] Failed to send SPEAKING payload: ${error.getMessage}") >>
        Logger[F].error(s"[VOICE] WebSocket might be closed. Error type: ${error.getClass.getName}") >>
        Async[F].raiseError(error)
      } >>
      Logger[F].info("[VOICE] SPEAKING=1 sent successfully")
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
      _ <- waitForDaveMediaReady
      _ <- (socketOpt, ssrcOpt, ipOpt, portOpt, secretKeyOpt) match {
        case (Some(socket), Some(ssrcValue), Some(ip), Some(port), Some(secretKey)) =>
          val encryptionMode = encryptionModeOpt.getOrElse(EncryptionMode.AeadXChaCha20Poly1305RtpSize)
          Logger[F].info(s"[VOICE] Streaming audio...") >>
          Logger[F].debug("[VOICE] Stream URL redacted") >>
          Logger[F].debug(s"[VOICE] Endpoint: $ip:$port, SSRC: $ssrcValue") >>
          Logger[F].debug(s"[VOICE] Encryption: ${encryptionMode.value}") >>
          daveAudioEncryptor(ssrcValue).flatMap { daveEncryptor =>
            startPlaying(ws) >>
            audioStreamer.streamAudio(streamUrl, ws, socket, ssrcValue, ip, port, secretKey, guildId, encryptionMode = encryptionMode.value, daveAudioEncryptor = daveEncryptor).handleErrorWith { error =>
              Logger[F].error(s"[VOICE] Streaming error: ${error.getMessage}") >>
              stopPlaying(ws) >>
              closeVoiceConnection("audio stream error")
            } >>
            stopPlaying(ws) >>
            Logger[F].info(s"[VOICE] Playback completed") >>
            closeVoiceConnection("audio stream completed")
          }
        case _ =>
          Logger[F].error(s"[VOICE] Voice connection not properly initialized") >>
          Logger[F].error(
            s"[VOICE]   socketPresent=${socketOpt.nonEmpty}, ssrcPresent=${ssrcOpt.nonEmpty}, ipPresent=${ipOpt.nonEmpty}, portPresent=${portOpt.nonEmpty}, secretKeyPresent=${secretKeyOpt.nonEmpty}"
          )
      }
    } yield ()
  }

  private def handleDavePrepareTransition(json: io.circe.Json, ws: WebSocket[F]): F[Unit] = {
    val transitionId = json.hcursor.downField("d").get[Int]("transition_id").getOrElse(0)
    val protocolVersion = json.hcursor.downField("d").get[Int]("protocol_version").getOrElse(0)
    Logger[F].info(s"[DAVE] Prepare transition: id=$transitionId protocol=$protocolVersion") >>
    withDaveManager(_.onPrepareTransition(transitionId, protocolVersion)).flatMap(sendDaveActions(ws)) >>
    logDaveState(s"prepare transition $transitionId")
  }

  private def handleDaveClientsConnect(json: io.circe.Json): F[Unit] = {
    val userIds = json.hcursor.downField("d").get[List[String]]("user_ids").getOrElse(Nil)
    withDaveManager(_.addUsers(userIds)).flatMap(sendDaveActionsFromActiveSocket) >>
    Logger[F].info(s"[DAVE] Connected user_ids count=${userIds.size} values=${userIds.map(shortHash).mkString("[", ",", "]")}") >>
    logDaveState("clients connect")
  }

  private def handleDaveClientDisconnect(json: io.circe.Json): F[Unit] = {
    val userId = json.hcursor.downField("d").get[String]("user_id").toOption
    userId.fold(Async[F].unit) { id =>
      withDaveManager(_.removeUser(id)) >>
      Logger[F].debug("[DAVE] Recognized disconnected user event")
    }
  }

  private def handleDaveExecuteTransition(json: io.circe.Json): F[Unit] = {
    val transitionId = json.hcursor.downField("d").get[Int]("transition_id").getOrElse(0)
    Logger[F].info(s"[DAVE] Execute transition: id=$transitionId") >>
    withDaveManager { manager =>
      manager.onExecuteTransition(transitionId) >>
      manager.debugState.flatMap(state => Logger[F].info(s"[DAVE] State after execute transition $transitionId: $state"))
    }
  }

  private def handleDavePrepareEpoch(json: io.circe.Json, ws: WebSocket[F]): F[Unit] = {
    val d = json.hcursor.downField("d")
    val transitionId = d.get[Int]("transition_id").getOrElse(0)
    val epoch = d.get[Int]("epoch").getOrElse(0)
    val protocolVersion = d.get[Int]("protocol_version").getOrElse(0)
    Logger[F].info(s"[DAVE] Prepare epoch: transition_id=$transitionId epoch=$epoch protocol=$protocolVersion") >>
    daveProtocolVersionRef.set(protocolVersion) >>
    withDaveManager(_.onPrepareEpoch(transitionId, epoch.toLong, protocolVersion)).flatMap(sendDaveActions(ws)) >>
    logDaveState(s"prepare epoch $epoch transition $transitionId")
  }

  private def handleDaveBinaryMessage(payload: Array[Byte], finalFragment: Boolean): F[Unit] =
    Logger[F].info(s"[DAVE] Binary frame ${describeDaveBinaryEnvelope(payload, finalFragment)}") >> (
      DaveSupport.GatewayBinaryCodec.decode(payload) match {
      case Right(message: DaveSupport.MlsExternalSenderPackage) =>
        Logger[F].info(s"[DAVE] Received MLS external sender package seq=${message.sequenceNumber}") >>
        withDaveManager(_.onExternalSenderPackage(payload.drop(3))).flatTap(logDaveActions("external sender package")).flatMap(sendDaveActionsFromActiveSocket)
      case Right(message: DaveSupport.MlsProposals) =>
        Logger[F].info(s"[DAVE] Received MLS proposals seq=${message.sequenceNumber}, op=${message.operationType}, payload=${message.payload.length} bytes") >>
        withDaveManager(_.onMlsProposals(payload.drop(3))).flatTap(logDaveActions("proposal processing")).flatMap(sendDaveActionsFromActiveSocket)
      case Right(message: DaveSupport.MlsAnnounceCommitTransition) =>
        Logger[F].info(s"[DAVE] Received MLS commit transition seq=${message.sequenceNumber}, transition=${message.transitionId}, commit=${message.commitMessage.length} bytes") >>
        withDaveManager(_.onMlsCommitTransition(message.transitionId, message.commitMessage)).flatMap(sendDaveActionsFromActiveSocket)
      case Right(message: DaveSupport.MlsWelcome) =>
        Logger[F].info(s"[DAVE] Received MLS welcome seq=${message.sequenceNumber}, transition=${message.transitionId}, welcome=${message.welcomeMessage.length} bytes") >>
        withDaveManager(_.onMlsWelcome(message.transitionId, message.welcomeMessage)).flatMap(sendDaveActionsFromActiveSocket)
      case Right(message) =>
        Logger[F].debug(s"[DAVE] Ignoring ${message.getClass.getSimpleName}")
      case Left(error) =>
        Logger[F].warn(s"[DAVE] Failed to decode binary voice gateway message (${payload.length} bytes): $error")
      }
    )

  private def withDaveManager[A](f: DaveSessionManager[F] => F[A]): F[A] =
    daveSessionManagerRef.get.flatMap {
      case Some(manager) => f(manager)
      case None => Async[F].raiseError(new RuntimeException("DAVE session manager is not initialized"))
    }

  private def sendDaveActionsFromActiveSocket(actions: List[DaveGatewayAction]): F[Unit] =
    webSocketRef.get.flatMap {
      case Some(ws) => sendDaveActions(ws)(actions)
      case None if actions.isEmpty => Async[F].unit
      case None => Async[F].raiseError(new RuntimeException("Voice WebSocket is not available for DAVE response"))
    }

  private def logDaveActions(context: String)(actions: List[DaveGatewayAction]): F[Unit] =
    if (actions.isEmpty) {
      daveSessionManagerRef.get.flatMap(_.traverse_(manager =>
        manager.debugState.flatMap(state => Logger[F].warn(s"[DAVE] No outbound DAVE actions after $context; state: $state"))
      ))
    } else {
      val summary = actions.map {
        case DaveGatewayAction.SendMlsKeyPackage(payload) => s"key_package=${payload.length}B"
        case DaveGatewayAction.SendMlsCommitWelcome(payload) => s"commit_welcome=${payload.length}B"
        case DaveGatewayAction.SendReadyForTransition(transitionId) => s"ready_transition=$transitionId"
        case DaveGatewayAction.SendInvalidCommitWelcome(transitionId) => s"invalid_commit_welcome=$transitionId"
      }.mkString(", ")
      Logger[F].info(s"[DAVE] Outbound actions after $context: $summary")
    }

  private def sendDaveActions(ws: WebSocket[F])(actions: List[DaveGatewayAction]): F[Unit] =
    actions.traverse_ {
      case DaveGatewayAction.SendMlsKeyPackage(keyPackage) =>
        logKeyPackageSummary(keyPackage) >> {
          val payload = DaveSupport.GatewayBinaryCodec.encode(DaveSupport.MlsKeyPackage(keyPackage))
          incrementDaveOutboundDiagnostics(diagnostics =>
            diagnostics.copy(keyPackagesSent = diagnostics.keyPackagesSent + 1L)
          ).flatMap { diagnostics =>
            Logger[F].info(
              s"[DAVE] Sending MLS key package #${diagnostics.keyPackagesSent} (opcode ${DaveSupport.OpMlsKeyPackage}, mls=${keyPackage.length}B, gateway=${payload.length}B)"
            )
          } >>
          ws.sendBinary(payload)
        }

      case DaveGatewayAction.SendMlsCommitWelcome(commitWelcome) =>
        val payload = DaveSupport.GatewayBinaryCodec.encode(DaveSupport.MlsCommitWelcome(commitWelcome, None))
        incrementDaveOutboundDiagnostics(diagnostics =>
          diagnostics.copy(commitWelcomesSent = diagnostics.commitWelcomesSent + 1L)
        ).flatMap { diagnostics =>
          Logger[F].info(
            s"[DAVE] Sending MLS commit/welcome #${diagnostics.commitWelcomesSent} (opcode ${DaveSupport.OpMlsCommitWelcome}, mls=${commitWelcome.length}B, gateway=${payload.length}B)"
          )
        } >>
        ws.sendBinary(payload)

      case DaveGatewayAction.SendReadyForTransition(transitionId) =>
        val msg = Map(
          "op" -> DaveSupport.OpReadyForTransition.asJson,
          "d" -> Map("transition_id" -> transitionId.asJson).asJson
        ).asJson
        ws.sendText(msg.noSpaces) >>
        incrementDaveOutboundDiagnostics(diagnostics =>
          diagnostics.copy(readyTransitionsSent = diagnostics.readyTransitionsSent + 1L)
        ).flatMap { diagnostics =>
          Logger[F].info(s"[DAVE] Sent DAVE_TRANSITION_READY #${diagnostics.readyTransitionsSent} transition_id=$transitionId")
        }

      case DaveGatewayAction.SendInvalidCommitWelcome(transitionId) =>
        val msg = Map(
          "op" -> DaveSupport.OpMlsInvalidCommitWelcome.asJson,
          "d" -> Map("transition_id" -> transitionId.asJson).asJson
        ).asJson
        ws.sendText(msg.noSpaces) >>
        incrementDaveOutboundDiagnostics(diagnostics =>
          diagnostics.copy(invalidCommitWelcomesSent = diagnostics.invalidCommitWelcomesSent + 1L)
        ).flatMap { diagnostics =>
          Logger[F].warn(s"[DAVE] Sent MLS_INVALID_COMMIT_WELCOME #${diagnostics.invalidCommitWelcomesSent} transition_id=$transitionId")
        }
    }

  private def incrementDaveOutboundDiagnostics(
    update: VoiceGateway.DaveOutboundDiagnostics => VoiceGateway.DaveOutboundDiagnostics
  ): F[VoiceGateway.DaveOutboundDiagnostics] =
    daveOutboundDiagnosticsRef.modify { current =>
      val next = update(current)
      (next, next)
    }

  private def logDaveState(context: String): F[Unit] =
    daveSessionManagerRef.get.flatMap(
      _.fold(Async[F].unit)(manager =>
        manager.debugState.flatMap(state => Logger[F].info(s"[DAVE] State after $context: $state"))
      )
    )

  private def daveAudioEncryptor(ssrc: Int): F[Option[Array[Byte] => F[Array[Byte]]]] =
    (daveProtocolVersionRef.get, daveSessionManagerRef.get).flatMapN {
      case (version, Some(manager)) if version > 0 =>
        manager.isMediaReady.flatMap {
          case true =>
            manager.assignAudioSsrc(ssrc) >>
            manager.debugState.flatMap(state => Logger[F].info(s"[DAVE] Bound media encryptor to ssrc=$ssrc; state: $state")) >>
            Async[F].pure(Some((payload: Array[Byte]) => manager.encryptAudio(ssrc, payload)))
          case false =>
            manager.debugState.flatMap { state =>
              val message = s"DAVE negotiated protocol v$version but media ratchet is unavailable for SSRC $ssrc: $state"
              Logger[F].error(s"[DAVE] $message") >> Async[F].raiseError[Option[Array[Byte] => F[Array[Byte]]]](new RuntimeException(message))
            }
        }
      case _ =>
        Async[F].pure(None)
    }

  private def waitForDaveMediaReady: F[Unit] =
    (daveProtocolVersionRef.get, daveSessionManagerRef.get).flatMapN {
      case (version, Some(manager)) if version > 0 =>
        awaitDaveMediaReady(manager, daveMediaReadyTimeout, waitLogged = false)
      case _ =>
        Logger[F].info("[DAVE] MLS media ratchet is ready; starting playback")
    }

  private def awaitDaveMediaReady(
    manager: DaveSessionManager[F],
    remaining: FiniteDuration,
    waitLogged: Boolean
  ): F[Unit] =
    manager.isMediaReady.flatMap {
      case true =>
        manager.debugState.flatMap(state => Logger[F].info(s"[DAVE] MLS media ratchet is ready; starting playback; state: $state"))
      case false =>
        Option.when(remaining > Duration.Zero)(remaining - daveMediaReadyPollInterval).fold {
          manager.debugState.flatMap { state =>
            val message = s"DAVE negotiated protocol but media ratchet remained unavailable after ${daveMediaReadyTimeout.toSeconds} seconds; state: $state"
            Logger[F].error(s"[DAVE] $message") >> Async[F].raiseError[Unit](new RuntimeException(message))
          }
        } { nextRemaining =>
          Option.when(!waitLogged)(
            manager.debugState.flatMap(state =>
              Logger[F].warn(s"[DAVE] MLS media ratchet pending after op25/op26; waiting briefly for op27/op29/op30/op22 before playback; state: $state")
            )
          ).getOrElse(Async[F].unit) >>
          Async[F].sleep(daveMediaReadyPollInterval) >>
          awaitDaveMediaReady(manager, nextRemaining, waitLogged = true)
        }
    }




  private def errorMessage(error: Throwable): String =
    Option(error.getMessage).getOrElse(error.getClass.getSimpleName)

  private def describeDaveBinaryEnvelope(payload: Array[Byte], finalFragment: Boolean): String = {
    val opcodeOpt = payload.headOption.map(_ & 0xFF).flatMap { firstByte =>
      Option.when(firstByte == DaveSupport.OpMlsKeyPackage || firstByte == DaveSupport.OpMlsCommitWelcome)(firstByte)
        .orElse(payload.lift(2).map(_ & 0xFF))
    }
    val seq = opcodeOpt.fold("n/a") { opcode =>
      Option.when(opcode == DaveSupport.OpMlsKeyPackage || opcode == DaveSupport.OpMlsCommitWelcome)("n/a")
        .getOrElse(
          Option.when(payload.length >= 3)(((payload(0) & 0xFF) << 8) | (payload(1) & 0xFF)).fold("n/a")(_.toString)
        )
    }

    s"len=${payload.length}, final=$finalFragment, opcode=${opcodeOpt.getOrElse(-1)}, seq=$seq"
  }

  private def logVoiceEventSummary(message: String): F[Unit] =
    parse(message) match {
      case Right(json) =>
        val cursor = json.hcursor
        val op = cursor.get[Int]("op").getOrElse(-1)
        val seq = cursor.get[Int]("seq").toOption.fold("n/a")(_.toString)
        val detail = op match {
          case 2 =>
            val data = cursor.downField("d")
            val ssrc = data.get[Int]("ssrc").toOption.fold("n/a")(_.toString)
            val modeCount = data.get[List[String]]("modes").fold(_ => 0, _.size)
            s"ready ssrc=$ssrc modes=$modeCount"
          case 4 =>
            val data = cursor.downField("d")
            val mode = data.get[String]("mode").getOrElse("unknown")
            val secretKeyBytes = data.get[List[Int]]("secret_key").fold(_ => 0, _.size)
            val daveProtocol = data.get[Int]("dave_protocol_version").getOrElse(0)
            s"session_description mode=$mode secretKeyBytes=$secretKeyBytes dave=$daveProtocol"
          case 8 =>
            val heartbeat = cursor.downField("d").get[Int]("heartbeat_interval").toOption.fold("n/a")(_.toString)
            s"hello heartbeatMs=$heartbeat"
          case 11 =>
            val userCount = cursor.downField("d").get[List[String]]("user_ids").fold(_ => 0, _.size)
            s"dave_clients_connect users=$userCount"
          case 13 => "dave_client_disconnect"
          case 21 => "dave_prepare_transition"
          case 22 => "dave_execute_transition"
          case 24 => "dave_prepare_epoch"
          case 31 => "dave_invalid_commit_welcome"
          case other => s"op=$other"
        }

        Logger[F].debug(s"[VOICE] Received voice event op=$op seq=$seq $detail")
      case Left(error) =>
        Logger[F].debug(s"[VOICE] Received unparsable voice event: $error")
    }

  private def logKeyPackageSummary(keyPackage: Array[Byte]): F[Unit] =
    MlsMessages.parseKeyPackageMessage(keyPackage) match {
      case Right(parsed) =>
        val userIdPresent = parsed.leafNode.userId.nonEmpty
        val lifetimeNotBeforeZero = parsed.leafNode.lifetimeNotBefore.contains(0L)
        val lifetimeNotAfterMax = parsed.leafNode.lifetimeNotAfter.contains(-1L)
        val leafSignatureValid = MlsMessages.verifyLeafNodeSignature(parsed.leafNode)
        val keyPackageSignatureValid = MlsMessages.verifyKeyPackageSignature(parsed)
        Logger[F].info(
          s"[DAVE] MLS key package summary: userIdPresent=$userIdPresent, lifetimeNotBeforeZero=$lifetimeNotBeforeZero, lifetimeNotAfterMax=$lifetimeNotAfterMax, cipherSuite=${parsed.cipherSuite}, leafSignatureValid=$leafSignatureValid, keyPackageSignatureValid=$keyPackageSignatureValid, length=${keyPackage.length}B"
        )
      case Left(error) =>
        Logger[F].warn(s"[DAVE] MLS key package summary unavailable: ${error.message}, length=${keyPackage.length}B")
    }

  def closeVoiceConnection(reason: String = "unspecified"): F[Unit] = {
    shutdownSignalRef.get.flatMap {
      _.fold(
        Logger[F].debug("[VOICE] No active voice connection to close")
      )(signal =>
        Logger[F].info(s"[VOICE] Sending shutdown signal to close voice connection reason=$reason") >>
        isConnectedRef.set(false) >>
        udpSocketRef.getAndSet(None).flatMap(_.traverse_(socket => Async[F].blocking(socket.close()))) >>
        daveSessionManagerRef.get.flatMap(_.traverse_(manager => Async[F].blocking(manager.close()))) >>
        daveSessionManagerRef.set(None) >>
        daveProtocolVersionRef.set(0) >>
        signal.complete(()).void
      )
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
      _ <- waitForDaveMediaReady
      _ <- (socketOpt, ssrcOpt, ipOpt, portOpt, secretKeyOpt) match {
        case (Some(socket), Some(ssrcValue), Some(ip), Some(port), Some(secretKey)) =>
          val encryptionMode = encryptionModeOpt.getOrElse(EncryptionMode.AeadXChaCha20Poly1305RtpSize)
          Logger[F].info(s"[VOICE] Streaming audio from position ${startPosition}s...") >>
          Logger[F].debug("[VOICE] Stream URL redacted") >>
          daveAudioEncryptor(ssrcValue).flatMap { daveEncryptor =>
          startPlaying(ws) >>
          audioStreamer.streamAudioFromPosition(streamUrl, ws, socket, ssrcValue, ip, port, secretKey, startPosition, guildId, encryptionMode = encryptionMode.value, daveAudioEncryptor = daveEncryptor).handleErrorWith { error =>
            Logger[F].error(s"[VOICE] Streaming error: ${error.getMessage}") >>
            stopPlaying(ws) >>
             closeVoiceConnection(s"audio stream error from position ${startPosition}s")
          } >>
          stopPlaying(ws) >>
          Logger[F].info(s"[VOICE] Playback completed") >>
           closeVoiceConnection(s"audio stream completed from position ${startPosition}s")
          }
        case _ =>
          Logger[F].error(s"[VOICE] Voice connection not properly initialized")
      }
    } yield ()
  }
}
