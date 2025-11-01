package dev.raegous.magicconch

import cats.effect.*
import cats.effect.std.Queue
import cats.implicits.*
import org.typelevel.log4cats.Logger
import sttp.client4.*
import sttp.ws.WebSocket
import io.circe.parser.*
import io.circe.syntax.*
import DiscordModels.*
import sttp.client4.ws.async.asWebSocket

import java.net.DatagramSocket
import java.net.InetSocketAddress

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
class VoiceGateway[F[_]: Async: fs2.io.process.Processes](
  backend: WebSocketStreamBackend[F, ?],
  audioStreamer: AudioStreamer[F]
)(using Logger[F]) {

  private var udpSocket: Option[DatagramSocket] = None
  private var ssrc: Option[Int] = None
  
  def connectToVoiceGateway(
    endpoint: String,
    guildId: String,
    userId: String,
    sessionId: String,
    token: String
  ): F[WebSocket[F]] = {
    val voiceGatewayUrl = s"wss://${endpoint.replace(":80", "")}/?v=4"
    val request = basicRequest.get(uri"$voiceGatewayUrl")
    
    backend.send(
      request.response(asWebSocket { (ws: WebSocket[F]) =>
        for {
          _ <- Logger[F].info(s"Connected to voice gateway: $voiceGatewayUrl")
          _ <- sendVoiceIdentify(ws, guildId, userId, sessionId, token)
          _ <- handleVoiceEvents(ws)
        } yield ws
      })
    ).map(_.body).flatMap {
      case Right(ws) => Async[F].pure(ws)
      case Left(error) => Async[F].raiseError(new RuntimeException(s"Voice WebSocket connection failed: $error"))
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
    Logger[F].info("Sent voice identify payload")
  }
  
  private def handleVoiceEvents(ws: WebSocket[F]): F[Unit] = {
    def processVoiceEvent(message: String): F[Unit] = {
      parse(message) match {
        case Right(json) =>
          val op = json.hcursor.get[Int]("op").getOrElse(-1)
          op match {
            case 2 => handleReady(json, ws)
            case 4 => handleSessionDescription(json)
            case 8 => sendHeartbeat(ws)
            case _ => Logger[F].debug(s"Unhandled voice event: op=$op")
          }
        case Left(error) =>
          Logger[F].error(s"Failed to parse voice event: $error")
      }
    }
    
    ws.receiveText().flatMap(processVoiceEvent).foreverM
  }
  
  private def handleReady(json: io.circe.Json, ws: WebSocket[F]): F[Unit] = {
    val data = json.hcursor.downField("d")
    val extractedSsrc = data.get[Int]("ssrc").getOrElse(0)
    val ip = data.get[String]("ip").getOrElse("")
    val port = data.get[Int]("port").getOrElse(0)
    
    ssrc = Some(extractedSsrc)
    
    Logger[F].info(s"Voice ready: SSRC=$extractedSsrc, IP=$ip, Port=$port") >>
    performIPDiscovery(ws, ip, port, extractedSsrc)
  }
  
  private def performIPDiscovery(ws: WebSocket[F], ip: String, port: Int, ssrc: Int): F[Unit] = {
    for {
      socket <- Async[F].blocking {
        val socket = new DatagramSocket()
        udpSocket = Some(socket)
        socket
      }
      _ <- Logger[F].info("Performing IP discovery...")
      // IP discovery packet creation and handling would go here
      _ <- sendSelectProtocol(ws, ip, port)
    } yield ()
  }
  
  private def sendSelectProtocol(ws: WebSocket[F], ip: String, port: Int): F[Unit] = {
    val selectProtocol = Map(
      "op" -> 1.asJson,
      "d" -> Map(
        "protocol" -> "udp".asJson,
        "data" -> Map(
          "address" -> ip.asJson,
          "port" -> port.asJson,
          "mode" -> "xsalsa20_poly1305".asJson
        ).asJson
      ).asJson
    ).asJson
    
    ws.sendText(selectProtocol.noSpaces) >>
    Logger[F].info("Sent select protocol")
  }
  
  private def handleSessionDescription(json: io.circe.Json): F[Unit] = {
    Logger[F].info("Received session description - voice connection ready for audio streaming")
  }
  
  private def sendHeartbeat(ws: WebSocket[F]): F[Unit] = {
    val heartbeat = Map(
      "op" -> 3.asJson,
      "d" -> System.currentTimeMillis().asJson
    ).asJson
    
    ws.sendText(heartbeat.noSpaces)
  }
  
  def startPlaying(ws: WebSocket[F]): F[Unit] = {
    val speaking = Map(
      "op" -> 5.asJson,
      "d" -> Map(
        "speaking" -> 1.asJson,
        "delay" -> 0.asJson,
        "ssrc" -> ssrc.getOrElse(0).asJson
      ).asJson
    ).asJson
    
    ws.sendText(speaking.noSpaces) >>
    Logger[F].info("Started speaking")
  }
  
  def stopPlaying(ws: WebSocket[F]): F[Unit] = {
    val speaking = Map(
      "op" -> 5.asJson,
      "d" -> Map(
        "speaking" -> 0.asJson,
        "delay" -> 0.asJson,
        "ssrc" -> ssrc.getOrElse(0).asJson
      ).asJson
    ).asJson
    
    ws.sendText(speaking.noSpaces) >>
    Logger[F].info("Stopped speaking")
  }
  
  def streamAudio(streamUrl: String, ws: WebSocket[F]): F[Unit] = {
    (udpSocket, ssrc) match {
      case (Some(socket), Some(ssrcValue)) =>
        startPlaying(ws) >>
        audioStreamer.streamAudio(streamUrl, ws, socket, ssrcValue) >>
        stopPlaying(ws)
      case _ =>
        Logger[F].error("Voice connection not properly initialized")
    }
  }
}