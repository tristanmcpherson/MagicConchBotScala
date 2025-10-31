package dev.raegous.magicconch

import cats.effect.*
import cats.effect.std.Queue
import cats.syntax.all.*
import sttp.client4.*
import sttp.client4.httpclient.fs2.HttpClientFs2Backend
import sttp.ws.WebSocket
import fs2.Stream
import io.circe.parser.*
import io.circe.syntax.*
import org.typelevel.log4cats.Logger

import java.net.URI
import scala.concurrent.duration.*
import fs2.io.process.Processes
import sttp.client4.ws.async.*
import DiscordModels.*
import DiscordModels.given

case class PayloadResult(
  heartbeatInterval: Option[Int] = None,
  sessionId: Option[String] = None
)

class DiscordClient[F[_]: Async: Processes](token: String, applicationId: String, backend: WebSocketStreamBackend[F, ?])(using Logger[F]) {

  private val gatewayUrl = "wss://gateway.discord.gg/?v=10&encoding=json"
  
  private lazy val discordApi = new DiscordApiClient[F](token, backend)
  private lazy val voiceManager = new VoiceManager[F](applicationId, backend) // Will be set after READY
  private lazy val messageHandler = new MessageHandler[F](discordApi, voiceManager)
  private lazy val slashCommandManager = new SlashCommandManager[F](token, applicationId, discordApi, voiceManager)
  private lazy val eventHandler = new GatewayEventHandler[F](token, messageHandler, voiceManager, slashCommandManager, discordApi)
  
  def connect: F[Unit] = {
    val request = basicRequest.get(uri"$gatewayUrl")

    backend.send(
    request.response(asWebSocket { (ws: WebSocket[F]) =>
      Logger[F].info("Connected to Discord Gateway") >>
      processMessages(ws)
    })).map(_.body).flatMap(
      error => Async[F].raiseError(new RuntimeException(s"WebSocket connection failed: $error"))
    )
  }


  private def processMessages(ws: WebSocket[F]): F[Unit] = {
    Stream.repeatEval(ws.receiveText())
      .evalTap(message => Logger[F].info(s"Received: $message"))
      .map(decode[GatewayPayload])
      .evalMap {
        case Right(payload) => 
          eventHandler.handlePayload(payload, ws).flatMap { result =>
            if (result.heartbeatInterval.isDefined) {
              Logger[F].info("Starting heartbeat") >>
              startHeartbeat(ws, result.heartbeatInterval.getOrElse(41250))
            } else Async[F].unit
          }
        case Left(error) => 
          Logger[F].error(s"Failed to decode gateway payload: $error")
      }
      .compile
      .drain
      .handleErrorWith { error =>
        Logger[F].error(s"Message processing error: $error") >>
        Async[F].raiseError(error)
      }
  }
  
  
  private def startHeartbeat(ws: WebSocket[F], interval: Int): F[Unit] = {
    val heartbeatStream = Stream.fixedDelay[F](interval.millis)
      .evalMap { _ =>
        val heartbeat = GatewayPayload(op = 1, d = None, s = None, t = None)
        val heartbeatJson = heartbeat.asJson.noSpaces
        Logger[F].info(s"Sending heartbeat: $heartbeatJson") >>
        ws.sendText(heartbeatJson).handleErrorWith { error =>
          Logger[F].error(s"Failed to send heartbeat: $error") >>
          Async[F].unit // Don't fail the heartbeat loop on send errors
        }
      }
    
    Logger[F].info(s"Starting heartbeat with interval ${interval}ms") >>
    Async[F].start(heartbeatStream.compile.drain).void
  }
}