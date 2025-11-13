package dev.raegous.magicconch

import cats.effect.*
import cats.effect.implicits.*
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

object DiscordClient {
  def make[F[_]: Async: Processes](
    token: String,
    applicationId: String,
    youtubeApiKey: String,
    backend: WebSocketStreamBackend[F, ?],
    httpClient: org.http4s.client.Client[F]
  )(using Logger[F]): Resource[F, DiscordClient[F]] = {
    Resource.eval {
      for {
        heartbeatRunning <- Ref.of[F, Boolean](false)
        lastHeartbeatAck <- Ref.of[F, Option[Long]](Some(System.currentTimeMillis()))
        lastSeqRef <- Ref.of[F, Option[Int]](None)
        guildSettings <- GuildSettingsManager.make[F].allocated.map(_._1)
        voiceManager <- VoiceManager.make[F](applicationId, backend, guildSettings).allocated.map(_._1)
        guildTracker <- GuildTracker.make[F].allocated.map(_._1)
        discordApi = new DiscordApiClient[F](token, backend)
        youtubeSearch = new YouTubeSearchClient[F](youtubeApiKey, httpClient)
        commandRegistry = new commands.CommandRegistry[F](voiceManager, youtubeSearch, guildSettings)
        messageHandler = new MessageHandler[F](discordApi, commandRegistry)
        slashCommandManager = new SlashCommandManager[F](token, applicationId, discordApi, commandRegistry)
        eventHandler <- GatewayEventHandler.make[F](token, applicationId, messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings).allocated.map(_._1)
      } yield new DiscordClient[F](
        token,
        applicationId,
        youtubeApiKey,
        backend,
        httpClient,
        voiceManager,
        guildTracker,
        discordApi,
        youtubeSearch,
        commandRegistry,
        messageHandler,
        slashCommandManager,
        eventHandler,
        heartbeatRunning,
        lastHeartbeatAck,
        lastSeqRef
      )
    }
  }
}

class DiscordClient[F[_]: Async: Processes] private (
  token: String,
  applicationId: String,
  youtubeApiKey: String,
  backend: WebSocketStreamBackend[F, ?],
  httpClient: org.http4s.client.Client[F],
  val voiceManager: VoiceManager[F],  // Public for dashboard access
  val guildTracker: GuildTracker[F],   // Public for dashboard access
  private val discordApi: DiscordApiClient[F],
  private val youtubeSearch: YouTubeSearchClient[F],
  private val commandRegistry: commands.CommandRegistry[F],
  private val messageHandler: MessageHandler[F],
  private val slashCommandManager: SlashCommandManager[F],
  private val eventHandler: GatewayEventHandler[F],
  private val heartbeatRunning: Ref[F, Boolean],  // Track if heartbeat loop is running
  private val lastHeartbeatAck: Ref[F, Option[Long]],  // Timestamp of last heartbeat ACK (zombied connection detection)
  private val lastSeqRef: Ref[F, Option[Int]]  // Last sequence number received
)(using Logger[F]) {

  private val gatewayUrl = "wss://gateway.discord.gg/?v=10&encoding=json"
  
  def connect: F[Unit] = {
    val request = basicRequest.get(uri"$gatewayUrl")

    backend.send(
      request.response(asWebSocket { (ws: WebSocket[F]) =>
        Logger[F].info("[GATEWAY] Connected to Discord Gateway") >>
        handleGatewayEvents(ws)  // This blocks until error, keeping WebSocket alive
      })
    ).map(_.body).flatMap(
      error => Async[F].raiseError(new RuntimeException(s"WebSocket connection failed: $error"))
    )
  }

  private def handleGatewayEvents(ws: WebSocket[F]): F[Unit] = {
    def processGatewayEvent(message: String): F[Unit] = {
      Async[F].start {
        for {
          _ <- Logger[F].debug(s"[GATEWAY] Received: $message")
          _ <- parse(message) match {
            case Right(json) =>
              val op = json.hcursor.get[Int]("op").getOrElse(-1)
              val seq = json.hcursor.get[Int]("s").toOption
              // Only update sequence if present (only DISPATCH events have sequence numbers)
              val updateSeq = seq.fold(Async[F].unit)(s => lastSeqRef.set(Some(s)))
              updateSeq >>
              (op match {
                case 10 => // Hello - start heartbeat
                  val heartbeatInterval = json.hcursor.downField("d").get[Int]("heartbeat_interval").getOrElse(-1)
                  Logger[F].info(s"[GATEWAY] Received HELLO (heartbeat interval: ${heartbeatInterval}ms)") >>
                  eventHandler.sendIdentify(ws) >>
                  startHeartbeat(ws, heartbeatInterval)

                case 11 => // Heartbeat ACK
                  lastHeartbeatAck.set(Some(System.currentTimeMillis())) >>
                  Logger[F].debug("[GATEWAY] Received heartbeat ACK")

                case 1 => // Heartbeat Request
                  Logger[F].info("[GATEWAY] Discord requested immediate heartbeat") >>
                  sendHeartbeat(ws)

                case 0 => // Dispatch - delegate to event handler
                  decode[GatewayPayload](message) match {
                    case Right(payload) => eventHandler.handleDispatchEvent(payload, ws).void
                    case Left(error) => Logger[F].error(s"[GATEWAY] Failed to decode payload: $error")
                  }

                case 9 => // Invalid Session
                  Logger[F].error("[GATEWAY] Invalid session (opcode 9) - will reconnect and re-identify") >>
                  heartbeatRunning.set(false) >>
                  Async[F].raiseError(new Exception("Invalid session (opcode 9)"))

                case 7 => // Reconnect
                  Logger[F].warn("[GATEWAY] Discord requested reconnect (opcode 7)") >>
                  heartbeatRunning.set(false) >>
                  Async[F].raiseError(new Exception("Discord requested reconnect (opcode 7)"))

                case _ =>
                  Logger[F].debug(s"[GATEWAY] Unhandled opcode: $op")
              })
            case Left(error) =>
              Logger[F].error(s"[GATEWAY] Failed to parse message: $error")
          }
        } yield ()
      }.void
    }

    // Run event loop forever, propagating errors naturally
    (ws.receiveText()
      .flatMap(processGatewayEvent) >>
      Async[F].cede  // Yield after processing each message
    ).foreverM
  }
  
  
  private def sendHeartbeat(ws: WebSocket[F]): F[Unit] = {
    for {
      seq <- lastSeqRef.get
      heartbeat = Map(
        "op" -> 1.asJson,
        "d" -> seq.map(_.asJson).getOrElse(io.circe.Json.Null)
      ).asJson
      _ <- Logger[F].debug(s"[GATEWAY] Sending heartbeat with seq: $seq")
      _ <- Logger[F].debug(s"[GATEWAY] Heartbeat JSON: ${heartbeat.noSpaces}")
      _ <- ws.sendText(heartbeat.noSpaces)
    } yield ()
  }

  private def startHeartbeat(ws: WebSocket[F], interval: Int): F[Unit] = {
    // Check if heartbeat is already running
    heartbeatRunning.get.flatMap { isRunning =>
      if (isRunning) {
        Logger[F].debug("[GATEWAY] Heartbeat already running, skipping")
      } else {
        Logger[F].info(s"[GATEWAY] Starting heartbeat loop (interval: ${interval}ms)") >>
        heartbeatRunning.set(true) >>
        Async[F].start {
          fs2.Stream.fixedRateStartImmediately[F](interval.millis)
            .evalMap { _ =>
              heartbeatRunning.get.flatMap { stillRunning =>
                if (stillRunning) {
                  sendHeartbeat(ws).handleErrorWith { error =>
                    val errorMsg = Option(error.getMessage).getOrElse("unknown error")
                    if (errorMsg.contains("closed") || errorMsg.contains("Closed")) {
                      Logger[F].warn(s"[GATEWAY] Heartbeat stopped - WebSocket closed") >>
                      heartbeatRunning.set(false) >>
                      Async[F].raiseError(error)
                    } else {
                      Logger[F].warn(s"[GATEWAY] Failed to send heartbeat: $errorMsg")
                    }
                  }
                } else {
                  Logger[F].debug("[GATEWAY] Heartbeat loop exiting - marked as stopped") >>
                  Async[F].raiseError(new Exception("Heartbeat stopped"))
                }
              }
            }
            .compile
            .drain
            .handleErrorWith { error =>
              Logger[F].debug(s"[GATEWAY] Heartbeat loop stopped")
            }
            .guarantee(heartbeatRunning.set(false))
        }.void
      }
    }
  }
}