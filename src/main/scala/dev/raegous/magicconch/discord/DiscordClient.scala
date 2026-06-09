package dev.raegous.magicconch.discord

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
import dev.raegous.magicconch.*
import dev.raegous.magicconch.audio.VoiceManager
import dev.raegous.magicconch.guilds.{GuildSettingsManager, GuildTracker}
import dev.raegous.magicconch.music.*

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
    for {
      heartbeatFiber <- Resource.eval(
        Ref.of[F, Option[Fiber[F, Throwable, Unit]]](None)
      )
      lastHeartbeatAck <- Resource.eval(
        Ref.of[F, Option[Long]](Some(System.currentTimeMillis()))
      )
      lastSeqRef <- Resource.eval(Ref.of[F, Option[Int]](None))
      guildSettings <- GuildSettingsManager.make[F]
      voiceManager <- VoiceManager
        .make[F](applicationId, backend, guildSettings)
      guildTracker <- GuildTracker.make[F]
      discordApi = new DiscordApiClient[F](token, backend)
      youtubeSearch = new YouTubeSearchClient[F](youtubeApiKey, httpClient)
      trackExtractor = dev.raegous.magicconch.music.TrackExtractor.make[F]
      commandRegistry = new commands.CommandRegistry[F](
        voiceManager,
        trackExtractor,
        youtubeSearch,
        guildSettings,
        discordApi,
        applicationId
      )
      messageHandler = new MessageHandler[F](discordApi, commandRegistry)
      slashCommandManager = new SlashCommandManager[F](
        token,
        applicationId,
        discordApi,
        commandRegistry
      )
      eventHandler <- GatewayEventHandler.make[F](
        token,
        applicationId,
        messageHandler,
        voiceManager,
        trackExtractor,
        slashCommandManager,
        discordApi,
        guildTracker,
        guildSettings,
        commandRegistry
      )
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
      heartbeatFiber,
      lastHeartbeatAck,
      lastSeqRef
    )
  }
}

class DiscordClient[F[_]: Async: Processes: Temporal] private (
    token: String,
    applicationId: String,
    youtubeApiKey: String,
    backend: WebSocketStreamBackend[F, ?],
    httpClient: org.http4s.client.Client[F],
    val voiceManager: VoiceManager[F], // Public for dashboard access
    val guildTracker: GuildTracker[F], // Public for dashboard access
    private val discordApi: DiscordApiClient[F],
    private val youtubeSearch: YouTubeSearchClient[F],
    private val commandRegistry: commands.CommandRegistry[F],
    private val messageHandler: MessageHandler[F],
    private val slashCommandManager: SlashCommandManager[F],
    private val eventHandler: GatewayEventHandler[F],
    private val heartbeatFiber: Ref[F, Option[
      Fiber[F, Throwable, Unit]
    ]], // Track heartbeat fiber for cancellation
    private val lastHeartbeatAck: Ref[F, Option[
      Long
    ]], // Timestamp of last heartbeat ACK (zombied connection detection)
    private val lastSeqRef: Ref[F, Option[Int]] // Last sequence number received
)(using Logger[F]) {

  private val gatewayUrl = "wss://gateway.discord.gg/?v=10&encoding=json"

  def connect: F[Unit] = {
    val request = basicRequest.get(uri"$gatewayUrl")

    backend
      .send(
        request.response(asWebSocket { (ws: WebSocket[F]) =>
          Logger[F].info("[GATEWAY] Connected to Discord Gateway") >>
            handleGatewayEvents(ws).guaranteeCase { outcome =>
              logGatewayEventLoopTermination(outcome) >> stopHeartbeat
            } // This blocks until error, keeping WebSocket alive
        })
      )
      .map(_.body)
      .flatMap(error =>
        Async[F].raiseError(
          new RuntimeException(s"WebSocket connection failed: $error")
        )
      )
  }

  private def handleGatewayEvents(ws: WebSocket[F]): F[Unit] = {
    def processGatewayEvent(message: String): F[Unit] = {
      for {
        _ <- parse(message) match {
          case Right(json) =>
            val op = json.hcursor.get[Int]("op").getOrElse(-1)
            val seq = json.hcursor.get[Int]("s").toOption
            val eventType = json.hcursor.get[String]("t").toOption
            val updateSeq =
              seq.fold(Async[F].unit)(s => lastSeqRef.set(Some(s)))
            val logMetadata = Logger[F].debug(
              s"[GATEWAY] Received opcode=$op event=${eventType.getOrElse("none")} seq=${seq.fold("none")(_.toString)}"
            )
            updateSeq >> logMetadata >>
              (op match {
                case 10 => // Hello - start heartbeat
                  val heartbeatInterval = json.hcursor
                    .downField("d")
                    .get[Int]("heartbeat_interval")
                    .getOrElse(-1)
                  Logger[F].info(
                    s"[GATEWAY] Received HELLO (heartbeat interval: ${heartbeatInterval}ms)"
                  ) >>
                    eventHandler.sendIdentify(ws) >>
                    startHeartbeat(ws, heartbeatInterval)

                case 11 => // Heartbeat ACK
                  lastHeartbeatAck.set(Some(System.currentTimeMillis())) >>
                    Logger[F].debug("[GATEWAY] Received heartbeat ACK")

                case 1 => // Heartbeat Request
                  Logger[F].info(
                    "[GATEWAY] Discord requested immediate heartbeat"
                  ) >>
                    sendHeartbeat(ws)

                case 0 => // Dispatch - delegate to event handler
                  decode[GatewayPayload](message) match {
                    case Right(payload) =>
                      Async[F]
                        .start(
                          eventHandler
                            .handleDispatchEvent(payload, ws)
                            .handleErrorWith { error =>
                              Logger[F].error(error)(
                                s"[GATEWAY] Dispatch handler failed event=${payload.t.getOrElse("none")} seq=${payload.s.fold("none")(_.toString)}"
                              ) >>
                                Async[F].pure(PayloadResult())
                            }
                        )
                        .void
                    case Left(error) =>
                      Logger[F].error(
                        s"[GATEWAY] Failed to decode payload: $error"
                      )
                  }

                case 9 => // Invalid Session
                  Logger[F].error(
                    "[GATEWAY] Invalid session (opcode 9) - will reconnect and re-identify"
                  ) >>
                    stopHeartbeat >>
                    voiceManager.abortActiveVoiceSessions(
                      "main Discord gateway invalid session"
                    ) >>
                    Async[F].raiseError(
                      new Exception("Invalid session (opcode 9)")
                    )

                case 7 => // Reconnect
                  Logger[F].warn(
                    "[GATEWAY] Discord requested reconnect (opcode 7)"
                  ) >>
                    stopHeartbeat >>
                    Async[F].raiseError(
                      new Exception("Discord requested reconnect (opcode 7)")
                    )

                case _ =>
                  Logger[F].debug(s"[GATEWAY] Unhandled opcode: $op")
              })
          case Left(error) =>
            Logger[F].error(
              s"[GATEWAY] Failed to parse gateway message: ${error.getClass.getSimpleName}"
            )
        }
      } yield ()
    }

    // Run event loop forever, propagating errors naturally
    (ws
      .receiveText()
      .flatMap(processGatewayEvent) >>
      Async[F].cede // Yield after processing each message
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
      _ <- ws.sendText(heartbeat.noSpaces)
    } yield ()
  }

  private def isWebSocketClosed(error: Throwable): Boolean = {
    val errorMsg = Option(error.getMessage).getOrElse("")
    errorMsg.contains("closed") || errorMsg.contains("Closed")
  }

  private def handleHeartbeatError(error: Throwable): F[Unit] = {
    val logAction = Option
      .when(isWebSocketClosed(error))(
        Logger[F].warn(s"[GATEWAY] Heartbeat stopped - WebSocket closed")
      )
      .getOrElse(
        Logger[F].error(s"[GATEWAY] Heartbeat failed after retries")
      )
    logAction >> Async[F].raiseError(error)
  }

  private def heartbeatStream(
      ws: WebSocket[F],
      interval: Int
  ): fs2.Stream[F, Unit] = {
    fs2.Stream
      .fixedRateStartImmediately[F](interval.millis)
      .evalMap { _ =>
        fs2.Stream
          .retry(
            fo = sendHeartbeat(ws),
            delay = 100.millis,
            nextDelay = _ * 2,
            maxAttempts = 3,
            retriable = error => !isWebSocketClosed(error)
          )
          .compile
          .drain
          .handleErrorWith(handleHeartbeatError)
      }
  }

  private def startHeartbeat(ws: WebSocket[F], interval: Int): F[Unit] = {
    stopHeartbeat >>
      Logger[F].info(
        s"[GATEWAY] Starting heartbeat loop (interval: ${interval}ms)"
      ) >>
      Async[F].uncancelable { poll =>
        poll(
          Async[F].start(
            heartbeatStream(ws, interval).compile.drain.guarantee(
              heartbeatFiber.set(None) >>
                Logger[F].debug("[GATEWAY] Heartbeat loop stopped")
            )
          )
        ).flatMap { fiber =>
          poll(heartbeatFiber.set(Some(fiber))).onCancel(fiber.cancel)
        }
      }
  }

  private def stopHeartbeat: F[Unit] = {
    heartbeatFiber
      .getAndSet(None)
      .flatMap(
        _.fold(Async[F].unit)(fiber =>
          Logger[F].info("[GATEWAY] Stopping heartbeat") >> fiber.cancel
        )
      )
  }

  private def logGatewayEventLoopTermination(
      outcome: Outcome[F, Throwable, Unit]
  ): F[Unit] =
    outcome match {
      case Outcome.Errored(error) =>
        Logger[F].error(
          s"[GATEWAY] handleGatewayEvents errored: ${error.getMessage}"
        )
      case Outcome.Canceled() =>
        Logger[F].warn("[GATEWAY] handleGatewayEvents canceled/finalized")
      case Outcome.Succeeded(_) =>
        Logger[F].warn("[GATEWAY] handleGatewayEvents completed unexpectedly")
    }
}
