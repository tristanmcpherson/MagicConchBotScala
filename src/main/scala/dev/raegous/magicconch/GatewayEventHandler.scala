package dev.raegous.magicconch

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import sttp.ws.WebSocket
import io.circe.parser.*
import io.circe.syntax.*
import DiscordModels.*
import DiscordModels.given

class GatewayEventHandler[F[_]: Async](
  token: String,
  messageHandler: MessageHandler[F], 
  voiceManager: VoiceManager[F],
  slashCommandManager: SlashCommandManager[F],
  discordApi: DiscordApiClient[F]
)(using Logger[F]) {
  
  private val botUserIdRef: Ref[F, Option[String]] = Ref.unsafe(None)
  private val gatewayWsRef: Ref[F, Option[sttp.ws.WebSocket[F]]] = Ref.unsafe(None)
  
  def handlePayload(payload: GatewayPayload, ws: WebSocket[F]): F[PayloadResult] = {
    gatewayWsRef.set(Some(ws)) >> // Store the WebSocket reference
      (payload.op match {
      case 10 => // Hello
        payload.d
          .flatMap(_.as[HelloPayload].toOption)
          .map(hello => sendIdentify(ws).as(PayloadResult(heartbeatInterval = Some(hello.heartbeat_interval))))
          .getOrElse(Async[F].pure(PayloadResult()))
      case 0 => // Dispatch
        handleDispatchEvent(payload, ws)
      case _ => Async[F].pure(PayloadResult())
    })
  }
  
  private def handleDispatchEvent(payload: GatewayPayload, ws: WebSocket[F]): F[PayloadResult] = {
    payload.t match {
      case Some("READY") =>
        payload.d
          .flatMap(_.as[ReadyPayload].toOption)
          .map(ready => 
            botUserIdRef.set(Some(ready.user.id)) >>
            voiceManager.setBotUserId(ready.user.id) >>
            Async[F].start(slashCommandManager.registerSlashCommands()).void >>
            Logger[F].info(s"Ready! Logged in as ${ready.user.username}")
              .as(PayloadResult(sessionId = Some(ready.session_id)))
          )
          .getOrElse(Async[F].pure(PayloadResult()))
      case Some("MESSAGE_CREATE") =>
        payload.d
          .flatMap(_.as[DiscordMessage].toOption)
          .filter(message => !message.author.bot.getOrElse(false))
          .map(messageHandler.handleMessage(_, ws).as(PayloadResult()))
          .getOrElse(Async[F].pure(PayloadResult()))
      case Some("VOICE_STATE_UPDATE") =>
        payload.d
          .flatMap(_.as[VoiceStateUpdate].toOption)
          .map(voiceManager.handleVoiceStateUpdate)
          .getOrElse(Async[F].pure(()))
          .as(PayloadResult())
      case Some("VOICE_SERVER_UPDATE") =>
        payload.d
          .flatMap(_.as[VoiceServerUpdate].toOption)
          .map(voiceManager.handleVoiceServerUpdate)
          .getOrElse(Async[F].pure(()))
          .as(PayloadResult())
      case Some("INTERACTION_CREATE") =>
        payload.d
          .flatMap(_.as[Interaction].toOption)
          .map(interaction => 
            gatewayWsRef.get.flatMap { wsOpt =>
              slashCommandManager.handleSlashCommand(interaction, wsOpt).flatMap { response =>
                discordApi.sendInteractionResponse(interaction.id, interaction.token, response.asJson.noSpaces)
              }
            }
          )
          .getOrElse(Async[F].pure(()))
          .as(PayloadResult())
      case t => Logger[F].info(s"Failed to process payload type $t") >> Async[F].pure(PayloadResult())
      case None => Async[F].pure(PayloadResult())
    }
  }
  
  private def sendIdentify(ws: WebSocket[F]): F[Unit] = {
    import io.circe.syntax.*
    
    val identify = IdentifyPayload(
      token = token,
      intents = DiscordIntents.BOT_DEFAULT,
      properties = Map(
        "$os" -> "linux",
        "$browser" -> "magicconch",
        "$device" -> "magicconch"
      )
    )
    
    val payload = GatewayPayload(
      op = 2,
      d = Some(identify.asJson),
      s = None,
      t = None
    )
    
    val payloadJson = payload.asJson.noSpaces
    Logger[F].info(s"Sending identify payload: $payloadJson") >>
    ws.sendText(payloadJson).handleErrorWith { error =>
      Logger[F].error(s"Failed to send identify: $error") >>
      Async[F].raiseError(error)
    }
  }
  
  private def requestGuildMembers(ws: WebSocket[F]): F[Unit] = {
    import io.circe.syntax.*
    
    val requestGuildMembers = Map(
      "op" -> 8.asJson, // REQUEST_GUILD_MEMBERS
      "d" -> Map(
        "guild_id" -> "644004109057261631".asJson, // Your guild ID
        "query" -> "".asJson, // Empty query returns all members
        "limit" -> 0.asJson, // 0 = no limit
        "presences" -> false.asJson,
        "user_ids" -> List.empty[String].asJson
      ).asJson
    ).asJson
    
    ws.sendText(requestGuildMembers.noSpaces) >>
    Logger[F].info("Requested guild members to get current voice states")
  }
  
  def getBotUserId: F[Option[String]] = botUserIdRef.get
}