package dev.raegous.magicconch.discord

import cats.effect.*
import cats.implicits.*
import dev.raegous.magicconch.audio.VoiceManager
import dev.raegous.magicconch.discord.DiscordModels.given
import dev.raegous.magicconch.errors.*
import dev.raegous.magicconch.guilds.{GuildSettingsManager, GuildTracker}
import dev.raegous.magicconch.music.{TrackExtractor, YouTubeSearchResult}
import dev.raegous.magicconch.{MessageHandler, commands}
import io.circe.Printer
import io.circe.syntax.*
import org.typelevel.log4cats.Logger
import sttp.ws.WebSocket

import scala.concurrent.duration.*

object GatewayEventHandler {
  def make[F[_]: Async](
      token: String,
      applicationId: String,
      messageHandler: MessageHandler[F],
      voiceManager: VoiceManager[F],
      trackExtractor: TrackExtractor[F],
      slashCommandManager: SlashCommandManager[F],
      discordApi: DiscordApiClient[F],
      guildTracker: GuildTracker[F],
      guildSettings: GuildSettingsManager[F],
      commandRegistry: commands.CommandRegistry[F]
  )(using Logger[F]): Resource[F, GatewayEventHandler[F]] = {
    Resource.eval {
      for {
        botUserIdRef <- Ref.of[F, Option[String]](None)
        lastSeqRef <- Ref.of[F, Option[Int]](None)
      } yield new GatewayEventHandler[F](
        token,
        applicationId,
        messageHandler,
        voiceManager,
        trackExtractor,
        slashCommandManager,
        discordApi,
        guildTracker,
        guildSettings,
        commandRegistry,
        botUserIdRef,
        lastSeqRef
      )
    }
  }
}

class GatewayEventHandler[F[_]: Async] private (
    token: String,
    applicationId: String,
    messageHandler: MessageHandler[F],
    voiceManager: VoiceManager[F],
    trackExtractor: TrackExtractor[F],
    slashCommandManager: SlashCommandManager[F],
    discordApi: DiscordApiClient[F],
    guildTracker: GuildTracker[F],
    guildSettings: GuildSettingsManager[F],
    commandRegistry: commands.CommandRegistry[F],
    private val botUserIdRef: Ref[F, Option[String]],
    private val lastSeqRef: Ref[F, Option[Int]]
)(using Logger[F]) {

  // Custom printer that drops null values for Discord API compatibility
  private val jsonPrinter = Printer.noSpaces.copy(dropNullValues = true)

  def handlePayload(
      payload: GatewayPayload,
      ws: WebSocket[F]
  ): F[PayloadResult] = {
    lastSeqRef.set(payload.s) >>
      (payload.op match {
        case 10 => // Hello
          payload.d
            .flatMap(_.as[HelloPayload].toOption)
            .map(hello =>
              sendIdentify(ws).as(
                PayloadResult(heartbeatInterval =
                  Some(hello.heartbeat_interval)
                )
              )
            )
            .getOrElse(Async[F].pure(PayloadResult()))
        case 11 => // Heartbeat ACK
          Logger[F].debug("Received heartbeat ACK from Discord") >>
            Async[F].pure(PayloadResult())
        case 1 => // Heartbeat Request (Discord asking us to heartbeat immediately)
          Logger[F].info("Discord requested immediate heartbeat") >>
            sendHeartbeatWithRetry(ws).as(PayloadResult())
        case 0 => // Dispatch
          handleDispatchEvent(payload, ws)
        case 9 => // Invalid Session
          Logger[F].error(
            "Invalid session (opcode 9) - Discord rejected our session, will reconnect and re-identify"
          ) >>
            Async[F].raiseError(new Exception("Invalid session (opcode 9)"))
        case 7 => // Reconnect
          Logger[F].warn(
            "Discord requested reconnect (opcode 7) - will reconnect with exponential backoff"
          ) >>
            Async[F].raiseError(
              new Exception("Discord requested reconnect (opcode 7)")
            )
        case _ =>
          Logger[F].debug(s"Unhandled opcode: ${payload.op}") >>
            Async[F].pure(PayloadResult())
      })
  }

  private def sendHeartbeat(ws: WebSocket[F]): F[Unit] = {
    lastSeqRef.get.flatMap { seq =>
      val heartbeat = GatewayPayload(
        op = 1,
        d = seq.map(io.circe.Json.fromInt),
        s = None,
        t = None
      )
      val heartbeatJson = heartbeat.asJson.printWith(jsonPrinter)
      Logger[F].debug(s"Sending heartbeat with seq: $seq") >>
        ws.sendText(heartbeatJson)
    }
  }

  private def isWebSocketClosed(error: Throwable): Boolean = {
    val errorMsg = Option(error.getMessage).getOrElse("")
    errorMsg.contains("closed") || errorMsg.contains("Closed")
  }

  private def handleHeartbeatError(error: Throwable): F[Unit] = {
    val logAction = Option
      .when(isWebSocketClosed(error))(
        Logger[F].warn(s"Heartbeat failed - WebSocket closed")
      )
      .getOrElse(
        Logger[F].error(s"Heartbeat failed after retries")
      )
    logAction >> Async[F].raiseError(error)
  }

  // Used for immediate heartbeat requests from Discord (opcode 1)
  private def sendHeartbeatWithRetry(ws: WebSocket[F]): F[Unit] = {
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

  // Public method for automatic heartbeats from DiscordClient
  def getLastSeq: F[Option[Int]] = lastSeqRef.get

  def handleDispatchEvent(
      payload: GatewayPayload,
      ws: WebSocket[F]
  ): F[PayloadResult] = {
    payload.t match {
      case Some("READY") =>
        payload.d
          .flatMap(_.as[ReadyPayload].toOption)
          .map(ready =>
            botUserIdRef.set(Some(ready.user.id)) >>
              voiceManager.setBotUserId(ready.user.id) >>
              Async[F]
                .start(slashCommandManager.registerSlashCommands())
                .void >>
              Logger[F]
                .info(s"Ready! Logged in as ${ready.user.username}")
                .as(PayloadResult(sessionId = Some(ready.session_id)))
          )
          .getOrElse(Async[F].pure(PayloadResult()))
      case Some("GUILD_CREATE") =>
        handleGuildCreate(payload).as(PayloadResult())
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
        handleInteractionCreate(payload, ws).as(PayloadResult())
      case Some("VOICE_CHANNEL_START_TIME_UPDATE") | Some(
            "VOICE_CHANNEL_STATUS_UPDATE"
          ) =>
        // Ignore these voice channel events for now
        Async[F].pure(PayloadResult())
      case t =>
        Logger[F].debug(s"Unhandled payload type: $t") >> Async[F].pure(
          PayloadResult()
        )
    }
  }

  private def handleGuildCreate(payload: GatewayPayload): F[Unit] = {
    payload.d.fold(
      Logger[F].error("GUILD_CREATE event missing data field")
    )(guildJson =>
      guildJson
        .as[GuildCreate]
        .fold(
          error =>
            Logger[F].error(
              s"Failed to parse GUILD_CREATE event: ${error.getMessage}"
            ) >>
              Logger[F].error(
                s"Decode failure history: ${error.history.mkString(" -> ")}"
              ),
          guild => processGuildCreate(guild)
        )
    )
  }

  private def processGuildCreate(guild: GuildCreate): F[Unit] = {
    guildTracker.addGuild(guild.id, guild.name, guild.member_count) >>
      guild.voice_states.fold(
        Logger[F].error(
          s"GUILD_CREATE for ${guild.name} is MISSING voice_states field entirely!"
        ) >>
          Async[F].unit
      )(voiceStates => handleVoiceStates(voiceStates, guild.name))
  }

  private def handleVoiceStates(
      voiceStates: List[VoiceStateUpdate],
      guildName: String
  ): F[Unit] = {
    Option
      .when(voiceStates.nonEmpty)(())
      .fold(
        Logger[F].warn(
          s"GUILD_CREATE for $guildName has EMPTY voice_states array - Check Discord Developer Portal: Bot -> Privileged Gateway Intents -> 'Server Members Intent' must be ENABLED"
        ) >>
          Async[F].unit
      )(_ => voiceManager.populateVoiceStates(voiceStates))
  }

  private def handleInteractionCreate(
      payload: GatewayPayload,
      ws: WebSocket[F]
  ): F[Unit] = {
    payload.d.fold(
      Logger[F].error("INTERACTION_CREATE event missing data field")
    ) { interactionJson =>
      interactionJson
        .as[Interaction]
        .fold(
          _ =>
            Logger[F].error(
              "Failed to parse INTERACTION_CREATE - interaction data could not be decoded"
            ),
          interaction =>
            Logger[F].info(s"Received ${summarizeInteraction(interaction)}") >>
              processInteraction(interaction, ws)
        )
    }
  }

  private def processInteraction(
      interaction: Interaction,
      ws: WebSocket[F]
  ): F[Unit] = {
    Logger[F].info(s"Dispatching ${summarizeInteraction(interaction)}") >>
      (interaction.`type` match {
        case 2 => handleSlashCommandInteraction(interaction, ws)
        case 3 => handleMessageComponent(interaction, Some(ws))
        case _ =>
          Logger[F].warn(s"Unknown interaction type: ${interaction.`type`}")
      })
  }

  private def handleSlashCommandInteraction(
      interaction: Interaction,
      ws: WebSocket[F]
  ): F[Unit] = {
    val deferredAck = InteractionResponse(`type` = 5, data = None)
    Logger[F].info(s"Handling ${summarizeInteraction(interaction)}") >>
      discordApi.sendInteractionResponse(
        interaction.id,
        interaction.token,
        deferredAck.asJson.printWith(jsonPrinter)
      ) >>
      Async[F].start {
        slashCommandManager
          .handleSlashCommand(interaction, Some(ws))
          .flatMap { response =>
            val content = response.data.flatMap(_.content).getOrElse("")
            val embeds = response.data.flatMap(_.embeds)
            val components = response.data.flatMap(_.components)
            Logger[F].info(s"Command completed, editing deferred response") >>
              discordApi.editRichInteractionResponse(
                applicationId,
                interaction.token,
                content,
                embeds,
                components
              )
          }
          .handleErrorWith { error =>
            val msg =
              Option(error.getMessage).getOrElse(error.getClass.getSimpleName)
            Logger[F].error(s"[COMMAND] Slash command error: $msg") >>
              discordApi.editInteractionResponse(
                applicationId,
                interaction.token,
                s"An error occurred: $msg"
              )
          }
      }.void
  }

  private def summarizeInteraction(interaction: Interaction): String = {
    List(
      Some(s"interaction type=${interactionTypeLabel(interaction.`type`)}"),
      interaction.data.flatMap(_.name.map(name => s"command=$name")),
      interaction.data.flatMap(
        _.custom_id.map(customId => s"customId=$customId")
      ),
      interaction.guild_id.map(guildId => s"guildId=$guildId"),
      Some(s"channelId=${interaction.channel_id}")
    ).flatten.mkString(" ")
  }

  private def interactionTypeLabel(interactionType: Int): String =
    interactionType match {
      case 2     => "APPLICATION_COMMAND"
      case 3     => "MESSAGE_COMPONENT"
      case 5     => "MODAL_SUBMIT"
      case other => s"UNKNOWN($other)"
    }

  def sendIdentify(ws: WebSocket[F]): F[Unit] = {
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

    val payloadJson = payload.asJson.printWith(jsonPrinter)
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

  // Helper methods for interaction handling
  private def parseCustomId(
      customId: String,
      expectedParts: Int
  ): Option[List[String]] = {
    val parts = customId.split("_").toList
    Option.when(parts.length == expectedParts)(parts)
  }

  private def getUsername(interaction: Interaction): String = {
    interaction.member
      .flatMap(_.user.map(_.username))
      .orElse(interaction.user.map(_.username))
      .getOrElse("Unknown")
  }

  private def sendDeferredResponse(interaction: Interaction): F[Unit] = {
    val response = InteractionResponse(`type` = 5, data = None)
    discordApi.sendInteractionResponse(
      interaction.id,
      interaction.token,
      response.asJson.printWith(jsonPrinter)
    )
  }

  private def sendExpiredSearchResponse(interaction: Interaction): F[Unit] = {
    val response = InteractionResponse(
      `type` = 4,
      data = Some(
        InteractionResponseData(
          content = Some("❌ Search results expired. Please search again.")
        )
      )
    )
    discordApi.sendInteractionResponse(
      interaction.id,
      interaction.token,
      response.asJson.printWith(jsonPrinter)
    )
  }

  // Extracted handler methods
  private def handleSearchSelect(
      customId: String,
      interaction: Interaction,
      wsOpt: Option[sttp.ws.WebSocket[F]]
  ): F[Unit] = {
    parseCustomId(customId, 4).fold(
      Logger[F].error(s"Invalid custom_id format: $customId")
    ) { parts =>
      val userId = parts(2)
      val index = parts(3).toIntOption.getOrElse(0)

      voiceManager.getSearchResults(userId).flatMap { resultsOpt =>
        resultsOpt
          .flatMap(results =>
            Option.when(index >= 0 && index < results.length)(results(index))
          )
          .traverse(selected =>
            processSelectedTrack(selected, userId, interaction, wsOpt)
          )
          .flatMap {
            case Some(_) => Async[F].unit
            case None    => sendExpiredSearchResponse(interaction)
          }
      }
    }
  }

  private def processSelectedTrack(
      selected: YouTubeSearchResult,
      userId: String,
      interaction: Interaction,
      wsOpt: Option[sttp.ws.WebSocket[F]]
  ): F[Unit] = {
    val guildId = interaction.guild_id.getOrElse("")
    val username = getUsername(interaction)

    for {
      _ <- Logger[F].info(s"[SEARCH] User selected: ${selected.title}")
      _ <- sendDeferredResponse(interaction)
      trackOpt <- trackExtractor.extractTrackInfo(selected.url)
      _ <- trackOpt
        .traverse { track =>
          addTrackAndAutoPlay(
            track.copy(requestedBy = username),
            guildId,
            userId,
            wsOpt
          )
        }
        .flatMap {
          case Some(_) =>
            voiceManager.clearSearchResults(userId) >>
              discordApi.editInteractionResponse(
                applicationId,
                interaction.token,
                s"Added to queue: **${trackOpt.map(_.title).getOrElse("")}**"
              )
          case None =>
            discordApi.editInteractionResponse(
              applicationId,
              interaction.token,
              "Failed to extract audio from the selected video"
            )
        }
    } yield ()
  }

  private def addTrackAndAutoPlay(
      track: MusicTrack,
      guildId: String,
      userId: String,
      wsOpt: Option[sttp.ws.WebSocket[F]]
  ): F[Unit] = {
    for {
      queueBefore <- voiceManager.getQueue(guildId)
      addResult <- voiceManager.addToQueue(guildId, track)
      _ <- addResult.fold(
        error => Logger[F].warn(s"Failed to add track: ${error.message}"),
        _ =>
          Option
            .when(
              queueBefore.tracks.isEmpty && queueBefore.currentTrack.isEmpty
            )(())
            .traverse(_ => autoJoinAndPlay(guildId, userId, wsOpt))
            .void
      )
    } yield ()
  }

  private def autoJoinAndPlay(
      guildId: String,
      userId: String,
      wsOpt: Option[sttp.ws.WebSocket[F]]
  ): F[Unit] = {
    wsOpt
      .flatTraverse { ws =>
        voiceManager.getUserVoiceChannel(userId).flatMap {
          _.traverse { channelId =>
            voiceManager.joinVoiceChannel(guildId, channelId, ws) >>
              voiceManager.waitForVoiceConnection(guildId) >>
              voiceManager.playNextTrack(guildId)
          }
        }
      }
      .flatTap {
        case None =>
          Logger[F].warn(s"[SEARCH] User not in voice channel or no WebSocket")
        case _ => Async[F].unit
      }
      .void
  }

  private def handleVolumeControl(
      customId: String,
      interaction: Interaction
  ): F[Unit] = {
    parseCustomId(customId, 3).fold(
      Logger[F].error(s"Invalid volume custom_id format: $customId")
    ) { parts =>
      val userId = parts(1)
      val action = parts(2)
      val guildId = interaction.guild_id.getOrElse("")

      for {
        currentVolume <- guildSettings.getVolume(guildId)
        newVolume <- calculateNewVolume(action, currentVolume)
        _ <- guildSettings.setVolume(guildId, newVolume)
        _ <- sendVolumeUpdateResponse(interaction, newVolume)
      } yield ()
    }
  }

  private def calculateNewVolume(
      action: String,
      currentVolume: Double
  ): F[Double] = {
    action match {
      case "inc10"  => Async[F].pure(Math.min(2.0, currentVolume + 0.10))
      case "dec10"  => Async[F].pure(Math.max(0.0, currentVolume - 0.10))
      case levelStr =>
        levelStr.toIntOption match {
          case Some(level) if level >= 0 && level <= 200 =>
            Async[F].pure(level / 100.0)
          case _ =>
            Logger[F].warn(s"[VOLUME] Invalid volume level: $levelStr") >>
              Async[F].pure(currentVolume)
        }
    }
  }

  private def sendVolumeUpdateResponse(
      interaction: Interaction,
      newVolume: Double
  ): F[Unit] = {
    val currentPercent = (newVolume * 100).toInt
    val embed = MessageEmbed(
      title = Some("Player Controls"),
      description = Some(
        s"Current volume: **${currentPercent}%**\n\nUse the buttons below to control playback:"
      ),
      color = Some(0x3498db),
      fields = Some(
        List(
          EmbedField(
            name = "Quick Presets",
            value = "0% (Mute) - 50% - 100% (Default) - 150% - 200% (Max)",
            inline = Some(false)
          ),
          EmbedField(
            name = "Fine Control",
            value = "-10% or +10% adjustments",
            inline = Some(false)
          )
        )
      )
    )

    val response = InteractionResponse(
      `type` = 7,
      data = Some(
        InteractionResponseData(
          content = Some(s"Current volume: ${currentPercent}%"),
          embeds = Some(List(embed)),
          components = interaction.message.flatMap(_.components)
        )
      )
    )
    discordApi.sendInteractionResponse(
      interaction.id,
      interaction.token,
      response.asJson.printWith(jsonPrinter)
    )
  }

  private def handlePlayerPauseResume(
      customId: String,
      interaction: Interaction
  ): F[Unit] = {
    parseCustomId(customId, 3).fold(
      Logger[F].error(
        s"Invalid player pause/resume custom_id format: $customId"
      )
    ) { parts =>
      val action = parts(1)
      val guildId = parts(2)

      for {
        isPaused <- voiceManager.isPaused(guildId)
        _ <- (action, isPaused) match {
          case ("pause", false) => voiceManager.pausePlayback(guildId)
          case ("resume", true) => voiceManager.resumePlayback(guildId)
          case _                => Async[F].unit
        }
        newPaused <- voiceManager.isPaused(guildId)
        _ <- sendPauseResumeResponse(interaction, guildId, newPaused)
      } yield ()
    }
  }

  private def sendPauseResumeResponse(
      interaction: Interaction,
      guildId: String,
      isPaused: Boolean
  ): F[Unit] = {
    val message =
      Option.when(isPaused)(()).fold("Playback resumed")(_ => "Playback paused")
    val response = InteractionResponse(
      `type` = 7,
      data = Some(
        InteractionResponseData(
          content = Some(message),
          components = updatePauseResumeButtons(interaction, guildId, isPaused)
        )
      )
    )
    discordApi.sendInteractionResponse(
      interaction.id,
      interaction.token,
      response.asJson.printWith(jsonPrinter)
    )
  }

  private def updatePauseResumeButtons(
      interaction: Interaction,
      guildId: String,
      isPaused: Boolean
  ): Option[List[MessageComponent]] = {
    interaction.message.flatMap(_.components).map { components =>
      components.map { row =>
        row.copy(components = row.components.map(_.map { button =>
          Option
            .when(
              button.custom_id.exists(id =>
                id.startsWith("player_pause_") || id
                  .startsWith("player_resume_")
              )
            )(())
            .fold(
              button
            )(_ => updatePauseResumeButton(button, guildId, isPaused))
        }))
      }
    }
  }

  private def updatePauseResumeButton(
      button: MessageComponent,
      guildId: String,
      isPaused: Boolean
  ): MessageComponent = {
    val label = Option.when(isPaused)(()).fold("Pause")(_ => "Resume")
    val action = Option.when(isPaused)(()).fold("pause")(_ => "resume")
    val style = Option.when(isPaused)(()).fold(1)(_ => 3)
    button.copy(
      label = Some(label),
      custom_id = Some(s"player_${action}_$guildId"),
      style = Some(style)
    )
  }

  private def handlePlayerStop(
      customId: String,
      interaction: Interaction,
      wsOpt: Option[sttp.ws.WebSocket[F]]
  ): F[Unit] = {
    parseCustomId(customId, 3).fold(
      Logger[F].error(s"Invalid player stop custom_id format: $customId")
    ) { parts =>
      val guildId = parts(2)

      for {
        _ <- voiceManager.stopPlayback(guildId)
        _ <- wsOpt
          .traverse(ws => voiceManager.leaveVoiceChannel(guildId, ws))
          .flatTap {
            case None =>
              Logger[F].warn(
                "[STOP] No gateway WebSocket available to leave voice channel"
              )
            case _ => Async[F].unit
          }
        response = InteractionResponse(
          `type` = 7,
          data = Some(
            InteractionResponseData(
              content = Some("Playback stopped and queue cleared"),
              components = Some(List.empty)
            )
          )
        )
        _ <- discordApi.sendInteractionResponse(
          interaction.id,
          interaction.token,
          response.asJson.printWith(jsonPrinter)
        )
      } yield ()
    }
  }

  private def handlePlayerSkip(
      customId: String,
      interaction: Interaction
  ): F[Unit] = {
    parseCustomId(customId, 3).fold(
      Logger[F].error(s"Invalid player skip custom_id format: $customId")
    ) { parts =>
      val guildId = parts(2)

      for {
        queue <- voiceManager.getQueue(guildId)
        _ <- Option
          .when(queue.tracks.nonEmpty)(())
          .traverse(_ => voiceManager.skipTrack(guildId))
        newQueue <- voiceManager.getQueue(guildId)
        message = newQueue.currentTrack.fold("Skipped! Queue is now empty")(
          track => s"Skipped! Now playing: **${track.title}**"
        )
        components = newQueue.currentTrack.fold(
          Some(List.empty[MessageComponent])
        )(_ => interaction.message.flatMap(_.components))
        response = InteractionResponse(
          `type` = 7,
          data = Some(
            InteractionResponseData(
              content = Some(message),
              components = components
            )
          )
        )
        _ <- discordApi.sendInteractionResponse(
          interaction.id,
          interaction.token,
          response.asJson.printWith(jsonPrinter)
        )
      } yield ()
    }
  }

  /** Handle MESSAGE_COMPONENT interactions (button clicks, select menus)
    *
    * Delegates to the InteractionRouter which routes to appropriate handlers.
    */
  private def handleMessageComponent(
      interaction: Interaction,
      wsOpt: Option[sttp.ws.WebSocket[F]]
  ): F[Unit] = {
    commandRegistry.interactionRouter.route(interaction, wsOpt)
  }
}
