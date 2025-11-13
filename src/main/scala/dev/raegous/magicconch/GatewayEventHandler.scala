package dev.raegous.magicconch

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import sttp.ws.WebSocket
import io.circe.parser.*
import io.circe.syntax.*
import io.circe.Printer
import DiscordModels.*
import DiscordModels.given

object GatewayEventHandler {
  def make[F[_]: Async](
    token: String,
    applicationId: String,
    messageHandler: MessageHandler[F],
    voiceManager: VoiceManager[F],
    slashCommandManager: SlashCommandManager[F],
    discordApi: DiscordApiClient[F],
    guildTracker: GuildTracker[F],
    guildSettings: GuildSettingsManager[F]
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
        slashCommandManager,
        discordApi,
        guildTracker,
        guildSettings,
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
  slashCommandManager: SlashCommandManager[F],
  discordApi: DiscordApiClient[F],
  guildTracker: GuildTracker[F],
  guildSettings: GuildSettingsManager[F],
  private val botUserIdRef: Ref[F, Option[String]],
  private val lastSeqRef: Ref[F, Option[Int]]
)(using Logger[F]) {

  // Custom printer that drops null values for Discord API compatibility
  private val jsonPrinter = Printer.noSpaces.copy(dropNullValues = true)

  def handlePayload(payload: GatewayPayload, ws: WebSocket[F]): F[PayloadResult] = {
    lastSeqRef.set(payload.s) >>
      (payload.op match {
      case 10 => // Hello
        payload.d
          .flatMap(_.as[HelloPayload].toOption)
          .map(hello => sendIdentify(ws).as(PayloadResult(heartbeatInterval = Some(hello.heartbeat_interval))))
          .getOrElse(Async[F].pure(PayloadResult()))
      case 11 => // Heartbeat ACK
        Logger[F].debug("Received heartbeat ACK from Discord") >>
        Async[F].pure(PayloadResult())
      case 1 => // Heartbeat Request (Discord asking us to heartbeat immediately)
        Logger[F].info("Discord requested immediate heartbeat") >>
        sendHeartbeat(ws).as(PayloadResult())
      case 0 => // Dispatch
        handleDispatchEvent(payload, ws)
      case 9 => // Invalid Session
        Logger[F].error("Invalid session (opcode 9) - Discord rejected our session, will reconnect and re-identify") >>
        Async[F].raiseError(new Exception("Invalid session (opcode 9)"))
      case 7 => // Reconnect
        Logger[F].warn("Discord requested reconnect (opcode 7) - will reconnect with exponential backoff") >>
        Async[F].raiseError(new Exception("Discord requested reconnect (opcode 7)"))
      case _ =>
        Logger[F].debug(s"Unhandled opcode: ${payload.op}") >>
        Async[F].pure(PayloadResult())
    })
  }

  private def sendHeartbeat(ws: WebSocket[F]): F[Unit] = {
    lastSeqRef.get.flatMap { seq =>
      val heartbeat = GatewayPayload(op = 1, d = seq.map(io.circe.Json.fromInt), s = None, t = None)
      val heartbeatJson = heartbeat.asJson.printWith(jsonPrinter)
      Logger[F].debug(s"Sending heartbeat with seq: $seq") >>
      ws.sendText(heartbeatJson).handleErrorWith { error =>
        Logger[F].error(s"Failed to send heartbeat: $error") >>
        Async[F].unit
      }
    }
  }

  // Public method for automatic heartbeats from DiscordClient
  def getLastSeq: F[Option[Int]] = lastSeqRef.get
  
  def handleDispatchEvent(payload: GatewayPayload, ws: WebSocket[F]): F[PayloadResult] = {
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
      case Some("GUILD_CREATE") =>
        (payload.d match {
          case Some(guildJson) =>
            guildJson.as[GuildCreate] match {
              case Right(guild) =>
                // Track this guild for the dashboard
                guildTracker.addGuild(guild.id, guild.name, guild.member_count) >>
                guild.voice_states.map { voiceStates =>
                  if (voiceStates.nonEmpty) {
                    voiceManager.populateVoiceStates(voiceStates)
                  } else {
                    Logger[F].warn(s"GUILD_CREATE for ${guild.name} has EMPTY voice_states array - Check Discord Developer Portal: Bot -> Privileged Gateway Intents -> 'Server Members Intent' must be ENABLED") >>
                    Async[F].unit
                  }
                }.getOrElse(
                  Logger[F].error(s"GUILD_CREATE for ${guild.name} is MISSING voice_states field entirely!") >>
                  Async[F].unit
                )
              case Left(error) =>
                Logger[F].error(s"Failed to parse GUILD_CREATE event: ${error.getMessage}") >>
                Logger[F].error(s"Decode failure history: ${error.history.mkString(" -> ")}")
            }
          case None =>
            Logger[F].error("GUILD_CREATE event missing data field")
        }).as(PayloadResult())
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
        Logger[F].info(s"Received INTERACTION_CREATE event") >>
        Logger[F].info(s"Raw interaction payload: ${payload.d.map(_.noSpaces).getOrElse("No data")}") >>
        (payload.d.flatMap(_.as[Interaction].toOption) match {
          case Some(interaction) =>
            Logger[F].info(s"Parsed interaction - type: ${interaction.`type`}, command: ${interaction.data.map(_.name).getOrElse("N/A")}") >>
            (interaction.`type` match {
              case 2 => // APPLICATION_COMMAND (slash commands)
                Logger[F].info(s"Handling slash command: ${interaction.data.map(_.name).getOrElse("unknown")}") >>
                slashCommandManager.handleSlashCommand(interaction, Some(ws)).flatMap { response =>
                  Logger[F].info(s"Got response from slash command handler, sending to Discord") >>
                  discordApi.sendInteractionResponse(interaction.id, interaction.token, response.asJson.printWith(jsonPrinter))
                }
              case 3 => // MESSAGE_COMPONENT (button clicks)
                handleMessageComponent(interaction, Some(ws))
              case _ =>
                Logger[F].warn(s"Unknown interaction type: ${interaction.`type`}")
            })
          case None =>
            Logger[F].error("Failed to parse INTERACTION_CREATE - interaction data could not be decoded") >>
            Async[F].pure(())
        }).as(PayloadResult())
      case Some("VOICE_CHANNEL_START_TIME_UPDATE") | Some("VOICE_CHANNEL_STATUS_UPDATE") =>
        // Ignore these voice channel events for now
        Async[F].pure(PayloadResult())
      case t => Logger[F].debug(s"Unhandled payload type: $t") >> Async[F].pure(PayloadResult())
    }
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
  private def parseCustomId(customId: String, expectedParts: Int): Option[List[String]] = {
    val parts = customId.split("_").toList
    Option.when(parts.length == expectedParts)(parts)
  }

  private def getUsername(interaction: Interaction): String = {
    interaction.member.flatMap(_.user.map(_.username))
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
      data = Some(InteractionResponseData(
        content = Some("❌ Search results expired. Please search again.")
      ))
    )
    discordApi.sendInteractionResponse(interaction.id, interaction.token, response.asJson.printWith(jsonPrinter))
  }

  // Extracted handler methods
  private def handleSearchSelect(customId: String, interaction: Interaction, wsOpt: Option[sttp.ws.WebSocket[F]]): F[Unit] = {
    parseCustomId(customId, 4).fold(
      Logger[F].error(s"Invalid custom_id format: $customId")
    ) { parts =>
      val userId = parts(2)
      val index = parts(3).toIntOption.getOrElse(0)

      voiceManager.getSearchResults(userId).flatMap { resultsOpt =>
        resultsOpt
          .flatMap(results => Option.when(index >= 0 && index < results.length)(results(index)))
          .traverse(selected => processSelectedTrack(selected, userId, interaction, wsOpt))
          .flatMap {
            case Some(_) => Async[F].unit
            case None => sendExpiredSearchResponse(interaction)
          }
      }
    }
  }

  private def processSelectedTrack(
    selected: SearchResult,
    userId: String,
    interaction: Interaction,
    wsOpt: Option[sttp.ws.WebSocket[F]]
  ): F[Unit] = {
    val guildId = interaction.guild_id.getOrElse("")
    val username = getUsername(interaction)

    for {
      _ <- Logger[F].info(s"[SEARCH] User selected: ${selected.title}")
      _ <- sendDeferredResponse(interaction)
      trackOpt <- voiceManager.extractAudioFromYoutube(selected.url)
      _ <- trackOpt.traverse { track =>
        addTrackAndAutoPlay(track.copy(requestedBy = username), guildId, userId, wsOpt)
      }.flatMap {
        case Some(_) =>
          voiceManager.clearSearchResults(userId) >>
            discordApi.editInteractionResponse(applicationId, interaction.token, s"Added to queue: **${trackOpt.map(_.title).getOrElse("")}**")
        case None =>
          discordApi.editInteractionResponse(applicationId, interaction.token, "Failed to extract audio from the selected video")
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
      _ <- voiceManager.addToQueue(guildId, track)
      _ <- Option.when(queueBefore.tracks.isEmpty && queueBefore.currentTrack.isEmpty)(())
        .traverse(_ => autoJoinAndPlay(guildId, userId, wsOpt))
        .void
    } yield ()
  }

  private def autoJoinAndPlay(
    guildId: String,
    userId: String,
    wsOpt: Option[sttp.ws.WebSocket[F]]
  ): F[Unit] = {
    wsOpt.flatTraverse { ws =>
      voiceManager.getUserVoiceChannel(userId).flatMap {
        _.traverse { channelId =>
          voiceManager.joinVoiceChannel(guildId, channelId, ws) >>
            voiceManager.waitForVoiceConnection(guildId) >>
            voiceManager.playNextTrack(guildId)
        }
      }
    }.flatTap {
      case None => Logger[F].warn(s"[SEARCH] User not in voice channel or no WebSocket")
      case _ => Async[F].unit
    }.void
  }

  private def handleVolumeControl(customId: String, interaction: Interaction): F[Unit] = {
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

  private def calculateNewVolume(action: String, currentVolume: Double): F[Double] = {
    action match {
      case "inc10" => Async[F].pure(Math.min(2.0, currentVolume + 0.10))
      case "dec10" => Async[F].pure(Math.max(0.0, currentVolume - 0.10))
      case levelStr =>
        levelStr.toIntOption match {
          case Some(level) if level >= 0 && level <= 200 => Async[F].pure(level / 100.0)
          case _ =>
            Logger[F].warn(s"[VOLUME] Invalid volume level: $levelStr") >>
              Async[F].pure(currentVolume)
        }
    }
  }

  private def sendVolumeUpdateResponse(interaction: Interaction, newVolume: Double): F[Unit] = {
    val currentPercent = (newVolume * 100).toInt
    val embed = MessageEmbed(
      title = Some("Volume Control"),
      description = Some(s"Current volume: **${currentPercent}%**\n\nUse the buttons below to adjust volume:"),
      color = Some(0x3498db),
      fields = Some(List(
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
      ))
    )

    val response = InteractionResponse(
      `type` = 7,
      data = Some(InteractionResponseData(
        content = Some(s"Current volume: ${currentPercent}%"),
        embeds = Some(List(embed)),
        components = interaction.message.flatMap(_.components)
      ))
    )
    discordApi.sendInteractionResponse(interaction.id, interaction.token, response.asJson.printWith(jsonPrinter))
  }

  private def handlePlayerPauseResume(customId: String, interaction: Interaction): F[Unit] = {
    parseCustomId(customId, 3).fold(
      Logger[F].error(s"Invalid player pause/resume custom_id format: $customId")
    ) { parts =>
      val action = parts(1)
      val guildId = parts(2)

      for {
        isPaused <- voiceManager.isPaused(guildId)
        _ <- (action, isPaused) match {
          case ("pause", false) => voiceManager.pausePlayback(guildId)
          case ("resume", true) => voiceManager.resumePlayback(guildId)
          case _ => Async[F].unit
        }
        newPaused <- voiceManager.isPaused(guildId)
        _ <- sendPauseResumeResponse(interaction, guildId, newPaused)
      } yield ()
    }
  }

  private def sendPauseResumeResponse(interaction: Interaction, guildId: String, isPaused: Boolean): F[Unit] = {
    val response = InteractionResponse(
      `type` = 7,
      data = Some(InteractionResponseData(
        content = Some(if (isPaused) "Playback paused" else "Playback resumed"),
        components = updatePauseResumeButtons(interaction, guildId, isPaused)
      ))
    )
    discordApi.sendInteractionResponse(interaction.id, interaction.token, response.asJson.printWith(jsonPrinter))
  }

  private def updatePauseResumeButtons(interaction: Interaction, guildId: String, isPaused: Boolean): Option[List[MessageActionRow]] = {
    interaction.message.flatMap(_.components).map { components =>
      components.map { row =>
        row.copy(components = row.components.map(_.map { button =>
          if (button.custom_id.exists(id => id.startsWith("player_pause_") || id.startsWith("player_resume_"))) {
            button.copy(
              label = Some(if (isPaused) "Resume" else "Pause"),
              custom_id = Some(s"player_${if (isPaused) "resume" else "pause"}_$guildId"),
              style = Some(if (isPaused) 3 else 1)
            )
          } else button
        }))
      }
    }
  }

  private def handlePlayerStop(customId: String, interaction: Interaction, wsOpt: Option[sttp.ws.WebSocket[F]]): F[Unit] = {
    parseCustomId(customId, 3).fold(
      Logger[F].error(s"Invalid player stop custom_id format: $customId")
    ) { parts =>
      val guildId = parts(2)

      for {
        _ <- voiceManager.stopPlayback(guildId)
        _ <- wsOpt.traverse(ws => voiceManager.leaveVoiceChannel(guildId, ws))
          .flatTap {
            case None => Logger[F].warn("[STOP] No gateway WebSocket available to leave voice channel")
            case _ => Async[F].unit
          }
        response = InteractionResponse(
          `type` = 7,
          data = Some(InteractionResponseData(
            content = Some("Playback stopped and queue cleared"),
            components = Some(List.empty)
          ))
        )
        _ <- discordApi.sendInteractionResponse(interaction.id, interaction.token, response.asJson.printWith(jsonPrinter))
      } yield ()
    }
  }

  private def handlePlayerSkip(customId: String, interaction: Interaction): F[Unit] = {
    parseCustomId(customId, 3).fold(
      Logger[F].error(s"Invalid player skip custom_id format: $customId")
    ) { parts =>
      val guildId = parts(2)

      for {
        queue <- voiceManager.getQueue(guildId)
        _ <- Option.when(queue.tracks.nonEmpty)(()).traverse(_ => voiceManager.skipTrack(guildId))
        newQueue <- voiceManager.getQueue(guildId)
        response = InteractionResponse(
          `type` = 7,
          data = Some(InteractionResponseData(
            content = Some(newQueue.currentTrack match {
              case Some(track) => s"Skipped! Now playing: **${track.title}**"
              case None => "Skipped! Queue is now empty"
            }),
            components = if (newQueue.currentTrack.isDefined) interaction.message.flatMap(_.components) else Some(List.empty)
          ))
        )
        _ <- discordApi.sendInteractionResponse(interaction.id, interaction.token, response.asJson.printWith(jsonPrinter))
      } yield ()
    }
  }

  private def handleMessageComponent(interaction: Interaction, wsOpt: Option[sttp.ws.WebSocket[F]]): F[Unit] = {
    interaction.data.flatMap(_.custom_id) match {
      case Some(customId) if customId.startsWith("search_select_") =>
        handleSearchSelect(customId, interaction, wsOpt)
      case Some(customId) if customId.startsWith("volume_") =>
        handleVolumeControl(customId, interaction)
      case Some(customId) if customId.startsWith("player_pause_") || customId.startsWith("player_resume_") =>
        handlePlayerPauseResume(customId, interaction)
      case Some(customId) if customId.startsWith("player_stop_") =>
        handlePlayerStop(customId, interaction, wsOpt)
      case Some(customId) if customId.startsWith("player_skip_") =>
        handlePlayerSkip(customId, interaction)
      case Some(customId) =>
        Logger[F].warn(s"Unknown custom_id: $customId")
      case None =>
        Logger[F].warn("Message component interaction missing custom_id")
    }
  }
}