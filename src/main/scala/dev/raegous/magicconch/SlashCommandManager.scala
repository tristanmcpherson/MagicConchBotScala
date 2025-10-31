package dev.raegous.magicconch

import cats.effect.*
import org.typelevel.log4cats.Logger
import sttp.client4.*
import io.circe.syntax.*
import DiscordModels.*
import cats.implicits.*
import DiscordModels.given

class SlashCommandManager[F[_]: Async](token: String, applicationId: String, discordApi: DiscordApiClient[F], voiceManager: VoiceManager[F])(using Logger[F]) {
  
  private val guildId = "644004109057261631" // Test server ID
  
  def registerSlashCommands(): F[Unit] = {
    val desiredCommands = List(
      SlashCommand(
        id = "",
        name = "play",
        description = "Play music from a YouTube URL",
        options = Some(List(
          SlashCommandOption(
            name = "url",
            description = "YouTube URL to play",
            `type` = 3, // STRING type
            required = Some(true)
          )
        ))
      ),
      SlashCommand(
        id = "",
        name = "stop",
        description = "Stop the current music and clear queue"
      ),
      SlashCommand(
        id = "",
        name = "skip",
        description = "Skip the current track"
      ),
      SlashCommand(
        id = "",
        name = "queue",
        description = "Show the current music queue"
      ),
      SlashCommand(
        id = "",
        name = "join",
        description = "Join your current voice channel"
      ),
      SlashCommand(
        id = "",
        name = "leave",
        description = "Leave the voice channel"
      ),
      SlashCommand(
        id = "",
        name = "magicconch",
        description = "Ask the Magic Conch Shell a question",
        options = Some(List(
          SlashCommandOption(
            name = "question",
            description = "Your question for the Magic Conch",
            `type` = 3, // STRING type
            required = Some(false)
          )
        ))
      )
    )
    
    discordApi.getGuildSlashCommands(applicationId, guildId).flatMap { existingCommands =>
      val existingCommandNames = existingCommands.map(_.name).toSet
      val commandsToRegister = desiredCommands.filterNot(cmd => existingCommandNames.contains(cmd.name))
      
      if (commandsToRegister.nonEmpty) {
        Logger[F].info(s"Registering ${commandsToRegister.length} new commands: ${commandsToRegister.map(_.name).mkString(", ")}") >>
        registerCommands(commandsToRegister)
      } else {
        Logger[F].info("All commands already registered, skipping registration")
      }
    }
  }
  
  private def registerCommands(commands: List[SlashCommand]): F[Unit] = {
    commands.traverse_(registerCommand)
  }
  
  private def registerCommand(command: SlashCommand): F[Unit] = {
    val commandData = Map(
      "name" -> command.name.asJson,
      "description" -> command.description.asJson,
      "options" -> command.options.asJson
    ).asJson
    
    discordApi.registerGuildSlashCommand(applicationId, guildId, commandData.noSpaces).flatMap { response =>
      Logger[F].info(s"Registered guild slash command: ${command.name} - Response: $response")
    }
  }
  
  def handleSlashCommand(interaction: Interaction, gatewayWs: Option[sttp.ws.WebSocket[F]] = None): F[InteractionResponse] = {
    val commandName = interaction.data.map(_.name)
    
    commandName match {
      case Some("play") =>
        handlePlaySlashCommand(interaction)
      case Some("stop") =>
        interaction.guild_id match {
          case Some(guildId) =>
            voiceManager.stopMusic(guildId) >>
            voiceManager.clearQueue(guildId) >>
            Async[F].pure(InteractionResponse(
              `type` = 4,
              data = Some(InteractionResponseData(content = Some("🛑 Music stopped and queue cleared!")))
            ))
          case None =>
            Async[F].pure(InteractionResponse(
              `type` = 4,
              data = Some(InteractionResponseData(content = Some("❌ This command only works in a server!")))
            ))
        }
      case Some("skip") =>
        interaction.guild_id match {
          case Some(guildId) =>
            voiceManager.playNext(guildId).flatMap {
              case Some(nextTrack) =>
                voiceManager.startPlayingCurrent(guildId) >>
                Async[F].pure(InteractionResponse(
                  `type` = 4,
                  data = Some(InteractionResponseData(content = Some(s"⏭️ Skipped! Now playing: **${nextTrack.title}**")))
                ))
              case None =>
                Async[F].pure(InteractionResponse(
                  `type` = 4,
                  data = Some(InteractionResponseData(content = Some("❌ No more tracks in queue!")))
                ))
            }
          case None =>
            Async[F].pure(InteractionResponse(
              `type` = 4,
              data = Some(InteractionResponseData(content = Some("❌ This command only works in a server!")))
            ))
        }
      case Some("queue") =>
        interaction.guild_id match {
          case Some(guildId) =>
            voiceManager.getQueue(guildId).map { queue =>
              val queueText = queue.currentTrack match {
                case Some(current) =>
                  val upcoming = if (queue.tracks.isEmpty) {
                    "Empty"
                  } else {
                    queue.tracks.take(10).zipWithIndex.map { case (track, idx) =>
                      s"${idx + 1}. ${track.title} (requested by ${track.requestedBy})"
                    }.mkString("\n")
                  }
                  s"🎵 **Now Playing:** ${current.title}\n\n**Queue (${queue.tracks.length} tracks):**\n$upcoming"
                case None =>
                  if (queue.tracks.isEmpty) {
                    "🎵 Queue is empty. Use `/play <youtube-url>` to add songs!"
                  } else {
                    val upcoming = queue.tracks.take(10).zipWithIndex.map { case (track, idx) =>
                      s"${idx + 1}. ${track.title} (requested by ${track.requestedBy})"
                    }.mkString("\n")
                    s"🎵 **Queue (${queue.tracks.length} tracks):**\n$upcoming"
                  }
              }
              InteractionResponse(
                `type` = 4,
                data = Some(InteractionResponseData(content = Some(queueText)))
              )
            }
          case None =>
            Async[F].pure(InteractionResponse(
              `type` = 4,
              data = Some(InteractionResponseData(content = Some("❌ This command only works in a server!")))
            ))
        }
      case Some("join") =>
        handleJoinSlashCommand(interaction, gatewayWs)
      case Some("leave") =>
        (interaction.guild_id, gatewayWs) match {
          case (Some(guildId), Some(ws)) =>
            voiceManager.leaveVoiceChannel(guildId, ws) >>
            Async[F].pure(InteractionResponse(
              `type` = 4,
              data = Some(InteractionResponseData(content = Some("👋 Left voice channel!")))
            ))
          case (None, _) =>
            Async[F].pure(InteractionResponse(
              `type` = 4,
              data = Some(InteractionResponseData(content = Some("❌ This command only works in a server!")))
            ))
          case (_, None) =>
            Async[F].pure(InteractionResponse(
              `type` = 4,
              data = Some(InteractionResponseData(content = Some("❌ Gateway connection not available")))
            ))
        }
      case Some("magicconch") =>
        handleMagicConchSlashCommand(interaction)
      case _ =>
        Async[F].pure(InteractionResponse(
          `type` = 4,
          data = Some(InteractionResponseData(content = Some("Unknown command")))
        ))
    }
  }
  
  private def handlePlaySlashCommand(interaction: Interaction): F[InteractionResponse] = {
    interaction.guild_id match {
      case Some(guildId) =>
        val url = extractOptionValue(interaction, "url").getOrElse("")
        val requestedBy = interaction.member.flatMap(_.user.map(_.username))
          .orElse(interaction.user.map(_.username))
          .getOrElse("Unknown")

        if (url.contains("youtube.com") || url.contains("youtu.be")) {
          voiceManager.extractAudioFromYoutube(url).flatMap {
            case Some(track) =>
              val trackWithUser = track.copy(requestedBy = requestedBy)
              voiceManager.addToQueue(guildId, trackWithUser) >>
              Async[F].pure(InteractionResponse(
                `type` = 4,
                data = Some(InteractionResponseData(content = Some(s"🎵 Added to queue: **${track.title}**")))
              ))
            case None =>
              Async[F].pure(InteractionResponse(
                `type` = 4,
                data = Some(InteractionResponseData(content = Some("❌ Failed to extract audio from URL. Make sure it's a valid YouTube video!")))
              ))
          }
        } else {
          Async[F].pure(InteractionResponse(
            `type` = 4,
            data = Some(InteractionResponseData(content = Some("❌ Please provide a valid YouTube URL")))
          ))
        }
      case None =>
        Async[F].pure(InteractionResponse(
          `type` = 4,
          data = Some(InteractionResponseData(content = Some("❌ This command only works in a server!")))
        ))
    }
  }
  
  private def handleJoinSlashCommand(interaction: Interaction, gatewayWs: Option[sttp.ws.WebSocket[F]]): F[InteractionResponse] = {
    (gatewayWs, interaction.guild_id) match {
      case (Some(ws), Some(guildId)) =>
        val userId = interaction.member.flatMap(_.user.map(_.id))
          .orElse(interaction.user.map(_.id))
          .getOrElse("")

        Logger[F].info(s"Looking up voice channel for user $userId") >>
        voiceManager.getUserVoiceChannel(userId).flatMap {
          case Some(channelId) =>
            Logger[F].info(s"User $userId is in voice channel $channelId, joining...") >>
            voiceManager.joinVoiceChannel(guildId, channelId, ws) >>
            Async[F].pure(InteractionResponse(
              `type` = 4,
              data = Some(InteractionResponseData(content = Some(s"🔊 Joining your voice channel...")))
            ))
          case None =>
            Logger[F].info(s"No voice channel found for user $userId") >>
            Async[F].pure(InteractionResponse(
              `type` = 4,
              data = Some(InteractionResponseData(content = Some("❌ You need to be in a voice channel first!")))
            ))
        }
      case (None, _) =>
        Async[F].pure(InteractionResponse(
          `type` = 4,
          data = Some(InteractionResponseData(content = Some("❌ Gateway connection not available")))
        ))
      case (_, None) =>
        Async[F].pure(InteractionResponse(
          `type` = 4,
          data = Some(InteractionResponseData(content = Some("❌ This command only works in a server!")))
        ))
    }
  }

  private def handleMagicConchSlashCommand(interaction: Interaction): F[InteractionResponse] = {
    val responses = List(
      "Maybe someday.",
      "Nothing.",
      "Neither.", 
      "I don't think so.",
      "No.",
      "Yes.",
      "Try asking again."
    )
    val response = responses(scala.util.Random.nextInt(responses.length))
    Async[F].pure(InteractionResponse(
      `type` = 4,
      data = Some(InteractionResponseData(content = Some(s"🐚 The Magic Conch says: **$response**")))
    ))
  }
  
  private def extractOptionValue(interaction: Interaction, optionName: String): Option[String] = {
    for {
      data <- interaction.data
      options <- data.options
      option <- options.find(_.name == optionName)
      value <- option.value
    } yield value
  }
}