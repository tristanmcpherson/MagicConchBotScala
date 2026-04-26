package dev.raegous.magicconch.commands

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import dev.raegous.magicconch.*
import dev.raegous.magicconch.audio.VoiceManager
import dev.raegous.magicconch.discord.*
import dev.raegous.magicconch.discord.DiscordModels.given
import dev.raegous.magicconch.guilds.GuildSettingsManager
import io.circe.Printer
import io.circe.syntax.*
import sttp.ws.WebSocket

class PlayerInteractionHandler[F[_]: Async](
  voiceManager: VoiceManager[F],
  discordApi: DiscordApiClient[F],
  guildSettings: GuildSettingsManager[F]
)(using Logger[F]) extends ComponentInteractionHandler[F] {

  private val jsonPrinter = Printer.noSpaces.copy(dropNullValues = true)

  override def canHandle(customId: String): Boolean = {
    customId.startsWith("volume_") ||
    customId.startsWith("player_pause_") ||
    customId.startsWith("player_resume_") ||
    customId.startsWith("player_stop_") ||
    customId.startsWith("player_skip_")
  }

  override def handle(
    customId: String,
    interaction: Interaction,
    wsOpt: Option[WebSocket[F]]
  ): F[Unit] = {
    customId match {
      case id if id.startsWith("volume_") => handleVolumeControl(id, interaction)
      case id if id.startsWith("player_pause_") || id.startsWith("player_resume_") =>
        handlePlayerPauseResume(id, interaction)
      case id if id.startsWith("player_stop_") => handlePlayerStop(id, interaction, wsOpt)
      case id if id.startsWith("player_skip_") => handlePlayerSkip(id, interaction)
      case _ => Logger[F].warn(s"[PLAYER] Unknown custom_id: $customId")
    }
  }

  // Volume Control
  private def handleVolumeControl(customId: String, interaction: Interaction): F[Unit] = {
    parseCustomId(customId, 3).fold(
      Logger[F].error(s"[PLAYER] Invalid volume custom_id format: $customId")
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
            Logger[F].warn(s"[PLAYER] Invalid volume level: $levelStr") >>
              Async[F].pure(currentVolume)
        }
    }
  }

  private def sendVolumeUpdateResponse(interaction: Interaction, newVolume: Double): F[Unit] = {
    val currentPercent = (newVolume * 100).toInt
    val embed = MessageEmbed(
      title = Some("Player Controls"),
      description = Some(s"Current volume: **${currentPercent}%**\n\nUse the buttons below to control playback:"),
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
      `type` = 7, // UPDATE_MESSAGE
      data = Some(InteractionResponseData(
        content = Some(s"Current volume: ${currentPercent}%"),
        embeds = Some(List(embed)),
        components = interaction.message.flatMap(_.components)
      ))
    )
    discordApi.sendInteractionResponse(interaction.id, interaction.token, response.asJson.printWith(jsonPrinter))
  }

  // Pause/Resume
  private def handlePlayerPauseResume(customId: String, interaction: Interaction): F[Unit] = {
    parseCustomId(customId, 3).fold(
      Logger[F].error(s"[PLAYER] Invalid player pause/resume custom_id format: $customId")
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
      `type` = 7, // UPDATE_MESSAGE
      data = Some(InteractionResponseData(
        content = Some(Option.when(isPaused)("Playback paused").getOrElse("Playback resumed")),
        components = updatePauseResumeButtons(interaction, guildId, isPaused)
      ))
    )
    discordApi.sendInteractionResponse(interaction.id, interaction.token, response.asJson.printWith(jsonPrinter))
  }

  private def updatePauseResumeButtons(interaction: Interaction, guildId: String, isPaused: Boolean): Option[List[MessageComponent]] = {
    interaction.message.flatMap(_.components).map { components =>
      components.map { row =>
        row.copy(components = row.components.map(_.map { button =>
          Option.when(button.custom_id.exists(id => id.startsWith("player_pause_") || id.startsWith("player_resume_")))(
            button.copy(
              label = Some(Option.when(isPaused)("Resume").getOrElse("Pause")),
              custom_id = Some(s"player_${Option.when(isPaused)("resume").getOrElse("pause")}_$guildId"),
              style = Some(Option.when(isPaused)(3).getOrElse(1))
            )
          ).getOrElse(button)
        }))
      }
    }
  }

  // Stop
  private def handlePlayerStop(customId: String, interaction: Interaction, wsOpt: Option[WebSocket[F]]): F[Unit] = {
    parseCustomId(customId, 3).fold(
      Logger[F].error(s"[PLAYER] Invalid player stop custom_id format: $customId")
    ) { parts =>
      val guildId = parts(2)

      for {
        _ <- voiceManager.stopPlayback(guildId)
        _ <- wsOpt.traverse(ws => voiceManager.leaveVoiceChannel(guildId, ws))
          .flatTap {
            case None => Logger[F].warn("[PLAYER] No gateway WebSocket available to leave voice channel")
            case _ => Async[F].unit
          }
        response = InteractionResponse(
          `type` = 7, // UPDATE_MESSAGE
          data = Some(InteractionResponseData(
            content = Some("Playback stopped and queue cleared"),
            components = Some(List.empty)
          ))
        )
        _ <- discordApi.sendInteractionResponse(interaction.id, interaction.token, response.asJson.printWith(jsonPrinter))
      } yield ()
    }
  }

  // Skip
  private def handlePlayerSkip(customId: String, interaction: Interaction): F[Unit] = {
    parseCustomId(customId, 3).fold(
      Logger[F].error(s"[PLAYER] Invalid player skip custom_id format: $customId")
    ) { parts =>
      val guildId = parts(2)

      for {
        queue <- voiceManager.getQueue(guildId)
        _ <- Option.when(queue.tracks.nonEmpty)(()).traverse(_ => voiceManager.skipTrack(guildId))
        newQueue <- voiceManager.getQueue(guildId)
        response = InteractionResponse(
          `type` = 7, // UPDATE_MESSAGE
          data = Some(InteractionResponseData(
            content = Some(newQueue.currentTrack.fold("Skipped! Queue is now empty")(track => s"Skipped! Now playing: **${track.title}**")),
            components = newQueue.currentTrack.fold(Some(List.empty[MessageComponent]))(_ => interaction.message.flatMap(_.components))
          ))
        )
        _ <- discordApi.sendInteractionResponse(interaction.id, interaction.token, response.asJson.printWith(jsonPrinter))
      } yield ()
    }
  }

  // Utilities
  private def parseCustomId(customId: String, expectedParts: Int): Option[Array[String]] = {
    val parts = customId.split("_")
    Option.when(parts.length >= expectedParts)(parts)
  }
}
