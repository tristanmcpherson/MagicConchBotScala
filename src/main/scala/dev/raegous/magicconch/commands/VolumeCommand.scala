package dev.raegous.magicconch.commands

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import dev.raegous.magicconch.*
import dev.raegous.magicconch.discord.*
import dev.raegous.magicconch.guilds.GuildSettingsManager

class VolumeCommand[F[_]: Async](guildSettings: GuildSettingsManager[F])(using Logger[F]) extends Command[F] {

  val name = "volume"
  val description: String = "Adjust playback volume (0-200%)"
  val arguments = List(
    CommandArgument("level", "Volume level (0-200, or use buttons)")
  )

  def execute(context: CommandContext[F]): F[CommandResult] = {
    context.args.get("level").fold(
      showVolumeControls(context)
    )(levelStr => setVolumeLevel(levelStr, context.guildId))
  }

  private def setVolumeLevel(levelStr: String, guildId: String): F[CommandResult] = {
    levelStr.toIntOption
      .filter(level => level >= 0 && level <= 200)
      .fold(
        Async[F].pure(CommandResult(
          message = "Volume must be between 0 and 200",
          isError = true
        ))
      )(level => applyVolumeLevel(level, guildId))
  }

  private def applyVolumeLevel(level: Int, guildId: String): F[CommandResult] = {
    val volumeMultiplier = level / 100.0
    guildSettings.setVolume(guildId, volumeMultiplier).map { _ =>
      CommandResult(
        message = s"Volume set to **${level}%**",
        embeds = None,
        components = None
      )
    }
  }

  private def showVolumeControls(context: CommandContext[F]): F[CommandResult] = {
    guildSettings.getVolume(context.guildId).map { currentVolume =>
      val currentPercent = (currentVolume * 100).toInt
      buildVolumeControlResult(currentPercent, context.userId)
    }
  }

  private def buildVolumeControlResult(currentPercent: Int, userId: String): CommandResult = {
    val embed = MessageEmbed(
      title = Some("Player Controls"),
      description = Some(s"Current volume: **${currentPercent}%**\n\nUse the buttons below to control playback:"),
      color = Some(0x3498db),
      fields = Some(List(
        EmbedField(
          name = "Quick Presets",
          value = "0% (Mute) • 50% • 100% (Default) • 150% • 200% (Max)",
          inline = Some(false)
        ),
        EmbedField(
          name = "Fine Control",
          value = "-10% or +10% adjustments",
          inline = Some(false)
        )
      ))
    )

    val presetButtons = createPresetButtons(userId)
    val adjustButtons = createAdjustButtons(userId)

    val presetRow = MessageComponent(`type` = 1, components = Some(presetButtons))
    val adjustRow = MessageComponent(`type` = 1, components = Some(adjustButtons))

    CommandResult(
      message = s"Current volume: ${currentPercent}%",
      embeds = Some(List(embed)),
      components = Some(List(presetRow, adjustRow))
    )
  }

  private def createPresetButtons(userId: String): List[MessageComponent] = List(
    MessageComponent(`type` = 2, style = Some(4), label = Some("Mute"), custom_id = Some(s"volume_${userId}_0")),
    MessageComponent(`type` = 2, style = Some(2), label = Some("50%"), custom_id = Some(s"volume_${userId}_50")),
    MessageComponent(`type` = 2, style = Some(1), label = Some("100%"), custom_id = Some(s"volume_${userId}_100")),
    MessageComponent(`type` = 2, style = Some(2), label = Some("150%"), custom_id = Some(s"volume_${userId}_150")),
    MessageComponent(`type` = 2, style = Some(3), label = Some("200%"), custom_id = Some(s"volume_${userId}_200"))
  )

  private def createAdjustButtons(userId: String): List[MessageComponent] = List(
    MessageComponent(`type` = 2, style = Some(2), label = Some("-10%"), custom_id = Some(s"volume_${userId}_dec10")),
    MessageComponent(`type` = 2, style = Some(2), label = Some("+10%"), custom_id = Some(s"volume_${userId}_inc10"))
  )
}
