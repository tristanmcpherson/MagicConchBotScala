package dev.raegous.magicconch.commands

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import dev.raegous.magicconch.*
import dev.raegous.magicconch.DiscordModels.*

class VolumeCommand[F[_]: Async](guildSettings: GuildSettingsManager[F])(using Logger[F]) extends Command[F] {

  val name = "volume"
  val description: String = "Adjust playback volume (0-200%)"
  val arguments = List(
    CommandArgument("level", "Volume level (0-200, or use buttons)")
  )

  def execute(context: CommandContext[F]): F[CommandResult] = {
    context.args.get("level") match {
      case Some(levelStr) =>
        // Direct volume setting via argument
        levelStr.toIntOption match {
          case Some(level) if level >= 0 && level <= 200 =>
            val volumeMultiplier = level / 100.0
            guildSettings.setVolume(context.guildId, volumeMultiplier).map { _ =>
              CommandResult(
                message = s"🔊 Volume set to **${level}%**",
                embeds = None,
                components = None
              )
            }
          case _ =>
            Async[F].pure(CommandResult(
              message = "❌ Volume must be between 0 and 200",
              isError = true
            ))
        }

      case None =>
        // Show interactive volume controls
        guildSettings.getVolume(context.guildId).map { currentVolume =>
          val currentPercent = (currentVolume * 100).toInt

          // Create embed showing current volume
          val embed = MessageEmbed(
            title = Some("🔊 Volume Control"),
            description = Some(s"Current volume: **${currentPercent}%**\n\nUse the buttons below to adjust volume:"),
            color = Some(0x3498db), // Blue color
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

          // Create action row with volume buttons
          val presetButtons = List(
            MessageComponent(
              `type` = 2,
              style = Some(4), // Danger (red) for mute
              label = Some("Mute"),
              custom_id = Some(s"volume_${context.userId}_0")
            ),
            MessageComponent(
              `type` = 2,
              style = Some(2), // Secondary (gray)
              label = Some("50%"),
              custom_id = Some(s"volume_${context.userId}_50")
            ),
            MessageComponent(
              `type` = 2,
              style = Some(1), // Primary (blue) for default
              label = Some("100%"),
              custom_id = Some(s"volume_${context.userId}_100")
            ),
            MessageComponent(
              `type` = 2,
              style = Some(2), // Secondary
              label = Some("150%"),
              custom_id = Some(s"volume_${context.userId}_150")
            ),
            MessageComponent(
              `type` = 2,
              style = Some(3), // Success (green) for max
              label = Some("200%"),
              custom_id = Some(s"volume_${context.userId}_200")
            )
          )

          val adjustButtons = List(
            MessageComponent(
              `type` = 2,
              style = Some(2), // Secondary
              label = Some("-10%"),
              custom_id = Some(s"volume_${context.userId}_dec10")
            ),
            MessageComponent(
              `type` = 2,
              style = Some(2), // Secondary
              label = Some("+10%"),
              custom_id = Some(s"volume_${context.userId}_inc10")
            )
          )

          val presetRow = MessageComponent(
            `type` = 1, // Action Row
            components = Some(presetButtons)
          )

          val adjustRow = MessageComponent(
            `type` = 1, // Action Row
            components = Some(adjustButtons)
          )

          CommandResult(
            message = s"Current volume: ${currentPercent}%",
            embeds = Some(List(embed)),
            components = Some(List(presetRow, adjustRow))
          )
        }
    }
  }
}
