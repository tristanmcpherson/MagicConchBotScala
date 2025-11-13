package dev.raegous.magicconch.commands

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import dev.raegous.magicconch.{DiscordModels, MessageComponent, VoiceManager}
import DiscordModels.*

class QueueCommand[F[_]: Async](voiceManager: VoiceManager[F])(using Logger[F]) extends Command[F] {
  val name = "queue"
  val description = "Show the current music queue"
  val arguments = List.empty

  def execute(context: CommandContext[F]): F[CommandResult] = {
    voiceManager.getQueue(context.guildId).map { queue =>
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

      // Add control buttons if there's a track playing
      val components = queue.currentTrack.map { _ =>
        val hasQueue = queue.tracks.nonEmpty

        // Pause/Resume button (style 1 = Primary/Blue, 3 = Success/Green)
        val pauseButton = MessageComponent(
          `type` = 2, // Button
          style = Some(1), // Primary
          label = Some("⏸ Pause"),
          custom_id = Some(s"player_pause_${context.guildId}")
        )

        // Stop button (style 4 = Danger/Red)
        val stopButton = MessageComponent(
          `type` = 2,
          style = Some(4), // Danger
          label = Some("⏹ Stop"),
          custom_id = Some(s"player_stop_${context.guildId}")
        )

        // Skip button (style 2 = Secondary/Gray) - only if there's a queue
        val skipButton = MessageComponent(
          `type` = 2,
          style = Some(2), // Secondary
          label = Some("⏭ Skip"),
          custom_id = Some(s"player_skip_${context.guildId}"),
          disabled = Some(!hasQueue)
        )

        // Action row with buttons
        val actionRow = MessageComponent(
          `type` = 1, // Action Row
          components = Some(List(pauseButton, stopButton, skipButton))
        )

        List(actionRow)
      }

      CommandResult(queueText, components = components)
    }
  }
}
