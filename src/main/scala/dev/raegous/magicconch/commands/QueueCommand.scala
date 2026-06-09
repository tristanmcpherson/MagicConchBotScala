package dev.raegous.magicconch.commands

import cats.effect.*
import cats.implicits.*
import dev.raegous.magicconch.*
import dev.raegous.magicconch.audio.VoiceManager
import dev.raegous.magicconch.discord.DiscordModels.*
import dev.raegous.magicconch.discord.*
import org.typelevel.log4cats.Logger

class QueueCommand[F[_]: Async](voiceManager: VoiceManager[F])(using Logger[F])
    extends Command[F] {
  val name = "queue"
  val description = "Show the current music queue"
  val arguments = List.empty

  def execute(context: CommandContext[F]): F[CommandResult] = {
    voiceManager.getQueue(context.guildId).map { queue =>
      val queueText = buildQueueText(queue)
      val components =
        queue.currentTrack.map(_ => buildQueueControls(queue, context.guildId))
      CommandResult(queueText, components = components)
    }
  }

  private def buildQueueText(queue: MusicQueue): String = {
    queue.currentTrack.fold(
      buildEmptyQueueText(queue.tracks)
    )(current => buildPlayingQueueText(current, queue.tracks))
  }

  private def buildEmptyQueueText(tracks: List[MusicTrack]): String = {
    Option
      .when(tracks.isEmpty)(())
      .fold(
        formatQueueList(tracks)
      )(_ => "Queue is empty. Use `/play <youtube-url>` to add songs!")
  }

  private def buildPlayingQueueText(
      current: MusicTrack,
      tracks: List[MusicTrack]
  ): String = {
    val upcoming = Option
      .when(tracks.isEmpty)(())
      .fold(
        formatTrackList(tracks)
      )(_ => "Empty")
    s"**Now Playing:** ${current.title}\n\n**Queue (${tracks.length} tracks):**\n$upcoming"
  }

  private def formatQueueList(tracks: List[MusicTrack]): String = {
    val upcoming = formatTrackList(tracks)
    s"**Queue (${tracks.length} tracks):**\n$upcoming"
  }

  private def formatTrackList(tracks: List[MusicTrack]): String = {
    tracks
      .take(10)
      .zipWithIndex
      .map { case (track, idx) =>
        s"${idx + 1}. ${track.title} (requested by ${track.requestedBy})"
      }
      .mkString("\n")
  }

  private def buildQueueControls(
      queue: MusicQueue,
      guildId: String
  ): List[MessageComponent] = {
    val hasQueue = queue.tracks.nonEmpty

    val pauseButton = MessageComponent(
      `type` = 2,
      style = Some(1),
      label = Some("Pause"),
      custom_id = Some(s"player_pause_$guildId")
    )

    val stopButton = MessageComponent(
      `type` = 2,
      style = Some(4),
      label = Some("Stop"),
      custom_id = Some(s"player_stop_$guildId")
    )

    val skipButton = MessageComponent(
      `type` = 2,
      style = Some(2),
      label = Some("Skip"),
      custom_id = Some(s"player_skip_$guildId"),
      disabled = Some(!hasQueue)
    )

    val actionRow = MessageComponent(
      `type` = 1,
      components = Some(List(pauseButton, stopButton, skipButton))
    )

    List(actionRow)
  }
}
