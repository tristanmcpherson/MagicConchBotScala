package dev.raegous.magicconch.commands

import cats.effect.*
import cats.implicits.*
import cats.data.OptionT
import dev.raegous.magicconch.audio.VoiceManager
import org.typelevel.log4cats.Logger
import dev.raegous.magicconch.discord.MusicTrack

class SeekCommand[F[_]: Async](voiceManager: VoiceManager[F])(using Logger[F]) extends Command[F] {
  val name = "seek"
  val description = "Seek to a position in the current track"
  val arguments = List(
    CommandArgument("position", "Position (e.g., 90, 1:30, or 1:23:45)", required = true)
  )

  def execute(context: CommandContext[F]): F[CommandResult] = {
    context.args.get("position").fold(
      Async[F].pure(CommandResult(
        "Please provide a position (e.g., `/seek 90` or `/seek 1:30`)",
        isError = true
      ))
    )(positionStr => performSeek(positionStr, context.guildId))
  }

  private def performSeek(positionStr: String, guildId: String): F[CommandResult] = {
    parsePosition(positionStr)
      .filter(_ >= 0)
      .fold(
        handleInvalidPosition(positionStr)
      )(seconds => seekToPosition(seconds, guildId))
  }

  private def handleInvalidPosition(positionStr: String): F[CommandResult] = {
    parsePosition(positionStr).fold(
      Async[F].pure(CommandResult(
        s"Invalid position format. Use: `123` (seconds), `1:23` (minutes:seconds), or `1:23:45` (hours:minutes:seconds)",
        isError = true
      ))
    )(_ => Async[F].pure(CommandResult(
      "Position must be a positive number",
      isError = true
    )))
  }

  private def seekToPosition(seconds: Int, guildId: String): F[CommandResult] = {
    for {
      queue <- voiceManager.getQueue(guildId)
      result <- queue.currentTrack.fold(
        Async[F].pure(CommandResult(
          "No track currently playing",
          isError = true
        ))
      )(track => validateAndSeek(seconds, track, guildId))
    } yield result
  }

  private def validateAndSeek(seconds: Int, track: MusicTrack, guildId: String): F[CommandResult] = {
    val maxDuration = track.duration.getOrElse(Int.MaxValue)
    Option.when(seconds > maxDuration)(()).fold(
      voiceManager.seek(guildId, seconds) >>
      Async[F].pure(CommandResult(
        s"Seeking to ${formatTime(seconds)} in **${track.title}**"
      ))
    )(_ => Async[F].pure(CommandResult(
        s"Position ${formatTime(seconds)} is beyond track duration (${formatTime(maxDuration)})",
        isError = true
      ))
    )
  }

  /**
   * Parse position string to seconds.
   * Supports:
   * - "123" -> 123 seconds
   * - "1:30" -> 90 seconds (1 minute 30 seconds)
   * - "1:23:45" -> 5025 seconds (1 hour 23 minutes 45 seconds)
   */
  private def parsePosition(input: String): Option[Int] = {
    input.split(":").toList match {
      case seconds :: Nil =>
        // Just seconds
        seconds.toIntOption
      case minutes :: seconds :: Nil =>
        // Minutes:seconds
        for {
          m <- minutes.toIntOption
          s <- seconds.toIntOption if s < 60
        } yield m * 60 + s
      case hours :: minutes :: seconds :: Nil =>
        // Hours:minutes:seconds
        for {
          h <- hours.toIntOption
          m <- minutes.toIntOption if m < 60
          s <- seconds.toIntOption if s < 60
        } yield h * 3600 + m * 60 + s
      case _ =>
        None
    }
  }

  private def formatTime(seconds: Int): String = {
    val h = seconds / 3600
    val m = (seconds % 3600) / 60
    val s = seconds % 60
    Option.when(h > 0)(()).fold(
      f"$m:$s%02d"
    )(_ => f"$h:$m%02d:$s%02d")
  }
}
