package dev.raegous.magicconch.commands

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import dev.raegous.magicconch.VoiceManager

class SkipCommand[F[_]: Async](voiceManager: VoiceManager[F])(using Logger[F]) extends Command[F] {
  val name = "skip"
  val description = "Skip the current track"
  val arguments = List.empty

  def execute(context: CommandContext[F]): F[CommandResult] = {
    voiceManager.playNext(context.guildId).flatMap {
      case Some(nextTrack) =>
        voiceManager.startPlayingCurrent(context.guildId) >>
        Async[F].pure(CommandResult(s"⏭️ Skipped! Now playing: **${nextTrack.title}**"))
      case None =>
        Async[F].pure(CommandResult("❌ No more tracks in queue!", isError = true))
    }
  }
}
