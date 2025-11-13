package dev.raegous.magicconch.commands

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import dev.raegous.magicconch.VoiceManager

class StopCommand[F[_]: Async](voiceManager: VoiceManager[F])(using Logger[F]) extends Command[F] {
  val name = "stop"
  val description = "Stop the current music and clear queue"
  val arguments = List.empty

  def execute(context: CommandContext[F]): F[CommandResult] = {
    voiceManager.stopMusic(context.guildId) >>
    voiceManager.clearQueue(context.guildId) >>
    Async[F].pure(CommandResult("🛑 Music stopped and queue cleared!"))
  }
}
