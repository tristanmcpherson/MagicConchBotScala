package dev.raegous.magicconch.commands

import cats.effect.*
import cats.implicits.*
import dev.raegous.magicconch.audio.VoiceManager
import org.typelevel.log4cats.Logger

class LeaveCommand[F[_]: Async](voiceManager: VoiceManager[F])(using Logger[F]) extends Command[F] {
  val name = "leave"
  val description = "Leave the voice channel"
  val arguments = List.empty

  def execute(context: CommandContext[F]): F[CommandResult] = {
    context.gatewayWs match {
      case Some(ws) =>
        voiceManager.leaveVoiceChannel(context.guildId, ws) >>
        Async[F].pure(CommandResult("👋 Left voice channel!"))
      case None =>
        Async[F].pure(CommandResult("❌ Gateway connection not available", isError = true))
    }
  }
}
