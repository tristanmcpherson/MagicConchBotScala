package dev.raegous.magicconch.commands

import cats.effect.*
import cats.implicits.*
import cats.data.OptionT
import dev.raegous.magicconch.audio.VoiceManager
import org.typelevel.log4cats.Logger

class JoinCommand[F[_]: Async](voiceManager: VoiceManager[F])(using Logger[F]) extends Command[F] {
  val name = "join"
  val description = "Join your current voice channel"
  val arguments = List.empty

  def execute(context: CommandContext[F]): F[CommandResult] = {
    val result = for {
      ws <- OptionT.fromOption[F](context.gatewayWs)
      _ <- OptionT.liftF(Logger[F].info(s"[JOIN] Looking up voice channel for user ${context.userId} (username: ${context.username})"))
      channelId <- OptionT(voiceManager.getUserVoiceChannel(context.userId))
      _ <- OptionT.liftF(Logger[F].info(s"[JOIN] ✓ User ${context.userId} is in voice channel $channelId, joining..."))
      _ <- OptionT.liftF(voiceManager.joinVoiceChannel(context.guildId, channelId, ws))
    } yield CommandResult("🔊 Joining your voice channel...")

    result.value.flatMap {
      case Some(cmdResult) => Async[F].pure(cmdResult)
      case None => handleJoinFailure(context)
    }
  }

  private def handleJoinFailure(context: CommandContext[F]): F[CommandResult] = {
    context.gatewayWs.fold(
      Logger[F].error(s"[JOIN] Gateway WebSocket not available") >>
      Async[F].pure(CommandResult("Gateway connection not available", isError = true))
    )(_ =>
      Logger[F].warn(s"[JOIN] No voice channel found for user ${context.userId}. This usually means:") >>
      Logger[F].warn(s"[JOIN]   1. User is not in a voice channel") >>
      Logger[F].warn(s"[JOIN]   2. Bot doesn't have 'View Channels' permission") >>
      Logger[F].warn(s"[JOIN]   3. GUILD_VOICE_STATES intent not properly configured") >>
      Async[F].pure(CommandResult(
        "You need to be in a voice channel first!\n\n**Troubleshooting:**\n- Make sure you're in a voice channel\n- Bot needs 'View Channels' permission\n- Check bot has GUILD_VOICE_STATES intent enabled",
        isError = true
      ))
    )
  }
}
