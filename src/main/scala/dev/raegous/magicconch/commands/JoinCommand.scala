package dev.raegous.magicconch.commands

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import dev.raegous.magicconch.VoiceManager

class JoinCommand[F[_]: Async](voiceManager: VoiceManager[F])(using Logger[F]) extends Command[F] {
  val name = "join"
  val description = "Join your current voice channel"
  val arguments = List.empty

  def execute(context: CommandContext[F]): F[CommandResult] = {
    context.gatewayWs match {
      case Some(ws) =>
        Logger[F].info(s"[JOIN] Looking up voice channel for user ${context.userId} (username: ${context.username})") >>
        voiceManager.getUserVoiceChannel(context.userId).flatMap {
          case Some(channelId) =>
            Logger[F].info(s"[JOIN] ✓ User ${context.userId} is in voice channel $channelId, joining...") >>
            voiceManager.joinVoiceChannel(context.guildId, channelId, ws) >>
            Async[F].pure(CommandResult("🔊 Joining your voice channel..."))
          case None =>
            Logger[F].warn(s"[JOIN] ✗ No voice channel found for user ${context.userId}. This usually means:") >>
            Logger[F].warn(s"[JOIN]   1. User is not in a voice channel") >>
            Logger[F].warn(s"[JOIN]   2. Bot doesn't have 'View Channels' permission") >>
            Logger[F].warn(s"[JOIN]   3. GUILD_VOICE_STATES intent not properly configured") >>
            Async[F].pure(CommandResult("❌ You need to be in a voice channel first!\n\n**Troubleshooting:**\n- Make sure you're in a voice channel\n- Bot needs 'View Channels' permission\n- Check bot has GUILD_VOICE_STATES intent enabled", isError = true))
        }
      case None =>
        Logger[F].error(s"[JOIN] Gateway WebSocket not available") >>
        Async[F].pure(CommandResult("❌ Gateway connection not available", isError = true))
    }
  }
}
