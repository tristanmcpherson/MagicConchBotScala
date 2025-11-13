package dev.raegous.magicconch.commands

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import dev.raegous.magicconch.VoiceManager
import scala.concurrent.duration.*

class PlayCommand[F[_]: Async](voiceManager: VoiceManager[F])(using Logger[F]) extends Command[F] {
  val name = "play"
  val description = "Play music from a YouTube URL"
  val arguments = List(
    CommandArgument("url", "YouTube URL to play", required = true)
  )

  def execute(context: CommandContext[F]): F[CommandResult] = {
    context.args.get("url") match {
      case Some(url) if url.startsWith("http://") || url.startsWith("https://") =>
        for {
          queueBefore <- voiceManager.getQueue(context.guildId)
          trackOpt <- voiceManager.extractAudioFromYoutube(url)
          result <- trackOpt match {
            case Some(track) =>
              for {
                _ <- Logger[F].info(s"[PLAY] Adding track to queue: ${track.title}")
                trackWithUser = track.copy(requestedBy = context.username)
                _ <- voiceManager.addToQueue(context.guildId, trackWithUser)
                // If queue was empty and nothing was playing, join voice and start playback
                _ <- if (queueBefore.tracks.isEmpty && queueBefore.currentTrack.isEmpty) {
                  for {
                    _ <- Logger[F].info(s"[PLAY] Queue was empty, will auto-join and play")
                    _ <- context.gatewayWs match {
                      case Some(ws) =>
                        voiceManager.getUserVoiceChannel(context.userId).flatMap {
                          case Some(channelId) =>
                            for {
                              _ <- Logger[F].info(s"[PLAY] Found user in voice channel $channelId, joining...")
                              _ <- voiceManager.joinVoiceChannel(context.guildId, channelId, ws)
                              _ <- voiceManager.waitForVoiceConnection(context.guildId)
                              _ <- Logger[F].info(s"[PLAY] Starting playback now...")
                              _ <- voiceManager.playNextTrack(context.guildId)
                            } yield ()
                          case None =>
                            Logger[F].warn(s"[PLAY] User ${context.userId} is not in a voice channel, cannot auto-join")
                        }
                      case None =>
                        Logger[F].error(s"[PLAY] Gateway WebSocket not available for joining voice channel")
                    }
                  } yield ()
                } else {
                  Logger[F].info(s"[PLAY] Queue not empty, track will play after current track finishes")
                }
              } yield CommandResult(s"🎵 Added to queue: **${track.title}**")
            case None =>
              Async[F].pure(CommandResult("❌ Failed to extract audio from URL. Make sure it's a valid audio file or YouTube URL!", isError = true))
          }
        } yield result
      case Some(_) =>
        Async[F].pure(CommandResult("❌ Please provide a valid URL (YouTube, MP3, WAV, etc.)", isError = true))
      case None =>
        Async[F].pure(CommandResult("❌ Please provide a URL", isError = true))
    }
  }
}
