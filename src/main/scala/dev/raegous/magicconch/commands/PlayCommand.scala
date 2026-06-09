package dev.raegous.magicconch.commands

import cats.effect.*
import cats.implicits.*
import cats.data.OptionT
import dev.raegous.magicconch.audio.VoiceManager
import dev.raegous.magicconch.errors.*
import dev.raegous.magicconch.music.TrackExtractor
import org.typelevel.log4cats.Logger
import dev.raegous.magicconch.discord.DiscordModels.{*, given}
import dev.raegous.magicconch.discord.*
import dev.raegous.magicconch.music.PlayerControls

import scala.concurrent.duration.*

class PlayCommand[F[_]: Async](
    voiceManager: VoiceManager[F],
    trackExtractor: TrackExtractor[F]
)(using Logger[F])
    extends Command[F] {
  val name = "play"
  val description = "Play music from a YouTube URL"
  val arguments = List(
    CommandArgument("url", "YouTube URL to play", required = true)
  )

  def execute(context: CommandContext[F]): F[CommandResult] = {
    val validatedUrl = context.args
      .get("url")
      .filter(url => url.startsWith("http://") || url.startsWith("https://"))

    validatedUrl.fold(
      getInvalidUrlMessage(context.args.get("url"))
    )(url => playTrack(url, context))
  }

  private def getInvalidUrlMessage(urlOpt: Option[String]): F[CommandResult] = {
    val message = urlOpt.fold(
      "Please provide a URL"
    )(_ => "Please provide a valid URL (YouTube, MP3, WAV, etc.)")
    Async[F].pure(CommandResult(message, isError = true))
  }

  private def playTrack(
      url: String,
      context: CommandContext[F]
  ): F[CommandResult] = {
    for {
      queueBefore <- voiceManager.getQueue(context.guildId)
      trackOpt <- trackExtractor.extractTrackInfo(url)
      result <- trackOpt.fold(
        Async[F].pure(
          CommandResult(
            "❌ Failed to extract audio from URL. Make sure it's a valid audio file or YouTube URL!",
            isError = true
          )
        )
      )(track => addTrackAndPlay(track, queueBefore, context))
    } yield result
  }

  private def addTrackAndPlay(
      track: MusicTrack,
      queueBefore: MusicQueue,
      context: CommandContext[F]
  ): F[CommandResult] = {
    val trackWithUser = track.copy(requestedBy = context.username)
    val wasEmpty =
      queueBefore.tracks.isEmpty && queueBefore.currentTrack.isEmpty

    for {
      _ <- Logger[F].info(s"[PLAY] Adding track to queue: ${track.title}")
      addResult <- voiceManager.addToQueue(context.guildId, trackWithUser)
      result <- addResult.fold(
        error =>
          Async[F].pure(CommandResult(s"❌ ${error.message}", isError = true)),
        _ =>
          for {
            _ <- Option
              .when(wasEmpty)(())
              .fold(logQueueing)(_ => autoJoinAndPlay(context))
            queueAfter <- voiceManager.getQueue(context.guildId)
          } yield {
            val components =
              PlayerControls.createPlayerControls(context.guildId, queueAfter)
            val message = Option
              .when(wasEmpty)(())
              .fold(
                s"Added to queue: **${track.title}**"
              )(_ => s"Now playing: **${track.title}**")
            CommandResult(message, components = components)
          }
      )
    } yield result
  }

  private def autoJoinAndPlay(context: CommandContext[F]): F[Unit] = {
    val action = for {
      _ <- OptionT.liftF(
        Logger[F].info(s"[PLAY] Queue was empty, will auto-join and play")
      )
      ws <- OptionT.fromOption[F](context.gatewayWs)
      channelId <- OptionT(voiceManager.getUserVoiceChannel(context.userId))
      _ <- OptionT.liftF(joinAndStartPlayback(context.guildId, channelId, ws))
    } yield ()

    action.value.flatMap(
      _.fold(handleAutoJoinFailure(context))(_ => Async[F].unit)
    )
  }

  private def handleAutoJoinFailure(context: CommandContext[F]): F[Unit] = {
    context.gatewayWs.fold(
      Logger[F].error(s"[PLAY] Gateway WebSocket not available")
    )(_ =>
      Logger[F].warn(
        s"[PLAY] User ${context.userId} is not in a voice channel, cannot auto-join"
      )
    )
  }

  private def joinAndStartPlayback(
      guildId: String,
      channelId: String,
      ws: sttp.ws.WebSocket[F]
  ): F[Unit] = {
    for {
      _ <- Logger[F].info(
        s"[PLAY] Found user in voice channel $channelId, joining..."
      )
      _ <- voiceManager.joinVoiceChannel(guildId, channelId, ws)
      _ <- voiceManager.waitForVoiceConnection(guildId)
      _ <- Logger[F].info(s"[PLAY] Starting playback now...")
      _ <- voiceManager.playNextTrack(guildId)
    } yield ()
  }

  private val logQueueing: F[Unit] =
    Logger[F].info(
      s"[PLAY] Queue not empty, track will play after current track finishes"
    )
}
