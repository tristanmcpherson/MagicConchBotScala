package dev.raegous.magicconch.music

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import fs2.io.process.Processes
import dev.raegous.magicconch.audio.*
import dev.raegous.magicconch.audio.internals.*
import dev.raegous.magicconch.discord.*

class MusicService[F[_]: Async: Processes](
  val queueManager: QueueManager[F],
  val playbackController: PlaybackController[F],
  val trackExtractor: TrackExtractor[F]
)(using Logger[F]) {

  def addToQueue(guildId: String, track: MusicTrack): F[Unit] =
    queueManager.addToQueue(guildId, track)

  def getQueue(guildId: String): F[MusicQueue] =
    queueManager.getQueue(guildId)

  def clearQueue(guildId: String): F[Unit] =
    queueManager.clearQueue(guildId)

  def addPlaylist(guildId: String, playlistUrl: String, requestedBy: String): F[Int] =
    Logger[F].info(s"[PLAYLIST] Adding playlist to queue for guild $guildId") >>
    trackExtractor.extractPlaylistUrls(playlistUrl).flatMap { urls =>
      Logger[F].info(s"[PLAYLIST] Found ${urls.length} tracks in playlist") >>
      urls.traverse { url =>
        trackExtractor.extractTrackInfo(url).flatMap(
          _.fold(Async[F].pure(0)) { track =>
            val trackWithUser = track.copy(requestedBy = requestedBy)
            addToQueue(guildId, trackWithUser).as(1)
          }
        )
      }.flatMap { results =>
        val addedCount = results.sum
        Logger[F].info(s"[PLAYLIST] Successfully added $addedCount tracks to queue").as(addedCount)
      }
    }

  def extractTrackInfo(url: String): F[Option[MusicTrack]] =
    trackExtractor.extractTrackInfo(url)

  def startPlayingCurrent(guildId: String): F[Unit] =
    playbackController.startPlayingCurrent(guildId)

  def playNextTrack(guildId: String): F[Unit] =
    playbackController.playNextTrack(guildId)

  def pausePlayback(guildId: String): F[Unit] =
    playbackController.pausePlayback(guildId)

  def resumePlayback(guildId: String): F[Unit] =
    playbackController.resumePlayback(guildId)

  def skipTrack(guildId: String): F[Unit] =
    playbackController.skipTrack(guildId)

  def stopPlayback(guildId: String): F[Unit] =
    playbackController.stopPlayback(guildId)

  def stopMusic(guildId: String): F[Unit] =
    playbackController.stopMusic(guildId)

  def seek(guildId: String, position: Int): F[Unit] =
    playbackController.seek(guildId, position)

  def isPaused(guildId: String): F[Boolean] =
    queueManager.isPaused(guildId)

  def getCurrentPosition(guildId: String): F[Int] =
    queueManager.getCurrentPosition(guildId)
}

object MusicService {
  def make[F[_]: Async: Processes](
    voiceGateway: VoiceGateway[F],
    activePlaybackFibers: Ref[F, Map[String, Fiber[F, Throwable, Unit]]],
    activeVoiceConnections: Ref[F, Map[String, sttp.ws.WebSocket[F]]],
    gatewayWebSocketRef: Ref[F, Option[sttp.ws.WebSocket[F]]]
  )(using Logger[F]): Resource[F, MusicService[F]] =
    for {
      queueManager <- QueueManager.make[F]
      trackExtractor = TrackExtractor.make[F]
      playbackController = PlaybackController.make[F](
        queueManager,
        trackExtractor,
        voiceGateway,
        activePlaybackFibers,
        activeVoiceConnections,
        gatewayWebSocketRef
      )
    } yield new MusicService[F](queueManager, playbackController, trackExtractor)
}
