package dev.raegous.magicconch.music

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import fs2.io.process.Processes
import dev.raegous.magicconch.{MusicTrack, MusicQueue, VoiceGateway}

/**
 * Facade service for all music-related operations
 *
 * Coordinates:
 * - QueueManager (queue operations)
 * - PlaybackController (playback control)
 * - TrackExtractor (track info extraction)
 *
 * This is where high-level music operations belong, including:
 * - Adding playlists
 * - Adding tracks
 * - Playback control
 */
class MusicService[F[_]: Async: Processes](
  val queueManager: QueueManager[F],
  val playbackController: PlaybackController[F],
  val trackExtractor: TrackExtractor[F]
)(using Logger[F]) {

  // ===== Queue Operations =====

  /**
   * Add a single track to the queue
   */
  def addToQueue(guildId: String, track: MusicTrack): F[Unit] = {
    queueManager.addToQueue(guildId, track)
  }

  /**
   * Get the current queue state
   */
  def getQueue(guildId: String): F[MusicQueue] = {
    queueManager.getQueue(guildId)
  }

  /**
   * Clear the queue
   */
  def clearQueue(guildId: String): F[Unit] = {
    queueManager.clearQueue(guildId)
  }

  /**
   * Add a YouTube playlist to the queue
   *
   * THIS IS THE METHOD that was misplaced in VoiceManager!
   * It orchestrates:
   * 1. Extract playlist URLs
   * 2. Extract track info for each URL
   * 3. Add tracks to queue
   */
  def addPlaylist(guildId: String, playlistUrl: String, requestedBy: String): F[Int] = {
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
      }.map { results =>
        val addedCount = results.sum
        Logger[F].info(s"[PLAYLIST] Successfully added $addedCount tracks to queue")
        addedCount
      }
    }
  }

  /**
   * Extract track info from a URL
   */
  def extractTrackInfo(url: String): F[Option[MusicTrack]] = {
    trackExtractor.extractTrackInfo(url)
  }

  // ===== Playback Control =====

  /**
   * Start playing the current track
   */
  def startPlayingCurrent(guildId: String): F[Unit] = {
    playbackController.startPlayingCurrent(guildId)
  }

  /**
   * Play the next track in queue
   */
  def playNextTrack(guildId: String): F[Unit] = {
    playbackController.playNextTrack(guildId)
  }

  /**
   * Pause playback
   */
  def pausePlayback(guildId: String): F[Unit] = {
    playbackController.pausePlayback(guildId)
  }

  /**
   * Resume playback
   */
  def resumePlayback(guildId: String): F[Unit] = {
    playbackController.resumePlayback(guildId)
  }

  /**
   * Skip to next track
   */
  def skipTrack(guildId: String): F[Unit] = {
    playbackController.skipTrack(guildId)
  }

  /**
   * Stop playback and clear queue
   */
  def stopPlayback(guildId: String): F[Unit] = {
    playbackController.stopPlayback(guildId)
  }

  /**
   * Stop music playback fiber
   */
  def stopMusic(guildId: String): F[Unit] = {
    playbackController.stopMusic(guildId)
  }

  /**
   * Seek to a position in the current track
   */
  def seek(guildId: String, position: Int): F[Unit] = {
    playbackController.seek(guildId, position)
  }

  /**
   * Check if playback is currently paused
   */
  def isPaused(guildId: String): F[Boolean] = {
    queueManager.isPaused(guildId)
  }

  /**
   * Get current playback position in seconds
   */
  def getCurrentPosition(guildId: String): F[Int] = {
    queueManager.getCurrentPosition(guildId)
  }
}

object MusicService {
  /**
   * Create MusicService with all dependencies properly injected
   *
   * Uses Resource pattern for lifecycle management
   */
  def make[F[_]: Async: Processes](
    voiceGateway: VoiceGateway[F],
    activePlaybackFibers: Ref[F, Map[String, Fiber[F, Throwable, Unit]]],
    activeVoiceConnections: Ref[F, Map[String, sttp.ws.WebSocket[F]]],
    gatewayWebSocketRef: Ref[F, Option[sttp.ws.WebSocket[F]]]
  )(using Logger[F]): Resource[F, MusicService[F]] = {
    for {
      // Create queue manager (has Resource lifecycle)
      queueManager <- QueueManager.make[F]

      // Create track extractor (pure, no lifecycle)
      trackExtractor = TrackExtractor.make[F]

      // Create playback controller (pure, depends on queue and track extractor)
      playbackController = PlaybackController.make[F](
        queueManager,
        trackExtractor,
        voiceGateway,
        activePlaybackFibers,
        activeVoiceConnections,
        gatewayWebSocketRef
      )

      // Create the facade service
      musicService = new MusicService[F](queueManager, playbackController, trackExtractor)
    } yield musicService
  }
}
