package dev.raegous.magicconch.music

import cats.effect.*
import cats.effect.implicits.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import dev.raegous.magicconch.audio.internals.*

/**
 * Controls music playback for guilds
 *
 * Responsibilities:
 * - Start/stop playback
 * - Pause/resume playback
 * - Skip tracks
 * - Seek within tracks
 * - Manage playback fibers
 */
class PlaybackController[F[_]: Async](
  private val queueManager: QueueManager[F],
  private val trackExtractor: TrackExtractor[F],
  private val voiceGateway: VoiceGateway[F],
  private val activePlaybackFibers: Ref[F, Map[String, Fiber[F, Throwable, Unit]]],
  private val activeVoiceConnections: Ref[F, Map[String, sttp.ws.WebSocket[F]]],
  private val gatewayWebSocketRef: Ref[F, Option[sttp.ws.WebSocket[F]]]
)(using Logger[F]) {

  /**
   * Start playing the current track in the queue
   */
  def startPlayingCurrent(guildId: String): F[Unit] = {
    Logger[F].debug(s"[PLAYBACK] Starting playback for guild $guildId") >>
    queueManager.getQueue(guildId).flatMap { queue =>
      queue.currentTrack.fold(
        Logger[F].warn(s"[PLAYBACK] No current track to play - queue might be empty")
      ) { track =>
        Logger[F].info(s"[PLAYBACK] Playing: ${track.title}") >>
        trackExtractor.getStreamUrl(track.url).flatMap {
          case None =>
            Logger[F].error(s"[PLAYBACK] Failed to extract stream URL") >>
            queueManager.setCurrentTrack(guildId, None)
          case Some(streamUrl) =>
            activeVoiceConnections.get.flatMap { connections =>
              connections.get(guildId).fold(
                Logger[F].error(s"[PLAYBACK] No active voice connection for guild $guildId")
              ) { voiceWs =>
                startPlaybackFiber(guildId, streamUrl, voiceWs, startPosition = 0)
              }
            }
        }
      }
    }
  }

  /**
   * Start playing from a specific position (for seek/resume)
   */
  def startPlayingCurrentWithPosition(guildId: String, startPosition: Int): F[Unit] = {
    queueManager.getQueue(guildId).flatMap { queue =>
      queue.currentTrack.fold(
        Logger[F].warn(s"[PLAYBACK] No current track to resume")
      ) { track =>
        trackExtractor.getStreamUrl(track.url).flatMap {
          case None =>
            Logger[F].error(s"[PLAYBACK] Failed to extract stream URL for resume") >>
            queueManager.setCurrentTrack(guildId, None)
          case Some(streamUrl) =>
            activeVoiceConnections.get.flatMap { connections =>
              connections.get(guildId).fold(
                Logger[F].error(s"[PLAYBACK] No active voice connection")
              ) { voiceWs =>
                startPlaybackFiber(guildId, streamUrl, voiceWs, startPosition)
              }
            }
        }
      }
    }
  }

  /**
   * Pause playback
   */
  def pausePlayback(guildId: String): F[Unit] = {
    queueManager.getQueue(guildId).flatMap { queue =>
      queue.currentTrack match {
        case Some(track) if !queue.isPaused =>
          for {
            currentPos <- queueManager.getCurrentPosition(guildId)
            _ <- Logger[F].info(s"[PAUSE] Pausing ${track.title} at ${currentPos}s")
            _ <- stopMusic(guildId)
            _ <- queueManager.updatePlaybackState(
              guildId,
              isPlaying = false,
              isPaused = true,
              currentPosition = currentPos,
              pauseTime = Some(System.currentTimeMillis())
            )
          } yield ()
        case _ => Async[F].unit
      }
    }
  }

  /**
   * Resume playback
   */
  def resumePlayback(guildId: String): F[Unit] = {
    queueManager.getQueue(guildId).flatMap { queue =>
      queue.currentTrack match {
        case Some(track) if queue.isPaused =>
          Logger[F].info(s"[RESUME] Resuming ${track.title} from ${queue.currentPosition}s") >>
          queueManager.updatePlaybackState(
            guildId,
            isPlaying = true,
            isPaused = false,
            startTime = Some(System.currentTimeMillis())
          ) >>
          startPlayingCurrentWithPosition(guildId, queue.currentPosition)
        case _ => Async[F].unit
      }
    }
  }

  /**
   * Skip to the next track
   */
  def skipTrack(guildId: String): F[Unit] = {
    Logger[F].info(s"[SKIP] Skipping current track") >>
    stopMusic(guildId) >>
    playNextTrack(guildId)
  }

  /**
   * Stop playback and clear queue
   */
  def stopPlayback(guildId: String): F[Unit] = {
    Logger[F].info(s"[STOP] Stopping playback and clearing queue") >>
    stopMusic(guildId) >>
    queueManager.clearQueue(guildId)
  }

  /**
   * Seek to a position in the current track
   */
  def seek(guildId: String, position: Int): F[Unit] = {
    queueManager.getQueue(guildId).flatMap { queue =>
      queue.currentTrack match {
        case Some(track) =>
          Logger[F].info(s"[SEEK] Seeking to ${position}s in ${track.title}") >>
          stopMusic(guildId) >>
          queueManager.updatePlaybackState(
            guildId,
            isPlaying = true,
            isPaused = false,
            currentPosition = position,
            startTime = Some(System.currentTimeMillis())
          ) >>
          startPlayingCurrentWithPosition(guildId, position)
        case None =>
          Logger[F].warn(s"[SEEK] No track currently playing")
      }
    }
  }

  /**
   * Play the next track from queue
   */
  def playNextTrack(guildId: String): F[Unit] = {
    Logger[F].info(s"[PLAYBACK] playNextTrack called") >>
    queueManager.playNext(guildId).flatMap(
      _.fold(
        Logger[F].warn(s"[PLAYBACK] Queue is empty")
      ) { track =>
        Logger[F].info(s"[PLAYBACK] Got next track: ${track.title}") >>
        startPlayingCurrent(guildId)
      }
    )
  }

  /**
   * Stop the music playback fiber
   */
  def stopMusic(guildId: String): F[Unit] = {
    activePlaybackFibers.get.flatMap { fibers =>
      fibers.get(guildId).fold(
        Logger[F].debug(s"[STOP] No active playback fiber for guild $guildId")
      ) { fiber =>
        Logger[F].info(s"[STOP] Cancelling playback fiber for guild $guildId") >>
        fiber.cancel >>
        activePlaybackFibers.update(_ - guildId)
      }
    }
  }

  // Private helper to start a playback fiber
  private def startPlaybackFiber(guildId: String, streamUrl: String, voiceWs: sttp.ws.WebSocket[F], startPosition: Int): F[Unit] = {
    val playbackAction = {
      val streamF = if (startPosition > 0) {
        voiceGateway.streamAudioFromPosition(streamUrl, voiceWs, startPosition, guildId)
      } else {
        voiceGateway.streamAudio(streamUrl, voiceWs, guildId)
      }

      streamF.handleErrorWith { error =>
        Logger[F].error(s"[PLAYBACK] Failed to stream audio: ${error.getMessage}") >>
        queueManager.setCurrentTrack(guildId, None)
      } >>
      Logger[F].info(s"[PLAYBACK] Finished playing") >>
      // After track finishes, play next or leave
      playNextTrackOrLeave(guildId)
    }

    // Update queue state to playing
    queueManager.updatePlaybackState(
      guildId,
      isPlaying = true,
      isPaused = false,
      currentPosition = startPosition,
      startTime = Some(System.currentTimeMillis())
    ) >>
    playbackAction.start.flatMap { fiber =>
      activePlaybackFibers.update(_ + (guildId -> fiber))
    }
  }

  // Private helper to play next track or leave voice channel
  private def playNextTrackOrLeave(guildId: String): F[Unit] = {
    queueManager.getQueue(guildId).flatMap { queue =>
      if (queue.tracks.nonEmpty) {
        // More tracks in queue, play next
        playNextTrack(guildId)
      } else {
        // Queue is empty, leave voice channel
        Logger[F].info(s"[PLAYBACK] Queue empty, leaving voice channel") >>
        gatewayWebSocketRef.get.flatMap(
          _.fold(
            Logger[F].warn(s"[PLAYBACK] No gateway WebSocket available")
          ) { gatewayWs =>
            // VoiceManager will handle the actual leave logic
            activeVoiceConnections.update(_ - guildId) >>
            activePlaybackFibers.update(_ - guildId)
          }
        )
      }
    }
  }
}

object PlaybackController {
  def make[F[_]: Async](
    queueManager: QueueManager[F],
    trackExtractor: TrackExtractor[F],
    voiceGateway: VoiceGateway[F],
    activePlaybackFibers: Ref[F, Map[String, Fiber[F, Throwable, Unit]]],
    activeVoiceConnections: Ref[F, Map[String, sttp.ws.WebSocket[F]]],
    gatewayWebSocketRef: Ref[F, Option[sttp.ws.WebSocket[F]]]
  )(using Logger[F]): PlaybackController[F] = {
    new PlaybackController[F](
      queueManager,
      trackExtractor,
      voiceGateway,
      activePlaybackFibers,
      activeVoiceConnections,
      gatewayWebSocketRef
    )
  }
}
