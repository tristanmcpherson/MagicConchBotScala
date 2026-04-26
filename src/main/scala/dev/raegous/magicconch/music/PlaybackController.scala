package dev.raegous.magicconch.music

import cats.effect.*
import cats.effect.implicits.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import dev.raegous.magicconch.audio.internals.*

class PlaybackController[F[_]: Async](
  private val queueManager: QueueManager[F],
  private val trackExtractor: TrackExtractor[F],
  private val voiceGateway: VoiceGateway[F],
  private val activePlaybackFibers: Ref[F, Map[String, Fiber[F, Throwable, Unit]]],
  private val activeVoiceConnections: Ref[F, Map[String, sttp.ws.WebSocket[F]]],
  private val gatewayWebSocketRef: Ref[F, Option[sttp.ws.WebSocket[F]]]
)(using Logger[F]) {

  def startPlayingCurrent(guildId: String): F[Unit] =
    Logger[F].debug(s"[PLAYBACK] Starting playback for guild $guildId") >>
    queueManager.getQueue(guildId).flatMap { queue =>
      queue.currentTrack.fold(
        Logger[F].warn(s"[PLAYBACK] No current track to play - queue might be empty")
      ) { track =>
        Logger[F].info(s"[PLAYBACK] Playing: ${track.title}") >>
        trackExtractor.getStreamUrl(track.url).flatMap(
          _.fold(
            Logger[F].error(s"[PLAYBACK] Failed to extract stream URL") >>
            queueManager.setCurrentTrack(guildId, None)
          ) { streamUrl =>
            activeVoiceConnections.get.flatMap(
              _.get(guildId).fold(
                Logger[F].error(s"[PLAYBACK] No active voice connection for guild $guildId")
              )(startPlaybackFiber(guildId, streamUrl, _, startPosition = 0))
            )
          }
        )
      }
    }

  def startPlayingCurrentWithPosition(guildId: String, startPosition: Int): F[Unit] =
    queueManager.getQueue(guildId).flatMap { queue =>
      queue.currentTrack.fold(
        Logger[F].warn(s"[PLAYBACK] No current track to resume")
      ) { track =>
        trackExtractor.getStreamUrl(track.url).flatMap(
          _.fold(
            Logger[F].error(s"[PLAYBACK] Failed to extract stream URL for resume") >>
            queueManager.setCurrentTrack(guildId, None)
          ) { streamUrl =>
            activeVoiceConnections.get.flatMap(
              _.get(guildId).fold(
                Logger[F].error(s"[PLAYBACK] No active voice connection")
              )(startPlaybackFiber(guildId, streamUrl, _, startPosition))
            )
          }
        )
      }
    }

  def pausePlayback(guildId: String): F[Unit] =
    queueManager.getQueue(guildId).flatMap { queue =>
      queue.currentTrack.filter(_ => !queue.isPaused).fold(Async[F].unit) { track =>
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
      }
    }

  def resumePlayback(guildId: String): F[Unit] =
    queueManager.getQueue(guildId).flatMap { queue =>
      queue.currentTrack.filter(_ => queue.isPaused).fold(Async[F].unit) { track =>
        Logger[F].info(s"[RESUME] Resuming ${track.title} from ${queue.currentPosition}s") >>
        queueManager.updatePlaybackState(
          guildId,
          isPlaying = true,
          isPaused = false,
          startTime = Some(System.currentTimeMillis())
        ) >>
        startPlayingCurrentWithPosition(guildId, queue.currentPosition)
      }
    }

  def skipTrack(guildId: String): F[Unit] =
    Logger[F].info(s"[SKIP] Skipping current track") >>
    stopMusic(guildId) >>
    playNextTrack(guildId)

  def stopPlayback(guildId: String): F[Unit] =
    Logger[F].info(s"[STOP] Stopping playback and clearing queue") >>
    stopMusic(guildId) >>
    queueManager.clearQueue(guildId)

  def seek(guildId: String, position: Int): F[Unit] =
    queueManager.getQueue(guildId).flatMap { queue =>
      queue.currentTrack.fold(
        Logger[F].warn(s"[SEEK] No track currently playing")
      ) { track =>
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
      }
    }

  def playNextTrack(guildId: String): F[Unit] =
    Logger[F].info(s"[PLAYBACK] playNextTrack called") >>
    queueManager.playNext(guildId).flatMap(
      _.fold(
        Logger[F].warn(s"[PLAYBACK] Queue is empty")
      ) { track =>
        Logger[F].info(s"[PLAYBACK] Got next track: ${track.title}") >>
        startPlayingCurrent(guildId)
      }
    )

  def stopMusic(guildId: String): F[Unit] =
    activePlaybackFibers.get.flatMap { fibers =>
      fibers.get(guildId).fold(
        Logger[F].debug(s"[STOP] No active playback fiber for guild $guildId")
      ) { fiber =>
        Logger[F].info(s"[STOP] Cancelling playback fiber for guild $guildId") >>
        fiber.cancel >>
        activePlaybackFibers.update(_ - guildId)
      }
    }

  private def startPlaybackFiber(guildId: String, streamUrl: String, voiceWs: sttp.ws.WebSocket[F], startPosition: Int): F[Unit] = {
    val streamF = Option.when(startPosition > 0)(
      voiceGateway.streamAudioFromPosition(streamUrl, voiceWs, startPosition, guildId)
    ).getOrElse(
      voiceGateway.streamAudio(streamUrl, voiceWs, guildId)
    )

    val playbackAction = streamF.handleErrorWith { error =>
      Logger[F].error(s"[PLAYBACK] Failed to stream audio: ${error.getMessage}") >>
      queueManager.setCurrentTrack(guildId, None)
    } >>
    Logger[F].info(s"[PLAYBACK] Finished playing") >>
    playNextTrackOrLeave(guildId)

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

  private def playNextTrackOrLeave(guildId: String): F[Unit] =
    queueManager.getQueue(guildId).flatMap { queue =>
      Option.when(queue.tracks.nonEmpty)(playNextTrack(guildId)).getOrElse(
        Logger[F].info(s"[PLAYBACK] Queue empty, leaving voice channel") >>
        gatewayWebSocketRef.get.flatMap(
          _.fold(
            Logger[F].warn(s"[PLAYBACK] No gateway WebSocket available")
          ) { _ =>
            activeVoiceConnections.update(_ - guildId) >>
            activePlaybackFibers.update(_ - guildId)
          }
        )
      )
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
  )(using Logger[F]): PlaybackController[F] =
    new PlaybackController[F](
      queueManager,
      trackExtractor,
      voiceGateway,
      activePlaybackFibers,
      activeVoiceConnections,
      gatewayWebSocketRef
    )
}
