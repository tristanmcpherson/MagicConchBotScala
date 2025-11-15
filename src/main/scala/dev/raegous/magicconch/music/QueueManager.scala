package dev.raegous.magicconch.music

import cats.effect.*
import cats.implicits.*
import dev.raegous.magicconch.discord.*
import org.typelevel.log4cats.Logger

/**
 * Manages music queues for guilds
 *
 * Responsibilities:
 * - Queue CRUD operations (add, get, clear)
 * - Queue state management (playing, paused, position)
 * - Next track selection
 */
class QueueManager[F[_]: Async](
  private val musicQueueRef: Ref[F, Map[String, MusicQueue]]
)(using Logger[F]) {

  /**
   * Add a track to the end of the queue for a guild
   */
  def addToQueue(guildId: String, track: MusicTrack): F[Unit] = {
    musicQueueRef.update { queues =>
      val currentQueue = queues.getOrElse(guildId, MusicQueue(List.empty, None, false))
      val updatedQueue = currentQueue.copy(tracks = currentQueue.tracks :+ track)
      queues + (guildId -> updatedQueue)
    } >> Logger[F].info(s"[QUEUE] Added '${track.title}' to queue for guild $guildId")
  }

  /**
   * Get the current queue state for a guild
   */
  def getQueue(guildId: String): F[MusicQueue] = {
    musicQueueRef.get.map(_.getOrElse(guildId, MusicQueue(List.empty, None, false)))
  }

  /**
   * Clear the entire queue for a guild
   */
  def clearQueue(guildId: String): F[Unit] = {
    musicQueueRef.update { queues =>
      queues + (guildId -> MusicQueue(List.empty, None, false))
    } >> Logger[F].info(s"[QUEUE] Cleared queue for guild $guildId")
  }

  /**
   * Pop the next track from the queue and set it as current
   * Returns the popped track
   */
  def playNext(guildId: String): F[Option[MusicTrack]] = {
    musicQueueRef.modify { queues =>
      val currentQueue = queues.getOrElse(guildId, MusicQueue(List.empty, None, false))
      currentQueue.tracks.headOption match {
        case Some(nextTrack) =>
          val updatedQueue = currentQueue.copy(
            tracks = currentQueue.tracks.tail,
            currentTrack = Some(nextTrack),
            isPlaying = false,  // Will be set to true when playback starts
            currentPosition = 0,
            startTime = None
          )
          (queues + (guildId -> updatedQueue), Some(nextTrack))
        case None =>
          val updatedQueue = currentQueue.copy(
            currentTrack = None,
            isPlaying = false
          )
          (queues + (guildId -> updatedQueue), None)
      }
    }
  }

  /**
   * Set the current track (used when resuming or seeking)
   */
  def setCurrentTrack(guildId: String, track: Option[MusicTrack]): F[Unit] = {
    musicQueueRef.update { queues =>
      val currentQueue = queues.getOrElse(guildId, MusicQueue(List.empty, None, false))
      val updatedQueue = currentQueue.copy(currentTrack = track)
      queues + (guildId -> updatedQueue)
    }
  }

  /**
   * Update queue playback state
   */
  def updatePlaybackState(
    guildId: String,
    isPlaying: Boolean,
    isPaused: Boolean = false,
    currentPosition: Int = 0,
    startTime: Option[Long] = None,
    pauseTime: Option[Long] = None
  ): F[Unit] = {
    musicQueueRef.update { queues =>
      val currentQueue = queues.getOrElse(guildId, MusicQueue(List.empty, None, false))
      val updatedQueue = currentQueue.copy(
        isPlaying = isPlaying,
        isPaused = isPaused,
        currentPosition = currentPosition,
        startTime = startTime,
        pauseTime = pauseTime
      )
      queues + (guildId -> updatedQueue)
    }
  }

  /**
   * Check if queue is currently paused
   */
  def isPaused(guildId: String): F[Boolean] = {
    getQueue(guildId).map(_.isPaused)
  }

  /**
   * Get current playback position in seconds
   */
  def getCurrentPosition(guildId: String): F[Int] = {
    getQueue(guildId).map { queue =>
      if (queue.isPaused) {
        queue.currentPosition
      } else {
        queue.startTime match {
          case Some(start) =>
            val elapsed = ((System.currentTimeMillis() - start) / 1000).toInt
            queue.currentPosition + elapsed
          case None =>
            queue.currentPosition
        }
      }
    }
  }
}

object QueueManager {
  def make[F[_]: Async](using Logger[F]): Resource[F, QueueManager[F]] = {
    Resource.eval {
      Ref.of[F, Map[String, MusicQueue]](Map.empty).map { ref =>
        new QueueManager[F](ref)
      }
    }
  }
}
