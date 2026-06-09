package dev.raegous.magicconch.music

import cats.effect.*
import dev.raegous.magicconch.TestFixtures.{testLogger, *}
import dev.raegous.magicconch.discord.MusicQueue
import munit.CatsEffectSuite

class QueueManagerTest extends CatsEffectSuite {

  test("new QueueManager should have empty queue") {
    QueueManager.make[IO].use { qm =>
      qm.getQueue("guild123").map { queue =>
        assert(queue.tracks.isEmpty, "Queue should be empty")
        assert(queue.currentTrack.isEmpty, "No current track")
        assert(!queue.isPlaying, "Should not be playing")
      }
    }
  }

  test("addToQueue should add track to empty queue") {
    QueueManager.make[IO].use { qm =>
      val track = sampleMusicTrack(title = "Track 1")

      for {
        _ <- qm.addToQueue("guild123", track)
        queue <- qm.getQueue("guild123")
      } yield {
        assertEquals(queue.tracks.length, 1, "Should have 1 track")
        assertEquals(queue.tracks.head.title, "Track 1")
      }
    }
  }

  test("addToQueue should append to existing queue") {
    QueueManager.make[IO].use { qm =>
      val track1 = sampleMusicTrack(title = "Track 1")
      val track2 = sampleMusicTrack(title = "Track 2")
      val track3 = sampleMusicTrack(title = "Track 3")

      for {
        _ <- qm.addToQueue("guild123", track1)
        _ <- qm.addToQueue("guild123", track2)
        _ <- qm.addToQueue("guild123", track3)
        queue <- qm.getQueue("guild123")
      } yield {
        assertEquals(queue.tracks.length, 3)
        assertEquals(
          queue.tracks.map(_.title),
          List("Track 1", "Track 2", "Track 3")
        )
      }
    }
  }

  test("playNext should return and remove first track") {
    QueueManager.make[IO].use { qm =>
      val track1 = sampleMusicTrack(title = "Track 1")
      val track2 = sampleMusicTrack(title = "Track 2")

      for {
        _ <- qm.addToQueue("guild123", track1)
        _ <- qm.addToQueue("guild123", track2)
        next <- qm.playNext("guild123")
        queue <- qm.getQueue("guild123")
      } yield {
        assert(next.isDefined, "Should return a track")
        assertEquals(next.get.title, "Track 1")
        assertEquals(queue.tracks.length, 1, "Should have 1 remaining track")
        assertEquals(queue.tracks.head.title, "Track 2")
        assertEquals(
          queue.currentTrack.map(_.title),
          Some("Track 1"),
          "Should set as current track"
        )
      }
    }
  }

  test("playNext should return None for empty queue") {
    QueueManager.make[IO].use { qm =>
      qm.playNext("guild123").map { next =>
        assert(next.isEmpty, "Should return None for empty queue")
      }
    }
  }

  test("setCurrentTrack should set the current track") {
    QueueManager.make[IO].use { qm =>
      val track = sampleMusicTrack(title = "Current Track")

      for {
        _ <- qm.setCurrentTrack("guild123", Some(track))
        queue <- qm.getQueue("guild123")
      } yield {
        assert(queue.currentTrack.isDefined, "Should have current track")
        assertEquals(queue.currentTrack.get.title, "Current Track")
      }
    }
  }

  test("clearQueue should clear all tracks and current track") {
    QueueManager.make[IO].use { qm =>
      val track1 = sampleMusicTrack(title = "Track 1")
      val track2 = sampleMusicTrack(title = "Track 2")
      val currentTrack = sampleMusicTrack(title = "Current")

      for {
        _ <- qm.addToQueue("guild123", track1)
        _ <- qm.addToQueue("guild123", track2)
        _ <- qm.setCurrentTrack("guild123", Some(currentTrack))
        _ <- qm.clearQueue("guild123")
        queue <- qm.getQueue("guild123")
      } yield {
        assert(queue.tracks.isEmpty, "Queue should be empty")
        assert(queue.currentTrack.isEmpty, "Current track should be cleared")
      }
    }
  }

  test("updatePlaybackState should update playing state") {
    QueueManager.make[IO].use { qm =>
      for {
        _ <- qm.updatePlaybackState("guild123", isPlaying = true)
        queue1 <- qm.getQueue("guild123")
        _ <- qm.updatePlaybackState("guild123", isPlaying = false)
        queue2 <- qm.getQueue("guild123")
      } yield {
        assert(queue1.isPlaying, "Should be playing")
        assert(!queue2.isPlaying, "Should not be playing")
      }
    }
  }

  test("updatePlaybackState should update paused state") {
    QueueManager.make[IO].use { qm =>
      for {
        _ <- qm.updatePlaybackState(
          "guild123",
          isPlaying = true,
          isPaused = true
        )
        queue1 <- qm.getQueue("guild123")
        _ <- qm.updatePlaybackState(
          "guild123",
          isPlaying = true,
          isPaused = false
        )
        queue2 <- qm.getQueue("guild123")
      } yield {
        assert(queue1.isPaused, "Should be paused")
        assert(!queue2.isPaused, "Should not be paused")
      }
    }
  }

  test("updatePlaybackState should update position and timestamps") {
    QueueManager.make[IO].use { qm =>
      val startTime = System.currentTimeMillis()
      val pauseTime = System.currentTimeMillis() + 5000

      for {
        _ <- qm.updatePlaybackState(
          "guild123",
          isPlaying = true,
          currentPosition = 120,
          startTime = Some(startTime),
          pauseTime = Some(pauseTime)
        )
        queue <- qm.getQueue("guild123")
      } yield {
        assertEquals(queue.currentPosition, 120)
        assertEquals(queue.startTime, Some(startTime))
        assertEquals(queue.pauseTime, Some(pauseTime))
      }
    }
  }

  test("isPaused should return paused state") {
    QueueManager.make[IO].use { qm =>
      for {
        paused1 <- qm.isPaused("guild123")
        _ <- qm.updatePlaybackState(
          "guild123",
          isPlaying = true,
          isPaused = true
        )
        paused2 <- qm.isPaused("guild123")
        _ <- qm.updatePlaybackState(
          "guild123",
          isPlaying = true,
          isPaused = false
        )
        paused3 <- qm.isPaused("guild123")
      } yield {
        assert(!paused1, "Should not be paused initially")
        assert(paused2, "Should be paused after update")
        assert(!paused3, "Should not be paused after second update")
      }
    }
  }

  test("getCurrentPosition should return static position when paused") {
    QueueManager.make[IO].use { qm =>
      for {
        _ <- qm.updatePlaybackState(
          "guild123",
          isPlaying = true,
          isPaused = true,
          currentPosition = 60
        )
        position <- qm.getCurrentPosition("guild123")
      } yield {
        assertEquals(position, 60, "Position should be static when paused")
      }
    }
  }

  test("different guilds should have independent queues") {
    QueueManager.make[IO].use { qm =>
      val track1 = sampleMusicTrack(title = "Guild1 Track")
      val track2 = sampleMusicTrack(title = "Guild2 Track")

      for {
        _ <- qm.addToQueue("guild1", track1)
        _ <- qm.addToQueue("guild2", track2)
        queue1 <- qm.getQueue("guild1")
        queue2 <- qm.getQueue("guild2")
      } yield {
        assertEquals(queue1.tracks.length, 1)
        assertEquals(queue2.tracks.length, 1)
        assertEquals(queue1.tracks.head.title, "Guild1 Track")
        assertEquals(queue2.tracks.head.title, "Guild2 Track")
      }
    }
  }

  test("playNext should handle queue becoming empty") {
    QueueManager.make[IO].use { qm =>
      val track = sampleMusicTrack(title = "Only Track")

      for {
        _ <- qm.addToQueue("guild123", track)
        next1 <- qm.playNext("guild123")
        next2 <- qm.playNext("guild123")
        queue <- qm.getQueue("guild123")
      } yield {
        assert(next1.isDefined, "First call should return track")
        assert(next2.isEmpty, "Second call should return None")
        assert(queue.tracks.isEmpty, "Queue should be empty")
      }
    }
  }

  test("complex queue scenario: add, play, pause, resume") {
    QueueManager.make[IO].use { qm =>
      val track1 = sampleMusicTrack(title = "Track 1")
      val track2 = sampleMusicTrack(title = "Track 2")
      val track3 = sampleMusicTrack(title = "Track 3")

      for {
        // Add tracks
        _ <- qm.addToQueue("guild123", track1)
        _ <- qm.addToQueue("guild123", track2)
        _ <- qm.addToQueue("guild123", track3)

        // Start playing first track
        next1 <- qm.playNext("guild123")
        _ <- qm.updatePlaybackState(
          "guild123",
          isPlaying = true,
          startTime = Some(System.currentTimeMillis())
        )
        queue1 <- qm.getQueue("guild123")

        // Pause
        pauseTime = System.currentTimeMillis()
        _ <- qm.updatePlaybackState(
          "guild123",
          isPlaying = true,
          isPaused = true,
          pauseTime = Some(pauseTime)
        )
        queue2 <- qm.getQueue("guild123")

        // Resume
        _ <- qm.updatePlaybackState(
          "guild123",
          isPlaying = true,
          isPaused = false,
          pauseTime = None
        )
        queue3 <- qm.getQueue("guild123")

        // Play next track
        next2 <- qm.playNext("guild123")
        queue4 <- qm.getQueue("guild123")
      } yield {
        // After adding 3 tracks and playing 1
        assertEquals(queue1.tracks.length, 2)
        assertEquals(queue1.currentTrack.get.title, "Track 1")
        assert(queue1.isPlaying)

        // After pausing
        assert(queue2.isPaused)
        assert(queue2.pauseTime.isDefined)

        // After resuming
        assert(!queue3.isPaused)
        assert(queue3.pauseTime.isEmpty)

        // After playing next
        assertEquals(queue4.tracks.length, 1)
        assertEquals(queue4.currentTrack.get.title, "Track 2")
      }
    }
  }
}
