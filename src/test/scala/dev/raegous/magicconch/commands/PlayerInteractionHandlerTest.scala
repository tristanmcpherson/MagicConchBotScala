package dev.raegous.magicconch.commands

import cats.effect.*
import cats.effect.unsafe.implicits.global
import dev.raegous.magicconch.*
import dev.raegous.magicconch.TestFixtures.{testLogger, *}
import dev.raegous.magicconch.discord.*
import munit.CatsEffectSuite
import com.bdmendes.smockito.*
import com.bdmendes.smockito.given
import dev.raegous.magicconch.audio.VoiceManager
import dev.raegous.magicconch.guilds.GuildSettingsManager
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.verify
import sttp.ws.WebSocket

class PlayerInteractionHandlerTest extends CatsEffectSuite, Smockito {

  def createMocks(): (Mock[VoiceManager[IO]], Mock[DiscordApiClient[IO]], Mock[GuildSettingsManager[IO]]) = {
    val voiceManager = mock[VoiceManager[IO]]
    val discordApi = mock[DiscordApiClient[IO]]
    val guildSettings = mock[GuildSettingsManager[IO]]

    (voiceManager, discordApi, guildSettings)
  }

  test("canHandle should return true for player control custom_ids") {
    val (voiceManager, discordApi, guildSettings) = createMocks()
    val handler = new PlayerInteractionHandler[IO](voiceManager, discordApi, guildSettings)

    assert(handler.canHandle("volume_user123_inc10"))
    assert(handler.canHandle("volume_user123_50"))
    assert(handler.canHandle("player_pause_guild123"))
    assert(handler.canHandle("player_resume_guild123"))
    assert(handler.canHandle("player_stop_guild123"))
    assert(handler.canHandle("player_skip_guild123"))
    assert(!handler.canHandle("search_select_user123_0"))
    assert(!handler.canHandle("other_button"))
  }

  test("handle volume increase should increase volume by 10%") {
    val (voiceManager, discordApi, guildSettings) = createMocks()

    // Setup mocks
    guildSettings
      .on(it.getVolume)(_ => IO.pure(0.5))
      .on(it.setVolume)((_) => IO.unit)

    discordApi
      .on(it.sendInteractionResponse)((_, _, _) => IO.unit)

    val handler = new PlayerInteractionHandler[IO](voiceManager, discordApi, guildSettings)

    val interaction = sampleInteraction(
      customId = Some("volume_user123_inc10"),
      guildId = Some("guild123")
    )

    handler.handle("volume_user123_inc10", interaction, None).map { _ =>
      // Verify volume was set to 0.6 (0.5 + 0.1)
      verify(guildSettings).setVolume("guild123", 0.6)
      verify(discordApi).sendInteractionResponse(any[String], any[String], any[String])
    }
  }

  test("handle volume decrease should decrease volume by 10%") {
    val voiceManager = mock[VoiceManager[IO]]
    val discordApi = mock[DiscordApiClient[IO]]
    val guildSettings = mock[GuildSettingsManager[IO]]

    guildSettings
      .on(it.getVolume)(_ => IO.pure(0.5))
      .on(it.setVolume)((_, _) => IO.unit)

    discordApi
      .on(it.sendInteractionResponse)((_, _, _) => IO.unit)

    val handler = new PlayerInteractionHandler[IO](voiceManager, discordApi, guildSettings)

    val interaction = sampleInteraction(
      customId = Some("volume_user123_dec10"),
      guildId = Some("guild123")
    )

    handler.handle("volume_user123_dec10", interaction, None).map { _ =>
      verify(guildSettings).setVolume("guild123", 0.4)
      verify(discordApi).sendInteractionResponse(any[String], any[String], any[String])
    }
  }

  test("handle volume should clamp at 0% and 200%") {
    val (voiceManager, discordApi, guildSettings) = createMocks()

    discordApi
      .on(it.sendInteractionResponse)((_, _, _) => IO.unit)

    val handler = new PlayerInteractionHandler[IO](voiceManager, discordApi, guildSettings)

    for {
      // Test max clamp
      _ <- IO {
        guildSettings
          .on(it.getVolume)(_ => IO.pure(1.95))
          .on(it.setVolume)((_, _) => IO.unit)
      }
      _ <- handler.handle("volume_user123_inc10", sampleInteraction(guildId = Some("guild123")), None)
      _ <- IO { verify(guildSettings).setVolume("guild123", 2.0) } // Should clamp at 2.0

      // Test min clamp
      _ <- IO {
        guildSettings
          .on(it.getVolume)(_ => IO.pure(0.05))
      }
      _ <- handler.handle("volume_user123_dec10", sampleInteraction(guildId = Some("guild123")), None)
      _ <- IO { verify(guildSettings).setVolume("guild123", 0.0) } // Should clamp at 0.0
    } yield ()
  }

  test("handle volume with numeric value should set exact volume") {
    val (voiceManager, discordApi, guildSettings) = createMocks()

    guildSettings
      .on(it.getVolume)(_ => IO.pure(0.5))
      .on(it.setVolume)((_, _) => IO.unit)

    discordApi
      .on(it.sendInteractionResponse)((_, _, _) => IO.unit)

    val handler = new PlayerInteractionHandler[IO](voiceManager, discordApi, guildSettings)

    val interaction = sampleInteraction(
      customId = Some("volume_user123_75"),
      guildId = Some("guild123")
    )

    handler.handle("volume_user123_75", interaction, None).map { _ =>
      verify(guildSettings).setVolume("guild123", 0.75)
    }
  }

  test("handle pause should pause playback") {
    val (voiceManager, discordApi, guildSettings) = createMocks()

    var pausedState = false

    voiceManager
      .on(it.isPaused)(_ => IO.pure(pausedState))
      .on(it.pausePlayback)(_ => IO {
        pausedState = true
      })

    discordApi
      .on(it.sendInteractionResponse)(_ => IO.unit)

    val handler = new PlayerInteractionHandler[IO](voiceManager, discordApi, guildSettings)

    val interaction = sampleInteraction(
      customId = Some("player_pause_guild123"),
      guildId = Some("guild123")
    )

    handler.handle("player_pause_guild123", interaction, None).map { _ =>
      verify(voiceManager).pausePlayback("guild123")
      verify(discordApi).sendInteractionResponse(any[String], any[String], any[String])
    }
  }

  test("handle resume should resume playback") {
    val (voiceManager, discordApi, guildSettings) = createMocks()

    var pausedState = true

    voiceManager
      .on(it.isPaused)(_ => IO.pure(pausedState))
      .on(it.resumePlayback)(_ => IO {
        pausedState = false
      })

    discordApi
      .on(it.sendInteractionResponse)(_ => IO.unit)

    val handler = new PlayerInteractionHandler[IO](voiceManager, discordApi, guildSettings)

    val interaction = sampleInteraction(
      customId = Some("player_resume_guild123"),
      guildId = Some("guild123")
    )

    handler.handle("player_resume_guild123", interaction, None).map { _ =>
      verify(voiceManager).resumePlayback("guild123")
      verify(discordApi).sendInteractionResponse(any[String], any[String], any[String])
    }
  }

  test("handle stop should stop playback and leave voice channel") {
    val (voiceManager, discordApi, guildSettings) = createMocks()

    voiceManager
      .on(it.stopPlayback)(_ => IO.unit)

    discordApi
      .on(it.sendInteractionResponse)((_, _, _) => IO.unit)

    val handler = new PlayerInteractionHandler[IO](voiceManager, discordApi, guildSettings)

    val interaction = sampleInteraction(
      customId = Some("player_stop_guild123"),
      guildId = Some("guild123")
    )

    // Note: WebSocket is None, so leave won't be called
    handler.handle("player_stop_guild123", interaction, None).map { _ =>
      verify(voiceManager).stopPlayback("guild123")
      verify(discordApi).sendInteractionResponse(any[String], any[String], any[String])
    }
  }

  test("handle skip should skip to next track") {
    val (voiceManager, discordApi, guildSettings) = createMocks()

    val queueBeforeSkip = sampleMusicQueue(
      tracks = List(sampleMusicTrack(title = "Next Track")),
      currentTrack = Some(sampleMusicTrack(title = "Current Track")),
      isPlaying = true
    )

    val queueAfterSkip = sampleMusicQueue(
      tracks = List.empty,
      currentTrack = Some(sampleMusicTrack(title = "Next Track")),
      isPlaying = true
    )

    var currentQueue = queueBeforeSkip

    voiceManager
      .on(it.getQueue)(_ => IO.pure(currentQueue))
      .on(it.skipTrack)(_ => IO {
        currentQueue = queueAfterSkip
      })

    discordApi
      .on(it.sendInteractionResponse)(_ => IO.unit)

    val handler = new PlayerInteractionHandler[IO](voiceManager, discordApi, guildSettings)

    val interaction = sampleInteraction(
      customId = Some("player_skip_guild123"),
      guildId = Some("guild123")
    )

    handler.handle("player_skip_guild123", interaction, None).map { _ =>
      verify(voiceManager).skipTrack("guild123")
      verify(discordApi).sendInteractionResponse(any[String], any[String], any[String])
    }
  }

  test("handle should gracefully handle invalid custom_id format") {
    val (voiceManager, discordApi, guildSettings) = createMocks()

    val handler = new PlayerInteractionHandler[IO](voiceManager, discordApi, guildSettings)

    val interaction = sampleInteraction(customId = Some("player_pause")) // Missing guild ID

    // Should not throw exception, just log error
    handler.handle("player_pause", interaction, None).attempt.map { result =>
      assert(result.isRight, "Should handle invalid format gracefully")
    }
  }
}
