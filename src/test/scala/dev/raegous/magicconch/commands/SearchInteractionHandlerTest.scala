package dev.raegous.magicconch.commands

import cats.effect.*
import cats.effect.unsafe.implicits.global
import dev.raegous.magicconch.*
import dev.raegous.magicconch.TestFixtures.{testLogger, *}
import dev.raegous.magicconch.discord.*
import dev.raegous.magicconch.music.YouTubeSearchResult
import munit.CatsEffectSuite
import com.bdmendes.smockito.*
import com.bdmendes.smockito.given
import dev.raegous.magicconch.audio.VoiceManager
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.verify
import sttp.ws.WebSocket

class SearchInteractionHandlerTest extends CatsEffectSuite, Smockito {

  def createMocks(): (Mock[VoiceManager[IO]], Mock[DiscordApiClient[IO]]) = {
    val voiceManager = mock[VoiceManager[IO]]
    val discordApi = mock[DiscordApiClient[IO]]

    (voiceManager, discordApi)
  }

  test("canHandle should return true for search_select_ custom_ids") {
    val (voiceManager, discordApi) = createMocks()
    val handler = new SearchInteractionHandler[IO](voiceManager, discordApi, "app_123")

    assert(handler.canHandle("search_select_user123_0"))
    assert(handler.canHandle("search_select_user456_1"))
    assert(!handler.canHandle("player_pause_guild123"))
    assert(!handler.canHandle("volume_user123_inc10"))
  }

  test("handle should process valid search selection") {
    val (voiceManager, discordApi) = createMocks()

    val searchResults = List(
      sampleYouTubeSearchResult(videoId = "video1", title = "Test Video 1"),
      sampleYouTubeSearchResult(videoId = "video2", title = "Test Video 2")
    )
    val testTrack = sampleMusicTrack(title = "Test Track", url = "https://youtube.com/watch?v=video1")
    val emptyQueue = sampleMusicQueue()

    // Setup mock behavior
    voiceManager
      .on(it.getSearchResults)((_) => IO.pure(Some(searchResults)))
      .on(it.extractAudioFromYoutube)((_) => IO.pure(Some(testTrack)))
      .on(it.getQueue)((_) => IO.pure(emptyQueue))
      .on(it.addToQueue)((_, _) => IO.unit)
      .on(it.clearSearchResults)((_) => IO.unit)

    discordApi
      .on(it.sendInteractionResponse)((_, _, _) => IO.unit)
      .on(it.editInteractionResponse)((_, _, _) => IO.unit)
      .on(it.editRichInteractionResponse)((_, _, _, _, _) => IO.unit)

    val handler = new SearchInteractionHandler[IO](voiceManager, discordApi, "app_123")

    val interaction = sampleInteraction(
      id = "interaction_123",
      customId = Some("search_select_user123_0"),
      guildId = Some("guild123"),
      userId = "user123"
    )

    handler.handle("search_select_user123_0", interaction, None).map { _ =>
      // Verify that search results were retrieved
      verify(voiceManager).getSearchResults("user123")

      // Verify that track was extracted from YouTube
      verify(voiceManager).extractAudioFromYoutube(any[String])

      // Verify that track was added to queue
      verify(voiceManager).addToQueue(any[String], any[MusicTrack])

      // Verify that search results were cleared
      verify(voiceManager).clearSearchResults("user123")

      // Verify that responses were sent
      verify(discordApi).sendInteractionResponse(any[String], any[String], any[String])
      verify(discordApi).editRichInteractionResponse(any[String], any[String], any[String], any[Option[List[MessageEmbed]]], any[Option[List[MessageComponent]]])
    }
  }

  test("handle should send error for expired search results") {
    val (voiceManager, discordApi) = createMocks()

    // Setup mock to return no results (expired)
    voiceManager
      .on(it.getSearchResults)((_) => IO.pure(None))

    discordApi
      .on(it.sendInteractionResponse)((_, _, _) => IO.unit)

    val handler = new SearchInteractionHandler[IO](voiceManager, discordApi, "app_123")

    val interaction = sampleInteraction(
      customId = Some("search_select_user123_0"),
      userId = "user123"
    )

    handler.handle("search_select_user123_0", interaction, None).map { _ =>
      // Verify that expired response was sent
      verify(discordApi).sendInteractionResponse(any[String], any[String], any[String])
    }
  }

  test("handle should send error for invalid index") {
    val (voiceManager, discordApi) = createMocks()

    val searchResults = List(
      sampleYouTubeSearchResult(videoId = "video1", title = "Test Video 1")
    )

    voiceManager
      .on(it.getSearchResults)(_ => IO.pure(Some(searchResults)))

    discordApi
      .on(it.sendInteractionResponse)(_ => IO.unit)

    val handler = new SearchInteractionHandler[IO](voiceManager, discordApi, "app_123")

    val interaction = sampleInteraction(
      customId = Some("search_select_user123_5"), // Index out of bounds
      userId = "user123"
    )

    handler.handle("search_select_user123_5", interaction, None).map { _ =>
      // Verify that error response was sent
      verify(discordApi).sendInteractionResponse(any[String], any[String], any[String])
    }
  }

  test("handle should handle invalid custom_id format gracefully") {
    val (voiceManager, discordApi) = createMocks()
    val handler = new SearchInteractionHandler[IO](voiceManager, discordApi, "app_123")

    val interaction = sampleInteraction(
      customId = Some("search_select_invalid") // Missing parts
    )

    // Should not throw exception, just log error
    handler.handle("search_select_invalid", interaction, None).attempt.map { result =>
      assert(result.isRight, "Should handle invalid format gracefully")
    }
  }
}
