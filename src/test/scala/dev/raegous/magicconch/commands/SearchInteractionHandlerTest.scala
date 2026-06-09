package dev.raegous.magicconch.commands

import cats.effect.*
import cats.effect.unsafe.implicits.global
import dev.raegous.magicconch.*
import dev.raegous.magicconch.MockFixtures.*
import dev.raegous.magicconch.MockFixtures.Setups.*
import dev.raegous.magicconch.TestFixtures.{testLogger, *}
import dev.raegous.magicconch.discord.*
import dev.raegous.magicconch.music.YouTubeSearchResult
import munit.CatsEffectSuite
import com.bdmendes.smockito.*

class SearchInteractionHandlerTest extends CatsEffectSuite, Smockito {

  test("canHandle should return true for search_select_ custom_ids") {
    val mocks = SearchMocks[IO]()
    val handler = mocks.createHandler()

    assert(handler.canHandle("search_select_user123_0"))
    assert(handler.canHandle("search_select_user456_1"))
    assert(!handler.canHandle("player_pause_guild123"))
    assert(!handler.canHandle("volume_user123_inc10"))
  }

  test("handle should process valid search selection") {
    val mocks = SearchMocks[IO]()

    val searchResults = List(
      sampleYouTubeSearchResult(videoId = "video1", title = "Test Video 1"),
      sampleYouTubeSearchResult(videoId = "video2", title = "Test Video 2")
    )
    val testTrack = sampleMusicTrack(
      title = "Test Track",
      url = "https://youtube.com/watch?v=video1"
    )
    val emptyQueue = sampleMusicQueue()

    // Setup mocks using utilities
    withSearchResults(mocks.voiceManager, "user123", searchResults)
    withTrackExtraction(mocks.trackExtractor, "", Some(testTrack))
    withQueue(mocks.voiceManager, "guild123", emptyQueue)
    withDiscordApi(mocks.discordApi)

    mocks.voiceManager
      .on(it.addToQueue)((_, _) => IO.pure(Right(())))

    mocks.discordApi
      .on(it.editRichInteractionResponse)((_, _, _, _, _) => IO.unit)

    val handler = mocks.createHandler()

    val interaction = sampleInteraction(
      id = "interaction_123",
      customId = Some("search_select_user123_0"),
      guildId = Some("guild123"),
      userId = "user123"
    )

    handler.handle("search_select_user123_0", interaction, None).map { _ =>
      // Verify that search results were retrieved
      assertEquals(
        mocks.voiceManager.calls(it.getSearchResults).head,
        "user123"
      )

      // Verify that track was extracted
      assertEquals(mocks.trackExtractor.times(it.extractTrackInfo), 1)

      // Verify that track was added to queue
      assertEquals(mocks.voiceManager.times(it.addToQueue), 1)

      // Verify that search results were cleared
      assertEquals(
        mocks.voiceManager.calls(it.clearSearchResults).head,
        "user123"
      )

      // Verify that responses were sent
      assertEquals(mocks.discordApi.times(it.sendInteractionResponse), 1)
      assertEquals(mocks.discordApi.times(it.editRichInteractionResponse), 1)
    }
  }

  test("handle should send error for expired search results") {
    val mocks = SearchMocks[IO]()

    // Setup mock to return no results (expired)
    mocks.voiceManager
      .on(it.getSearchResults)((_) => IO.pure(None))

    withDiscordApi(mocks.discordApi)

    val handler = mocks.createHandler()

    val interaction = sampleInteraction(
      customId = Some("search_select_user123_0"),
      userId = "user123"
    )

    handler.handle("search_select_user123_0", interaction, None).map { _ =>
      // Verify that expired response was sent
      assertEquals(mocks.discordApi.times(it.sendInteractionResponse), 1)
    }
  }

  test("handle should send error for invalid index") {
    val mocks = SearchMocks[IO]()

    val searchResults = List(
      sampleYouTubeSearchResult(videoId = "video1", title = "Test Video 1")
    )

    withSearchResults(mocks.voiceManager, "user123", searchResults)
    withDiscordApi(mocks.discordApi)

    val handler = mocks.createHandler()

    val interaction = sampleInteraction(
      customId = Some("search_select_user123_5"), // Index out of bounds
      userId = "user123"
    )

    handler.handle("search_select_user123_5", interaction, None).map { _ =>
      // Verify that error response was sent
      assertEquals(mocks.discordApi.times(it.sendInteractionResponse), 1)
    }
  }

  test("handle should handle invalid custom_id format gracefully") {
    val mocks = SearchMocks[IO]()
    val handler = mocks.createHandler()

    val interaction = sampleInteraction(
      customId = Some("search_select_invalid") // Missing parts
    )

    // Should not throw exception, just log error
    handler.handle("search_select_invalid", interaction, None).attempt.map {
      result =>
        assert(result.isRight, "Should handle invalid format gracefully")
    }
  }
}
