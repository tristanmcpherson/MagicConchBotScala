package dev.raegous.magicconch.commands

import cats.effect.*
import cats.effect.unsafe.implicits.global
import com.bdmendes.smockito.*
import com.bdmendes.smockito.given
import dev.raegous.magicconch.*
import dev.raegous.magicconch.MockFixtures.*
import dev.raegous.magicconch.TestFixtures.{testLogger, *}
import dev.raegous.magicconch.discord.{MusicQueue, MusicTrack}
import dev.raegous.magicconch.music.*
import munit.CatsEffectSuite
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.verify

class SearchCommandTest extends CatsEffectSuite, Smockito {
  test("SearchCommand should have correct metadata") {
    val mocks = CommandMocks[IO]()
    val command = mocks.createSearchCommand()

    assertEquals(command.name, "search")
    assertEquals(command.description, "Search YouTube for videos")
    assert(command.arguments.exists(_.name == "query"))
    assert(command.arguments.find(_.name == "query").exists(_.required))
  }

  test("execute should return error when query is missing") {
    val mocks = CommandMocks[IO]()
    val command = mocks.createSearchCommand()

    val context = CommandContext[IO](
      userId = "user123",
      guildId = "guild123",
      channelId = "channel123",
      username = "",
      gatewayWs = None,
      args = Map.empty
    )

    command.execute(context).map { result =>
      assert(result.isError, "Should return error")
      assert(result.message.contains("query"), "Error should mention query")
    }
  }

  test("execute should return error when no results found") {
    val mocks = CommandMocks[IO]()

    mocks.voiceManager
      .on(it.storeSearchResults)((_, _) => IO.unit)

    mocks.youtubeSearch
      .on(it.search)((_, _) => IO.pure(List.empty))

    val command = mocks.createSearchCommand()

    val context = CommandContext[IO](
      userId = "user123",
      guildId = "guild123",
      channelId = "channel123",
      username = "",
      gatewayWs = None,
      args = Map("query" -> "nonexistent")
    )

    command.execute(context).map { result =>
      assert(result.isError, "Should return error")
      assert(
        result.message.contains("No results"),
        "Should indicate no results"
      )
    }
  }

  test("execute should return search results with buttons") {
    val mocks = CommandMocks[IO]()

    val searchResults = List(
      sampleYouTubeSearchResult(videoId = "video1", title = "Test Video 1"),
      sampleYouTubeSearchResult(videoId = "video2", title = "Test Video 2"),
      sampleYouTubeSearchResult(videoId = "video3", title = "Test Video 3")
    )

    mocks.youtubeSearch
      .on(it.search)((_, _) => IO.pure(searchResults))

    mocks.voiceManager
      .on(it.storeSearchResults)((_, _) => IO.unit)

    val command = mocks.createSearchCommand()

    val context = CommandContext[IO](
      userId = "user123",
      guildId = "guild123",
      channelId = "channel123",
      username = "",
      gatewayWs = None,
      args = Map("query" -> "test query")
    )

    command.execute(context).map { result =>
      // Check basic result
      assert(!result.isError, "Should not be an error")
      assert(
        result.message.contains("3 results"),
        "Should mention result count"
      )

      // Check embeds
      assert(result.embeds.isDefined, "Should have embeds")
      assert(result.embeds.exists(_.nonEmpty), "Should have at least one embed")

      val embed = result.embeds.get.head
      assert(
        embed.title.exists(_.contains("Search Results")),
        "Title should indicate search results"
      )
      assert(embed.fields.exists(_.length == 3), "Should have 3 result fields")

      // Check components (buttons)
      assert(result.components.isDefined, "Should have components")
      assert(
        result.components.exists(_.nonEmpty),
        "Should have at least one action row"
      )

      val actionRow = result.components.get.head
      assertEquals(actionRow.`type`, 1, "Should be an action row")
      assert(
        actionRow.components.exists(_.length == 3),
        "Should have 3 buttons"
      )

      // Verify that results were stored
      verify(mocks.voiceManager).storeSearchResults("user123", searchResults)
    }
  }

  test("execute should create buttons with correct custom_ids") {
    val mocks = CommandMocks[IO]()

    val searchResults = List(
      sampleYouTubeSearchResult(videoId = "video1", title = "Test Video 1"),
      sampleYouTubeSearchResult(videoId = "video2", title = "Test Video 2")
    )

    mocks.youtubeSearch
      .on(it.search)((_, _) => IO.pure(searchResults))

    mocks.voiceManager
      .on(it.storeSearchResults)((_, _) => IO.unit)

    val command = mocks.createSearchCommand()

    val context = CommandContext[IO](
      userId = "user123",
      guildId = "guild123",
      channelId = "channel123",
      username = "",
      gatewayWs = None,
      args = Map("query" -> "test")
    )

    command.execute(context).map { result =>
      val buttons = result.components.get.head.components.get

      // Check button custom_ids
      assertEquals(buttons(0).custom_id, Some("search_select_user123_0"))
      assertEquals(buttons(1).custom_id, Some("search_select_user123_1"))

      // Check button labels
      assertEquals(buttons(0).label, Some("1"))
      assertEquals(buttons(1).label, Some("2"))

      // Check button styles (Primary = 1)
      assert(
        buttons.forall(_.style.contains(1)),
        "All buttons should be primary style"
      )
    }
  }

  test("execute should handle up to 5 results (button limit)") {
    val mocks = CommandMocks[IO]()

    val searchResults = (1 to 10).map { i =>
      sampleYouTubeSearchResult(videoId = s"video$i", title = s"Test Video $i")
    }.toList

    mocks.youtubeSearch
      .on(it.search)((_, _) => IO.pure(searchResults))

    mocks.voiceManager
      .on(it.storeSearchResults)((_, _) => IO.unit)

    val command = mocks.createSearchCommand()

    val context = CommandContext[IO](
      userId = "user123",
      guildId = "guild123",
      channelId = "channel123",
      username = "",
      gatewayWs = None,
      args = Map("query" -> "popular")
    )

    command.execute(context).map { result =>
      // Should only create buttons for first 5 results (Discord limit)
      val buttons = result.components.get.head.components.get
      assert(buttons.length <= 5, "Should have at most 5 buttons")

      val fields = result.embeds.get.head.fields.get
      assert(fields.length <= 5, "Should have at most 5 result fields")
    }
  }

  test("execute should truncate long titles") {
    val mocks = CommandMocks[IO]()

    val longTitle = "A" * 100 // Very long title
    val searchResults = List(
      sampleYouTubeSearchResult(videoId = "video1", title = longTitle)
    )

    mocks.youtubeSearch
      .on(it.search)((_, _) => IO.pure(searchResults))

    mocks.voiceManager
      .on(it.storeSearchResults)((_, _) => IO.unit)

    val command = mocks.createSearchCommand()

    val context = CommandContext[IO](
      userId = "user123",
      guildId = "guild123",
      channelId = "channel123",
      username = "",
      gatewayWs = None,
      args = Map("query" -> "test")
    )

    command.execute(context).map { result =>
      val field = result.embeds.get.head.fields.get.head
      assert(
        field.name.length <= 63,
        "Title should be truncated (60 chars + '...')"
      )
      assert(
        field.name.endsWith("..."),
        "Truncated title should end with '...'"
      )
    }
  }
}
