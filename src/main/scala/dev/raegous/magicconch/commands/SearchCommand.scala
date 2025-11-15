package dev.raegous.magicconch.commands

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import dev.raegous.magicconch.*
import dev.raegous.magicconch.audio.VoiceManager
import dev.raegous.magicconch.discord.*
import io.circe.syntax.*
import dev.raegous.magicconch.music.YouTubeSearchClient

class SearchCommand[F[_]: Async](
  voiceManager: VoiceManager[F],
  youtubeSearch: YouTubeSearchClient[F]
)(using Logger[F]) extends Command[F] {

  val name = "search"
  val description = "Search YouTube for videos"
  val arguments = List(
    CommandArgument("query", "Search query", required = true)
  )

  def execute(context: CommandContext[F]): F[CommandResult] = {
    context.args.get("query").fold(
      Async[F].pure(CommandResult(
        message = "Please provide a search query",
        isError = true
      ))
    )(query => performSearch(query, context.userId))
  }

  private def performSearch(query: String, userId: String): F[CommandResult] = {
    for {
      _ <- Logger[F].info(s"[SEARCH] Searching YouTube for: $query")
      results <- youtubeSearch.search(query, maxResults = 5)
      _ <- voiceManager.storeSearchResults(userId, results)
      response <- buildSearchResponse(query, results, userId)
    } yield response
  }

  private def buildSearchResponse(query: String, results: List[_], userId: String): F[CommandResult] = {
    Async[F].pure(results match {
      case Nil =>
        CommandResult(
          message = s"No results found for: **$query**",
          isError = true
        )
      case searchResults =>
        val limitedResults = searchResults.take(5)

        val embed = MessageEmbed(
          title = Some(s"Search Results: $query"),
          description = Some("Click a button below to add a track to the queue"),
          color = Some(0x1DB954), // Spotify green
          fields = Some(limitedResults.zipWithIndex.map { case (result: dev.raegous.magicconch.music.YouTubeSearchResult, index) =>
            EmbedField(
              name = s"${index + 1}. ${truncate(result.title, 60)}",
              value = s"By: ${truncate(result.channelTitle, 40)}",
              inline = Some(false)
            )
          })
        )

        val buttons = limitedResults.zipWithIndex.map { case (_, index) =>
          MessageComponent(
            `type` = 2, // Button
            style = Some(1), // Primary (blue)
            label = Some(s"${index + 1}"),
            custom_id = Some(s"search_select_${userId}_$index")
          )
        }

        val actionRow = MessageComponent(
          `type` = 1, // Action Row
          components = Some(buttons)
        )

        CommandResult(
          message = s"Found ${searchResults.length} results for: **$query**",
          embeds = Some(List(embed)),
          components = Some(List(actionRow))
        )
    })
  }

  private def truncate(str: String, maxLength: Int): String = {
    if (str.length <= maxLength) str
    else str.take(maxLength - 3) + "..."
  }
}
