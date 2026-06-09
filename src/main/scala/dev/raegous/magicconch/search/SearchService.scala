package dev.raegous.magicconch.search

import cats.effect.*
import org.typelevel.log4cats.Logger
import org.http4s.client.Client
import dev.raegous.magicconch.music.{YouTubeSearchClient, YouTubeSearchResult}

/** Facade service for YouTube search operations
  *
  * Coordinates:
  *   - YouTubeSearchClient (API calls)
  *   - SearchResultCache (result storage)
  */
class SearchService[F[_]: Async](
    private val youtubeSearchClient: YouTubeSearchClient[F],
    private val searchResultCache: SearchResultCache[F]
)(using Logger[F]) {

  /** Search YouTube videos
    */
  def search(
      query: String,
      maxResults: Int = 5
  ): F[List[YouTubeSearchResult]] = {
    youtubeSearchClient.search(query, maxResults)
  }

  /** Store search results for a user
    */
  def storeResults(
      userId: String,
      results: List[YouTubeSearchResult]
  ): F[Unit] = {
    searchResultCache.storeResults(userId, results)
  }

  /** Get search results for a user
    */
  def getResults(userId: String): F[Option[List[YouTubeSearchResult]]] = {
    searchResultCache.getResults(userId)
  }

  /** Clear search results for a user
    */
  def clearResults(userId: String): F[Unit] = {
    searchResultCache.clearResults(userId)
  }
}

object SearchService {
  def make[F[_]: Async](
      apiKey: String,
      httpClient: Client[F]
  )(using Logger[F]): Resource[F, SearchService[F]] = {
    for {
      searchResultCache <- SearchResultCache.make[F]
      youtubeSearchClient = new YouTubeSearchClient[F](apiKey, httpClient)
      searchService = new SearchService[F](
        youtubeSearchClient,
        searchResultCache
      )
    } yield searchService
  }
}
