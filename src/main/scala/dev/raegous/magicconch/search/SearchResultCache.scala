package dev.raegous.magicconch.search

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import dev.raegous.magicconch.YouTubeSearchResult

/**
 * Caches YouTube search results by user ID
 *
 * Responsibilities:
 * - Store search results for users
 * - Retrieve search results for users
 * - Clear search results
 */
class SearchResultCache[F[_]: Async](
  private val searchResultsRef: Ref[F, Map[String, List[YouTubeSearchResult]]]
)(using Logger[F]) {

  /**
   * Store search results for a user
   */
  def storeResults(userId: String, results: List[YouTubeSearchResult]): F[Unit] = {
    searchResultsRef.update(_ + (userId -> results)) >>
    Logger[F].debug(s"[SEARCH] Stored ${results.length} results for user $userId")
  }

  /**
   * Get search results for a user
   */
  def getResults(userId: String): F[Option[List[YouTubeSearchResult]]] = {
    searchResultsRef.get.map(_.get(userId))
  }

  /**
   * Clear search results for a user
   */
  def clearResults(userId: String): F[Unit] = {
    searchResultsRef.update(_ - userId) >>
    Logger[F].debug(s"[SEARCH] Cleared results for user $userId")
  }

  /**
   * Clear all search results
   */
  def clearAllResults(): F[Unit] = {
    searchResultsRef.set(Map.empty) >>
    Logger[F].debug(s"[SEARCH] Cleared all results")
  }
}

object SearchResultCache {
  def make[F[_]: Async](using Logger[F]): Resource[F, SearchResultCache[F]] = {
    Resource.eval {
      Ref.of[F, Map[String, List[YouTubeSearchResult]]](Map.empty).map { ref =>
        new SearchResultCache[F](ref)
      }
    }
  }
}
