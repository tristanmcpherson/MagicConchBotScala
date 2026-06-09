package dev.raegous.magicconch.search

import cats.effect.*
import cats.implicits.*
import dev.raegous.magicconch.music.YouTubeSearchResult
import org.typelevel.log4cats.Logger

class SearchResultCache[F[_]: Async](
    private val searchResultsRef: Ref[F, Map[String, List[YouTubeSearchResult]]]
)(using Logger[F]) {

  def storeResults(
      userId: String,
      results: List[YouTubeSearchResult]
  ): F[Unit] =
    searchResultsRef.update(_ + (userId -> results)) >>
      Logger[F].debug(
        s"[SEARCH] Stored ${results.length} results for user $userId"
      )

  def getResults(userId: String): F[Option[List[YouTubeSearchResult]]] =
    searchResultsRef.get.map(_.get(userId))

  def clearResults(userId: String): F[Unit] =
    searchResultsRef.update(_ - userId) >>
      Logger[F].debug(s"[SEARCH] Cleared results for user $userId")

  def clearAllResults(): F[Unit] =
    searchResultsRef.set(Map.empty) >>
      Logger[F].debug(s"[SEARCH] Cleared all results")
}

object SearchResultCache {
  def make[F[_]: Async](using Logger[F]): Resource[F, SearchResultCache[F]] =
    Resource.eval(
      Ref
        .of[F, Map[String, List[YouTubeSearchResult]]](Map.empty)
        .map(new SearchResultCache[F](_))
    )
}
