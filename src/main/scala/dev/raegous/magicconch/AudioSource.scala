package dev.raegous.magicconch

import cats.data.OptionT
import cats.effect.*
import cats.implicits.*

/**
 * Represents metadata about an audio track
 */
case class AudioTrackInfo(
  url: String,
  title: String,
  duration: Option[Int],
  source: String  // e.g., "YouTube", "Direct URL", "Spotify"
)

/**
 * Base trait for audio sources that can extract track information and provide streamable URLs
 */
trait AudioSource[F[_]] {
  /**
   * Check if this source can handle the given URL
   */
  def canHandle(url: String): Boolean

  /**
   * Extract track information from the URL
   */
  def extractTrackInfo(url: String): F[Option[AudioTrackInfo]]

  /**
   * Get a streamable audio URL (may be the same as input for direct URLs)
   */
  def getStreamUrl(url: String): F[Option[String]]
}

/**
 * Manages multiple audio sources and routes URLs to the appropriate handler
 */
class AudioSourceManager[F[_]: Async](sources: List[AudioSource[F]]) {

  /**
   * Find the first source that can handle the given URL
   */
  private def findSource(url: String): Option[AudioSource[F]] = {
    sources.find(_.canHandle(url))
  }

  /**
   * Extract track information using the appropriate source
   */
  def extractTrackInfo(url: String): F[Option[AudioTrackInfo]] = {
    findSource(url).flatTraverse(_.extractTrackInfo(url))
  }

  /**
   * Get streamable URL using the appropriate source
   */
  def getStreamUrl(url: String): F[Option[String]] = {
    findSource(url).flatTraverse(_.getStreamUrl(url))
  }
}
