package dev.raegous.magicconch.audio

import cats.effect.*

/** Represents metadata about an audio track
  */
case class AudioTrackInfo(
    url: String,
    title: String,
    duration: Option[Int],
    source: String // e.g., "YouTube", "Direct URL", "Spotify"
)

/** Base trait for audio sources that can extract track information and provide
  * streamable URLs
  */
trait AudioSource[F[_]] {

  /** Check if this source can handle the given URL
    */
  def canHandle(url: String): Boolean

  /** Extract track information from the URL
    */
  def extractTrackInfo(url: String): F[Option[AudioTrackInfo]]

  /** Get a streamable audio URL (may be the same as input for direct URLs)
    */
  def getStreamUrl(url: String): F[Option[String]]

  /** Extract individual track URLs from a playlist URL Returns empty list if
    * source doesn't support playlists or URL is not a playlist
    */
  def extractPlaylistUrls(url: String): F[List[String]]
}

/** Manages multiple audio sources and routes URLs to the appropriate handler
  */
class AudioSourceManager[F[_]: Async](
    sources: List[AudioSource[F]] // Injected list of sources
) {

  /** Find the first source that can handle the given URL
    */
  private def findSource(url: String): Option[AudioSource[F]] = {
    sources.find(_.canHandle(url))
  }

  /** Extract track information using the appropriate source
    */
  def extractTrackInfo(url: String): F[Option[AudioTrackInfo]] = {
    findSource(url) match {
      case Some(source) => source.extractTrackInfo(url)
      case None         => Async[F].pure(None)
    }
  }

  /** Get streamable URL using the appropriate source
    */
  def getStreamUrl(url: String): F[Option[String]] = {
    findSource(url) match {
      case Some(source) => source.getStreamUrl(url)
      case None         => Async[F].pure(None)
    }
  }

  /** Extract playlist URLs using the appropriate source Returns empty list if
    * no source can handle the URL or source doesn't support playlists
    */
  def extractPlaylistUrls(url: String): F[List[String]] = {
    findSource(url) match {
      case Some(source) => source.extractPlaylistUrls(url)
      case None         => Async[F].pure(List.empty)
    }
  }
}

object AudioSourceManager {

  /** Create AudioSourceManager with default sources (Direct URL, YouTube)
    */
  def make[F[_]: Async: fs2.io.process.Processes](using
      logger: org.typelevel.log4cats.Logger[F]
  ): AudioSourceManager[F] = {
    val sources = List(
      new DirectUrlAudioSource[F](),
      new YouTubeAudioSource[F]()
    )
    new AudioSourceManager[F](sources)
  }
}
