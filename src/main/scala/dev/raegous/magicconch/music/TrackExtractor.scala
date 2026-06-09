package dev.raegous.magicconch.music

import cats.data.OptionT
import cats.effect.*
import cats.implicits.*
import cats.syntax.all.*
import cats.effect.implicits.*
import org.typelevel.log4cats.Logger
import fs2.io.process.Processes
import dev.raegous.magicconch.audio.*
import dev.raegous.magicconch.discord.MusicTrack

/** Extracts track information from URLs
  *
  * Responsibilities:
  *   - Extract track metadata (title, duration, etc.)
  *   - Get streamable URLs for playback
  *   - Support multiple audio sources (YouTube, direct URLs, etc.)
  */
class TrackExtractor[F[_]: Async: Processes](
    audioSourceManager: AudioSourceManager[F]
)(using logger: Logger[F]) {

  /** Extract track information from a URL
    */
  def extractTrackInfo(url: String): F[Option[MusicTrack]] = {
    OptionT(audioSourceManager.extractTrackInfo(url))
      .map(info =>
        MusicTrack(
          url = info.url,
          title = info.title,
          duration = info.duration,
          requestedBy = "Unknown"
        )
      )
      .value
  }

  /** Get streamable URL for playback
    */
  def getStreamUrl(url: String): F[Option[String]] = {
    audioSourceManager.getStreamUrl(url)
  }

  /** Extract all track URLs from a playlist (uses appropriate source handler)
    */
  def extractPlaylistUrls(playlistUrl: String): F[List[String]] = {
    audioSourceManager.extractPlaylistUrls(playlistUrl)
  }
}

object TrackExtractor {

  /** Create TrackExtractor with injected dependencies
    */
  def make[F[_]: Async: Processes](using Logger[F]): TrackExtractor[F] = {
    val audioSourceManager = AudioSourceManager.make[F]
    new TrackExtractor[F](audioSourceManager)
  }
}
