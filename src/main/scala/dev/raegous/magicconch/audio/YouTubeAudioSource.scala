package dev.raegous.magicconch.audio

import cats.implicits.*
import cats.effect.*
import org.typelevel.log4cats.Logger
import fs2.io.process.Processes

/**
 * Audio source for YouTube videos using yt-dlp for extraction
 */
class YouTubeAudioSource[F[_]: Async: Processes](using logger: Logger[F]) extends AudioSource[F] {

  private val extractor = new YtDlpExtractor[F]()

  override def canHandle(url: String): Boolean = {
    url.contains("youtube.com/watch") ||
    url.contains("youtu.be/") ||
    url.contains("youtube.com/playlist") ||
    url.contains("music.youtube.com")
  }

  override def extractTrackInfo(url: String): F[Option[AudioTrackInfo]] = {
    if (!canHandle(url)) {
      Async[F].pure(None)
    } else {
      for {
        _ <- logger.info(s"[YouTubeAudioSource] Extracting info from YouTube URL")
        trackOpt <- extractor.extractTrackInfo(url)
        result = trackOpt.map(track =>
          AudioTrackInfo(
            url = track.url,
            title = track.title,
            duration = track.duration,
            source = "YouTube"
          )
        )
        _ <- result match {
          case Some(info) => logger.info(s"[YouTubeAudioSource] Extracted: ${info.title}")
          case None => logger.warn(s"[YouTubeAudioSource] Failed to extract track info")
        }
      } yield result
    }
  }

  override def getStreamUrl(url: String): F[Option[String]] = {
    if (!canHandle(url)) {
      Async[F].pure(None)
    } else {
      for {
        _ <- logger.info(s"[YouTubeAudioSource] Getting stream URL from YouTube")
        streamUrl <- extractor.getAudioStreamUrl(url)
        _ <- streamUrl match {
          case Some(_) => logger.info(s"[YouTubeAudioSource] Successfully obtained stream URL")
          case None => logger.warn(s"[YouTubeAudioSource] Failed to get stream URL")
        }
      } yield streamUrl
    }
  }

  override def extractPlaylistUrls(url: String): F[List[String]] = {
    if (!canHandle(url) || !url.contains("playlist")) {
      Async[F].pure(List.empty)
    } else {
      for {
        _ <- logger.info(s"[YouTubeAudioSource] Extracting playlist URLs")
        urls <- extractor.extractPlaylistUrls(url)
        _ <- logger.info(s"[YouTubeAudioSource] Found ${urls.length} tracks in playlist")
      } yield urls
    }
  }
}
