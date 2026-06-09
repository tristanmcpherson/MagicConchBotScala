package dev.raegous.magicconch.audio

import cats.implicits.*
import cats.effect.*
import org.typelevel.log4cats.Logger
import fs2.io.process.Processes

class YouTubeAudioSource[F[_]: Async: Processes](using logger: Logger[F])
    extends AudioSource[F] {

  private val extractor = new YtDlpExtractor[F]()

  override def canHandle(url: String): Boolean =
    url.contains("youtube.com/watch") ||
      url.contains("youtu.be/") ||
      url.contains("youtube.com/playlist") ||
      url.contains("music.youtube.com")

  override def extractTrackInfo(url: String): F[Option[AudioTrackInfo]] =
    Option
      .when(canHandle(url))(
        for {
          _ <- logger.info(
            s"[YouTubeAudioSource] Extracting info from YouTube URL"
          )
          trackOpt <- extractor.extractTrackInfo(url)
          result = trackOpt.map(track =>
            AudioTrackInfo(
              url = track.url,
              title = track.title,
              duration = track.duration,
              source = "YouTube"
            )
          )
          _ <- result.fold(
            logger.warn(s"[YouTubeAudioSource] Failed to extract track info")
          )(info =>
            logger.info(s"[YouTubeAudioSource] Extracted: ${info.title}")
          )
        } yield result
      )
      .getOrElse(Async[F].pure(none[AudioTrackInfo]))

  override def getStreamUrl(url: String): F[Option[String]] =
    Option
      .when(canHandle(url))(
        for {
          _ <- logger.info(
            s"[YouTubeAudioSource] Getting stream URL from YouTube"
          )
          streamUrl <- extractor.getAudioStreamUrl(url)
          _ <- streamUrl.fold(
            logger.warn(s"[YouTubeAudioSource] Failed to get stream URL")
          )(_ =>
            logger.info(
              s"[YouTubeAudioSource] Successfully obtained stream URL"
            )
          )
        } yield streamUrl
      )
      .getOrElse(Async[F].pure(none[String]))

  override def extractPlaylistUrls(url: String): F[List[String]] =
    Option
      .when(canHandle(url) && url.contains("playlist"))(
        for {
          _ <- logger.info(s"[YouTubeAudioSource] Extracting playlist URLs")
          urls <- extractor.extractPlaylistUrls(url)
          _ <- logger.info(
            s"[YouTubeAudioSource] Found ${urls.length} tracks in playlist"
          )
        } yield urls
      )
      .getOrElse(Async[F].pure(List.empty))
}
