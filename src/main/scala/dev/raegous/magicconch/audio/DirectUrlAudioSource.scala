package dev.raegous.magicconch.audio

import cats.implicits.*
import cats.effect.*
import org.typelevel.log4cats.Logger

/**
 * Audio source for direct URLs to audio files (MP3, WAV, OGG, etc.)
 */
class DirectUrlAudioSource[F[_]: Async](using logger: Logger[F]) extends AudioSource[F] {

  private val audioExtensions = List(
    ".mp3", ".wav", ".ogg", ".flac",
    ".m4a", ".aac", ".opus", ".webm"
  )

  override def canHandle(url: String): Boolean = {
    val lowerUrl = url.toLowerCase
    (lowerUrl.startsWith("http://") || lowerUrl.startsWith("https://")) &&
    audioExtensions.exists(ext => lowerUrl.contains(ext))
  }

  override def extractTrackInfo(url: String): F[Option[AudioTrackInfo]] = {
    if (!canHandle(url)) {
      Async[F].pure(None)
    } else {
      for {
        _ <- logger.info(s"[DirectUrlAudioSource] Extracting info for direct audio URL")
        filename = url.split("/").lastOption.getOrElse("Audio File")
        title = java.net.URLDecoder.decode(filename, "UTF-8")
          .replaceAll("\\.[^.]+$", "") // Remove file extension
        trackInfo = AudioTrackInfo(
          url = url,
          title = title,
          duration = None,
          source = "Direct URL"
        )
        _ <- logger.info(s"[DirectUrlAudioSource] Extracted: $title")
      } yield Some(trackInfo)
    }
  }

  override def getStreamUrl(url: String): F[Option[String]] = {
    Option.when(canHandle(url))(url)
      .traverse(url =>
        logger.info(s"[DirectUrlAudioSource] Using direct URL for streaming").as(url)
      )
  }

  override def extractPlaylistUrls(url: String): F[List[String]] = {
    // Direct URLs don't support playlists
    Async[F].pure(List.empty)
  }
}
