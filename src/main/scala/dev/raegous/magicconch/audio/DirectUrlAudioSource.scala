package dev.raegous.magicconch.audio

import cats.implicits.*
import cats.effect.*
import org.typelevel.log4cats.Logger

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

  override def extractTrackInfo(url: String): F[Option[AudioTrackInfo]] =
    Option.when(canHandle(url))(
      for {
        _ <- logger.info(s"[DirectUrlAudioSource] Extracting info for direct audio URL")
        filename = url.split("/").lastOption.getOrElse("Audio File")
        title = java.net.URLDecoder.decode(filename, "UTF-8")
          .replaceAll("\\.[^.]+$", "")
        trackInfo = AudioTrackInfo(url = url, title = title, duration = None, source = "Direct URL")
        _ <- logger.info(s"[DirectUrlAudioSource] Extracted: $title")
      } yield trackInfo.some
    ).getOrElse(Async[F].pure(none[AudioTrackInfo]))

  override def getStreamUrl(url: String): F[Option[String]] =
    Option.when(canHandle(url))(url)
      .traverse(u => logger.info(s"[DirectUrlAudioSource] Using direct URL for streaming").as(u))

  override def extractPlaylistUrls(url: String): F[List[String]] =
    Async[F].pure(List.empty)
}
