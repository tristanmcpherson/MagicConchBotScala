package dev.raegous.magicconch

import cats.effect.*
import cats.effect.std.Queue
import cats.implicits.*
import org.typelevel.log4cats.Logger
import fs2.Stream
import fs2.io.process.{ProcessBuilder, Processes}
import DiscordModels.*
import io.circe.parser.*
import io.circe.Json

class YouTubeExtractor[F[_]: Async: Processes](using Logger[F]) {
  
  def extractTrackInfo(url: String): F[Option[MusicTrack]] = {
    if (!isValidYouTubeUrl(url)) {
      Logger[F].warn(s"Invalid YouTube URL: $url") >>
      Async[F].pure(None)
    } else {
      extractWithYtDlp(url)
    }
  }
  
  def getAudioStreamUrl(url: String): F[Option[String]] = {
    val ytDlpCommand = List(
      "yt-dlp",
      "--get-url",
      "--format", "bestaudio[ext=opus]/bestaudio[ext=webm]/bestaudio",
      url
    )

    runCommand(ytDlpCommand)
      .map(_.map(_.trim))
      .flatTap {
        case Some(streamUrl) => Logger[F].info(s"Extracted audio stream URL: $streamUrl")
        case None => Logger[F].error(s"Failed to extract audio stream URL from: $url")
      }
  }
  
  private def extractWithYtDlp(url: String): F[Option[MusicTrack]] = {
    val ytDlpCommand = List(
      "yt-dlp",
      "--dump-json",
      "--no-playlist",
      url
    )

    runCommand(ytDlpCommand)
      .flatMap(_.traverse(jsonOutput => parseTrackInfo(jsonOutput, url)))
      .flatTap {
        case None => Logger[F].error(s"Failed to extract info from YouTube URL: $url")
        case _ => Async[F].unit
      }
  }
  
  private def parseTrackInfo(jsonOutput: String, originalUrl: String): F[Option[MusicTrack]] = {
    parse(jsonOutput) match {
      case Right(json) =>
        val title = json.hcursor.get[String]("title").getOrElse("Unknown Title")
        val duration = json.hcursor.get[Int]("duration").toOption
        val track = MusicTrack(
          url = originalUrl,
          title = title,
          duration = duration,
          requestedBy = "system"
        )
        Logger[F].info(s"Extracted track info: $title (${duration.map(_ + "s").getOrElse("unknown duration")})") >>
        Async[F].pure(Some(track))
      case Left(error) =>
        Logger[F].error(s"Failed to parse yt-dlp JSON output: $error") >>
        Async[F].pure(None)
    }
  }
  
  private def runCommand(command: List[String]): F[Option[String]] = {
    ProcessBuilder(command.head, command.tail*)
      .spawn[F]
      .use { process =>
        for {
          output <- process.stdout.through(fs2.text.utf8.decode).compile.string
          exitCode <- process.exitValue
          result <- if (exitCode == 0) {
            Async[F].pure(Some(output))
          } else {
            Logger[F].error(s"Command failed with exit code $exitCode: ${command.mkString(" ")}") >>
            Async[F].pure(None)
          }
        } yield result
      }
      .handleErrorWith { error =>
        Logger[F].error(s"Error running command: ${command.mkString(" ")} - $error") >>
        Async[F].pure(None)
      }
  }
  
  private def isValidYouTubeUrl(url: String): Boolean = {
    url.contains("youtube.com/watch") || 
    url.contains("youtu.be/") ||
    url.contains("youtube.com/playlist") ||
    url.contains("music.youtube.com")
  }
  
  def extractPlaylistUrls(playlistUrl: String): F[List[String]] = {
    val ytDlpCommand = List(
      "yt-dlp",
      "--flat-playlist",
      "--get-url",
      playlistUrl
    )

    runCommand(ytDlpCommand)
      .map(_.fold(List.empty[String])(_.split("\n").filter(_.trim.nonEmpty).toList))
      .flatTap(urls => Logger[F].info(s"Extracted ${urls.length} URLs from playlist"))
  }
}