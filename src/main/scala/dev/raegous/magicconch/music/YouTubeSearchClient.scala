package dev.raegous.magicconch.music

import cats.effect.*
import cats.implicits.*
import io.circe.generic.auto.*
import io.circe.{Decoder, Encoder}
import org.http4s.*
import org.http4s.circe.*
import org.http4s.client.Client
import org.typelevel.log4cats.Logger

/** YouTube Data API v3 client for searching videos.
  *
  * API Documentation: https://developers.google.com/youtube/v3/docs/search/list
  *
  * Required API Key: Get from https://console.cloud.google.com/ Enable "YouTube
  * Data API v3" in your Google Cloud project
  */
class YouTubeSearchClient[F[_]: Async](
    apiKey: String,
    httpClient: Client[F]
)(using Logger[F]) {

  private val baseUrl = "https://www.googleapis.com/youtube/v3"

  /** Search YouTube videos by query string.
    *
    * @param query
    *   Search terms
    * @param maxResults
    *   Number of results to return (1-50, default 5)
    * @return
    *   List of search results
    */
  def search(
      query: String,
      maxResults: Int = 5
  ): F[List[YouTubeSearchResult]] = {
    val uri = Uri.unsafeFromString(
      s"$baseUrl/search?part=snippet&type=video&q=${Uri.encode(query)}&maxResults=$maxResults&key=$apiKey"
    )

    for {
      _ <- Logger[F].debug(s"[YOUTUBE] Searching: $query")
      response <- httpClient.expect[YouTubeSearchResponse](uri)(
        jsonOf[F, YouTubeSearchResponse]
      )
      _ <- Logger[F].debug(s"[YOUTUBE] Found ${response.items.length} results")
    } yield response.items.map(item =>
      YouTubeSearchResult(
        videoId = item.id.videoId,
        title = item.snippet.title,
        channelTitle = item.snippet.channelTitle,
        thumbnail = item.snippet.thumbnails.default.url,
        url = s"https://www.youtube.com/watch?v=${item.id.videoId}"
      )
    )
  }
}

// API Response Models
case class YouTubeSearchResponse(items: List[YouTubeSearchItem])

case class YouTubeSearchItem(
    id: YouTubeVideoId,
    snippet: YouTubeSnippet
)

case class YouTubeVideoId(videoId: String)

case class YouTubeSnippet(
    title: String,
    channelTitle: String,
    thumbnails: YouTubeThumbnails
)

case class YouTubeThumbnails(default: YouTubeThumbnail)

case class YouTubeThumbnail(url: String)

// User-facing result model
case class YouTubeSearchResult(
    videoId: String,
    title: String,
    channelTitle: String,
    thumbnail: String,
    url: String
)

object YouTubeSearchResult {
  given Decoder[YouTubeSearchResult] = Decoder.derived
  given Encoder[YouTubeSearchResult] = Encoder.AsObject.derived
}
