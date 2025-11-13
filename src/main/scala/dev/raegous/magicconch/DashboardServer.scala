package dev.raegous.magicconch

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import org.http4s.*
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits.*
import org.http4s.server.Router
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.CORS
import org.http4s.circe.CirceEntityCodec.*
import com.comcast.ip4s.*
import io.circe.syntax.*
import io.circe.generic.auto.*
import org.http4s.headers.*

import scala.concurrent.duration.*

/**
 * Web dashboard API server for monitoring and controlling the bot.
 *
 * This server provides REST API endpoints only.
 * The frontend is served separately by the React dev server (Vite).
 *
 * For development:
 *   - Backend API: http://localhost:8080
 *   - Frontend UI: http://localhost:3000 (run 'cd dashboard-ui && npm run dev')
 *
 * For production:
 *   - Build the React app: 'cd dashboard-ui && npm run build'
 *   - Serve the built files from another web server (nginx, etc.)
 *   - Or update this server to serve static files from dashboard-ui/dist
 *
 * API Endpoints:
 * - GET  /api/health              - Health check
 * - GET  /api/guilds              - List all guilds
 * - GET  /api/guilds/:id/queue    - Get guild queue
 * - GET  /api/guilds/:id/playback - Get playback state
 * - POST /api/guilds/:id/skip     - Skip current track
 * - POST /api/guilds/:id/stop     - Stop playback
 * - DELETE /api/guilds/:id/queue/:index - Remove track from queue
 */
class DashboardServer[F[_]: Async](
  voiceManager: VoiceManager[F],
  guildTracker: GuildTracker[F],
  port: Int = 8080
)(using Logger[F]) {

  private val dsl = new Http4sDsl[F]{}
  import dsl.*

  // API Response Models
  case class HealthResponse(status: String, uptime: Long)
  case class GuildInfo(id: String, name: String, queueSize: Int, isPlaying: Boolean)
  case class QueueResponse(
    tracks: List[QueueTrack],
    currentTrack: Option[QueueTrack],
    isPlaying: Boolean
  )
  case class QueueTrack(
    title: String,
    url: String,
    duration: Option[Int],
    requestedBy: String
  )
  case class PlaybackResponse(
    currentTrack: Option[QueueTrack],
    isPlaying: Boolean,
    queueLength: Int
  )
  case class ErrorResponse(error: String)

  // Convert internal models to API responses
  private def toQueueTrack(track: MusicTrack): QueueTrack = {
    QueueTrack(
      title = track.title,
      url = track.url,
      duration = track.duration,
      requestedBy = track.requestedBy
    )
  }

  // API Routes
  private val apiRoutes = HttpRoutes.of[F] {
    case GET -> Root / "health" =>
      val response = HealthResponse(
        status = "ok",
        uptime = System.currentTimeMillis() / 1000
      )
      Ok(response.asJson)

    case GET -> Root / "guilds" =>
      for {
        trackedGuilds <- guildTracker.getAllGuilds
        guildInfos <- trackedGuilds.traverse { guild =>
          voiceManager.getQueue(guild.id).map { queue =>
            GuildInfo(
              id = guild.id,
              name = guild.name,
              queueSize = queue.tracks.length + (if (queue.currentTrack.isDefined) 1 else 0),
              isPlaying = queue.isPlaying
            )
          }
        }
        response <- Ok(guildInfos)
      } yield response

    case GET -> Root / "guilds" / guildId / "queue" =>
      voiceManager.getQueue(guildId).flatMap { queue =>
        val response = QueueResponse(
          tracks = queue.tracks.map(toQueueTrack),
          currentTrack = queue.currentTrack.map(toQueueTrack),
          isPlaying = queue.isPlaying
        )
        Ok(response.asJson)
      }

    case GET -> Root / "guilds" / guildId / "playback" =>
      voiceManager.getQueue(guildId).flatMap { queue =>
        val response = PlaybackResponse(
          currentTrack = queue.currentTrack.map(toQueueTrack),
          isPlaying = queue.isPlaying,
          queueLength = queue.tracks.length
        )
        Ok(response.asJson)
      }

    case POST -> Root / "guilds" / guildId / "skip" =>
      voiceManager.playNext(guildId).flatMap {
        case Some(track) =>
          Ok(Map("message" -> s"Skipped to: ${track.title}").asJson)
        case None =>
          Ok(Map("message" -> "Queue is empty").asJson)
      }

    case POST -> Root / "guilds" / guildId / "stop" =>
      voiceManager.stopMusic(guildId) >>
      voiceManager.clearQueue(guildId) >>
      Ok(Map("message" -> "Stopped playback and cleared queue").asJson)

    case DELETE -> Root / "guilds" / guildId / "queue" / IntVar(index) =>
      // TODO: Implement remove track by index
      // This requires adding a method to VoiceManager
      NotImplemented(ErrorResponse("Remove track not yet implemented").asJson)
  }

  // API-only routes (frontend served separately by Vite dev server or built React app)
  private val httpApp = Router(
    "/api" -> apiRoutes
  ).orNotFound

  // Apply CORS middleware to allow browser access
  private val httpAppWithCors = CORS.policy
    .withAllowOriginAll
    .withAllowMethodsAll
    .withAllowHeadersAll
    .apply(httpApp)

  def start: Resource[F, Unit] = {
    EmberServerBuilder
      .default[F]
      .withHost(ipv4"0.0.0.0")
      .withPort(Port.fromInt(port).getOrElse(port"8080"))
      .withHttpApp(httpAppWithCors)
      .build
      .evalMap { server =>
        Logger[F].info(s"🌐 Dashboard API server started at http://localhost:${server.address.getPort}") >>
        Logger[F].info(s"📱 Frontend: Run 'cd dashboard-ui && npm run dev' to start the React UI at http://localhost:3000")
      }
      .void
  }
}
