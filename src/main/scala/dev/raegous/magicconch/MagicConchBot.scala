package dev.raegous.magicconch

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import sttp.client4.httpclient.fs2.HttpClientFs2Backend
import org.http4s.ember.client.EmberClientBuilder

object MagicConchBot extends IOApp {

  given Logger[IO] = Slf4jLogger.getLogger[IO]

  private def connectWithRetry(client: DiscordClient[IO]): IO[Unit] = {
    import scala.concurrent.duration.*

    def attempt(retryCount: Int): IO[Unit] = {
      client.connect.handleErrorWith { error =>
        val delay = Math.min(30, Math.pow(2, retryCount)).seconds
        val errorMsg = Option(error.getMessage).getOrElse("unknown error")
        val errorType = error.getClass.getSimpleName
        Logger[IO].error(s"Discord connection failed ($errorType): $errorMsg") >>
        Logger[IO].info(s"Reconnecting in ${delay.toSeconds} seconds... (attempt ${retryCount + 1})") >>
        IO.sleep(delay) >>
        attempt(retryCount + 1)
      }
    }

    attempt(0)
  }

  def run(args: List[String]): IO[ExitCode] = {
    val token = sys.env.get("DISCORD_TOKEN") match {
      case Some(t) => t
      case None => 
        println("DISCORD_TOKEN environment variable not set!")
        return IO.pure(ExitCode.Error)
    }

    val applicationId = sys.env.get("DISCORD_APP_ID") match {
      case Some(t) => t
      case None =>
        println("DISCORD_APP_ID environment variable not set!")
        return IO.pure(ExitCode.Error)
    }

    val youtubeApiKey = sys.env.get("YOUTUBE_API_KEY") match {
      case Some(t) => t
      case None =>
        println("Warning: YOUTUBE_API_KEY environment variable not set! Search command will be disabled.")
        ""
    }

    (
      HttpClientFs2Backend.resource[IO](),
      EmberClientBuilder.default[IO].build
    ).tupled.flatMap { case (backend, httpClient) =>
      DiscordClient.make[IO](token, applicationId, youtubeApiKey, backend, httpClient)
    }.use { client =>
      val dashboardPort = sys.env.get("DASHBOARD_PORT").flatMap(_.toIntOption).getOrElse(9090)
      val dashboardServer = new DashboardServer[IO](client.voiceManager, client.guildTracker, dashboardPort)

      for {
        _ <- Logger[IO].info("Starting Magic Conch Bot...")
        _ <- Logger[IO].info(s"Dashboard will be available at http://localhost:$dashboardPort")

        // Start both Discord client and dashboard server concurrently
        _ <- (
          dashboardServer.start.use(_ => IO.never), // Keep dashboard running
          connectWithRetry(client)                   // Keep Discord client running with auto-reconnect
        ).parTupled
      } yield ExitCode.Success
    }
  }
}