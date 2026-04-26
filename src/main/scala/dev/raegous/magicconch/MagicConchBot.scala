package dev.raegous.magicconch

import cats.effect.*
import cats.implicits.*
import dev.raegous.magicconch.discord.*
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
    val envVars = for {
      token <- sys.env.get("DISCORD_TOKEN").toRight("DISCORD_TOKEN environment variable not set!")
      applicationId <- sys.env.get("DISCORD_APP_ID").toRight("DISCORD_APP_ID environment variable not set!")
    } yield (token, applicationId)

    envVars.fold(
      error => Logger[IO].error(error).as(ExitCode.Error),
      { case (token, applicationId) =>
        val youtubeApiKey = sys.env.getOrElse("YOUTUBE_API_KEY", {
          println("Warning: YOUTUBE_API_KEY environment variable not set! Search command will be disabled.")
          ""
        })

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
            _ <- (
              dashboardServer.start.use(_ => IO.never),
              connectWithRetry(client)
            ).parTupled
          } yield ExitCode.Success
        }
      }
    )
  }
}
