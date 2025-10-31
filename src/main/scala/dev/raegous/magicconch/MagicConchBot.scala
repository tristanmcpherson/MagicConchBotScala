package dev.raegous.magicconch

import cats.effect.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import sttp.client4.httpclient.fs2.HttpClientFs2Backend

object MagicConchBot extends IOApp {
  
  given Logger[IO] = Slf4jLogger.getLogger[IO]
  
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

    HttpClientFs2Backend.resource[IO]().use {
      backend =>

      for {
        _ <- Logger[IO].info("Starting Magic Conch Bot...")
        client = new DiscordClient[IO](token, applicationId, backend)
        _ <- client.connect
      } yield ExitCode.Success
    }
  }
}