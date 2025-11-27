package dev.raegous.magicconch.discord

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import sttp.client4.*
import io.circe.syntax.*
import io.circe.parser.*
import sttp.client4.httpclient.HttpClientAsyncBackend
import sttp.client4.httpclient.fs2.HttpClientFs2Backend
import DiscordModels.*
import DiscordModels.given
import cats.data.{EitherT, OptionT}

object DiscordApiClient {
  private val API_BASE = "https://discord.com/api/v10"

  def messagesUrl(channelId: String): String = s"$API_BASE/channels/$channelId/messages"
  def commandsUrl(applicationId: String): String = s"$API_BASE/applications/$applicationId/commands"
  def guildCommandsUrl(applicationId: String, guildId: String): String = s"$API_BASE/applications/$applicationId/guilds/$guildId/commands"
  def interactionResponseUrl(interactionId: String, interactionToken: String): String = s"$API_BASE/interactions/$interactionId/$interactionToken/callback"
  def interactionFollowupUrl(applicationId: String, interactionToken: String): String = s"$API_BASE/webhooks/$applicationId/$interactionToken"
  def interactionEditUrl(applicationId: String, interactionToken: String): String = s"$API_BASE/webhooks/$applicationId/$interactionToken/messages/@original"
  def guildMemberUrl(guildId: String, userId: String): String = s"$API_BASE/guilds/$guildId/members/$userId"
}

class DiscordApiClient[F[_]: Async](token: String, backend: WebSocketStreamBackend[F, ?])(using Logger[F]) {
  
  def sendMessage(channelId: String, content: String): F[Unit] = {
    sendRichMessage(channelId, content, None, None)
  }

  def sendRichMessage(
    channelId: String,
    content: String,
    embeds: Option[List[MessageEmbed]] = None,
    components: Option[List[MessageComponent]] = None
  ): F[Unit] = {
    import io.circe.Printer
    import io.circe.Json
    val jsonPrinter = Printer.noSpaces.copy(dropNullValues = true)

    val messagePayload = Json.obj(
      "content" := content,
      "embeds" := embeds,
      "components" := components
    )

    val request = basicRequest
      .post(uri"${DiscordApiClient.messagesUrl(channelId)}")
      .header("Authorization", s"Bot $token")
      .header("Content-Type", "application/json")
      .body(messagePayload.printWith(jsonPrinter))

    for {
      _ <- Logger[F].info(s"Sending rich message to channel $channelId with ${embeds.map(_.size).getOrElse(0)} embeds and ${components.map(_.size).getOrElse(0)} components")
      response <- request.send(backend)
      _ <- if (response.code.isSuccess) {
        Logger[F].info(s"Rich message sent to channel $channelId")
      } else {
        Logger[F].error(s"Failed to send rich message: ${response.code} - ${response.body}")
      }
    } yield ()
  }
  
  def sendAudioFile(channelId: String, audioBytes: Array[Byte], filename: String): F[Unit] = {

    val request = basicRequest
      .post(uri"${DiscordApiClient.messagesUrl(channelId)}")
      .header("Authorization", s"Bot $token")
      .multipartBody(
        multipart("file", audioBytes).fileName(filename).contentType("audio/ogg"),
        multipart("payload_json", """{"content":"🎵 Audio response!"}""").contentType("application/json")
      )
    
    request.send(backend).flatMap { response =>
      if (response.code.isSuccess) {
        Logger[F].info(s"Audio file sent to channel $channelId: $filename")
      } else {
        Logger[F].error(s"Failed to send audio file: ${response.code} - ${response.body}")
      }
    }
  }
  
  def registerSlashCommand(applicationId: String, commandData: String): F[String] = {
    val request = basicRequest
      .post(uri"${DiscordApiClient.commandsUrl(applicationId)}")
      .header("Authorization", s"Bot $token")
      .header("Content-Type", "application/json")
      .body(commandData)
    
    request.send(backend).flatMap { response =>
      if (response.code.isSuccess) {
        Logger[F].debug(s"Slash command registered successfully: ${response.code}")
        Async[F].pure(response.body.toString)
      } else {
        Logger[F].error(s"Failed to register slash command: ${response.code} - ${response.body}")
        Async[F].pure(s"Error: ${response.code}")
      }
    }
  }
  
  def registerGuildSlashCommand(applicationId: String, guildId: String, commandData: String): F[String] = {
    def attemptRegistration(): F[String] = {
      val request = basicRequest
        .post(uri"${DiscordApiClient.guildCommandsUrl(applicationId, guildId)}")
        .header("Authorization", s"Bot $token")
        .header("Content-Type", "application/json")
        .body(commandData)
      
      request.send(backend).flatMap { response =>
        response.code.code match {
          case 200 | 201 =>
            Logger[F].debug(s"Guild slash command registered successfully: ${response.code}") >>
            Async[F].pure(response.body.toString)
          case 429 =>
            val retryAfter = response.header("Retry-After")
              .flatMap(_.toIntOption)
              .getOrElse(1)
            Logger[F].warn(s"Rate limited, retrying after ${retryAfter} seconds") >>
            Async[F].sleep(scala.concurrent.duration.Duration(retryAfter, "seconds")) >>
            attemptRegistration()
          case _ =>
            Logger[F].error(s"Failed to register guild slash command: ${response.code} - ${response.body}") >>
            Async[F].pure(s"Error: ${response.code}")
        }
      }
    }
    
    attemptRegistration()
  }
  
  def getGlobalSlashCommands(applicationId: String): F[List[SlashCommand]] = {
    val request = basicRequest
      .get(uri"${DiscordApiClient.commandsUrl(applicationId)}")
      .header("Authorization", s"Bot $token")

    var response = request.send(backend).map(a => a.body.toOption)
    OptionT(response)
      .filter(_.nonEmpty)
      .semiflatTap(body => Logger[F].debug(s"Raw global commands response: $body"))
      .subflatMap(body => decode[List[SlashCommand]](body).toOption)
      .semiflatTap(commands => Logger[F].info(s"Retrieved ${commands.length} global commands"))
      .getOrElseF(Logger[F].warn("Failed to get global commands").as(List.empty))
  }

  def getGuildSlashCommands(applicationId: String, guildId: String): F[List[SlashCommand]] = {
    val request = basicRequest
      .get(uri"${DiscordApiClient.guildCommandsUrl(applicationId, guildId)}")
      .header("Authorization", s"Bot $token")

    OptionT.liftF(request.send(backend))
      .subflatMap(_.body.toOption)
      .filter(_.nonEmpty)
      .semiflatTap(body => Logger[F].debug(s"Raw response: $body"))
      .subflatMap(body => decode[List[SlashCommand]](body).toOption)
      .semiflatTap(commands => Logger[F].info(s"Retrieved ${commands.length} commands"))
      .getOrElseF(Logger[F].warn("Failed to get commands").as(List.empty))
  }

  def registerGlobalSlashCommand(applicationId: String, commandData: String): F[String] = {
    def attemptRegistration(): F[String] = {
      val request = basicRequest
        .post(uri"${DiscordApiClient.commandsUrl(applicationId)}")
        .header("Authorization", s"Bot $token")
        .header("Content-Type", "application/json")
        .body(commandData)

      request.send(backend).flatMap { response =>
        response.code.code match {
          case 200 | 201 =>
            Logger[F].debug(s"Global slash command registered successfully: ${response.code}") >>
            Async[F].pure(response.body.toString)
          case 429 =>
            val retryAfter = response.header("Retry-After")
              .flatMap(_.toIntOption)
              .getOrElse(1)
            Logger[F].warn(s"Rate limited, retrying after ${retryAfter} seconds") >>
            Async[F].sleep(scala.concurrent.duration.Duration(retryAfter, "seconds")) >>
            attemptRegistration()
          case _ =>
            Logger[F].error(s"Failed to register global slash command: ${response.code} - ${response.body}") >>
            Async[F].pure(s"Error: ${response.code}")
        }
      }
    }

    attemptRegistration()
  }

  def deleteGlobalSlashCommand(applicationId: String, commandId: String, commandName: String): F[Unit] = {
    def attemptDelete(): F[Unit] = {
      val request = basicRequest
        .delete(uri"${DiscordApiClient.commandsUrl(applicationId)}/$commandId")
        .header("Authorization", s"Bot $token")

      request.send(backend).flatMap { response =>
        response.code.code match {
          case code if code >= 200 && code < 300 =>
            Logger[F].info(s"Deleted global command: $commandName (ID: $commandId)")
          case 429 =>
            val retryAfter = response.header("Retry-After")
              .flatMap(_.toDoubleOption)
              .map(secs => scala.concurrent.duration.Duration((secs * 1000).toLong, "milliseconds"))
              .getOrElse(scala.concurrent.duration.Duration(1, "seconds"))
            Logger[F].warn(s"Rate limited deleting $commandName, retrying after $retryAfter") >>
            Async[F].sleep(retryAfter) >>
            attemptDelete()
          case _ =>
            Logger[F].error(s"Failed to delete global command $commandName: ${response.code} - ${response.body}")
        }
      }
    }

    attemptDelete()
  }

  def deleteGuildSlashCommand(applicationId: String, guildId: String, commandId: String, commandName: String): F[Unit] = {
    def attemptDelete(): F[Unit] = {
      val request = basicRequest
        .delete(uri"${DiscordApiClient.guildCommandsUrl(applicationId, guildId)}/$commandId")
        .header("Authorization", s"Bot $token")

      request.send(backend).flatMap { response =>
        response.code.code match {
          case code if code >= 200 && code < 300 =>
            Logger[F].info(s"Deleted command: $commandName (ID: $commandId)")
          case 429 =>
            val retryAfter = response.header("Retry-After")
              .flatMap(_.toDoubleOption)
              .map(secs => scala.concurrent.duration.Duration((secs * 1000).toLong, "milliseconds"))
              .getOrElse(scala.concurrent.duration.Duration(1, "seconds"))
            Logger[F].warn(s"Rate limited deleting $commandName, retrying after $retryAfter") >>
            Async[F].sleep(retryAfter) >>
            attemptDelete()
          case _ =>
            Logger[F].error(s"Failed to delete command $commandName: ${response.code} - ${response.body}")
        }
      }
    }

    attemptDelete()
  }
  
  def getUserVoiceState(guildId: String, userId: String): F[Option[String]] = {
    val request = basicRequest
      .get(uri"${DiscordApiClient.guildMemberUrl(guildId, userId)}")
      .header("Authorization", s"Bot $token")
    
    request.send(backend).flatMap { response =>
      if (response.code.isSuccess) {
        import io.circe.parser.*
        decode[GuildMember](response.body.toString) match {
          case Right(member) =>
            // Note: Discord API doesn't include voice state in member object
            // We need to track voice states from gateway events instead
            Logger[F].info(s"Retrieved member info for user $userId")
            Async[F].pure(None) // Will implement proper voice state tracking
          case Left(error) =>
            Logger[F].error(s"Failed to parse member info: $error")
            Async[F].pure(None)
        }
      } else {
        Logger[F].error(s"Failed to get member info: ${response.code} - ${response.body}")
        Async[F].pure(None)
      }
    }
  }
  
  def sendInteractionResponse(interactionId: String, interactionToken: String, response: String): F[Unit] = {
    val request = basicRequest
      .post(uri"${DiscordApiClient.interactionResponseUrl(interactionId, interactionToken)}")
      .header("Content-Type", "application/json")
      .body(response)

    for {
      httpResponse <- request.send(backend)
      _ <- if (httpResponse.code.isSuccess) {
        Logger[F].info(s"Interaction response sent successfully: ${httpResponse.code}")
      } else {
        Logger[F].error(s"Failed to send interaction response: ${httpResponse.code} - ${httpResponse.body}")
      }
    } yield ()
  }

  def editInteractionResponse(applicationId: String, interactionToken: String, content: String): F[Unit] = {
    editRichInteractionResponse(applicationId, interactionToken, content, None, None)
  }

  def editRichInteractionResponse(
    applicationId: String,
    interactionToken: String,
    content: String,
    embeds: Option[List[MessageEmbed]] = None,
    components: Option[List[MessageComponent]] = None
  ): F[Unit] = {
    import io.circe.Printer
    import io.circe.Json
    val jsonPrinter = Printer.noSpaces.copy(dropNullValues = true)

    val payload = Json.obj(
      "content" := content,
      "embeds" := embeds,
      "components" := components
    )

    val request = basicRequest
      .patch(uri"${DiscordApiClient.interactionEditUrl(applicationId, interactionToken)}")
      .header("Content-Type", "application/json")
      .body(payload.printWith(jsonPrinter))

    for {
      _ <- Logger[F].info(s"Editing interaction response with content: $content")
      httpResponse <- request.send(backend)
      _ <- if (httpResponse.code.isSuccess) {
        Logger[F].info(s"Interaction edit successful: ${httpResponse.code}")
      } else {
        Logger[F].error(s"Failed to edit interaction: ${httpResponse.code} - ${httpResponse.body}")
      }
    } yield ()
  }
}