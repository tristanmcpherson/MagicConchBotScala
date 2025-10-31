package dev.raegous.magicconch

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import sttp.client4.*
import io.circe.syntax.*
import sttp.client4.httpclient.HttpClientAsyncBackend
import sttp.client4.httpclient.fs2.HttpClientFs2Backend
import DiscordModels.given

object DiscordApiClient {
  private val API_BASE = "https://discord.com/api/v10"
  
  def messagesUrl(channelId: String): String = s"$API_BASE/channels/$channelId/messages"
  def commandsUrl(applicationId: String): String = s"$API_BASE/applications/$applicationId/commands"
  def guildCommandsUrl(applicationId: String, guildId: String): String = s"$API_BASE/applications/$applicationId/guilds/$guildId/commands"
  def interactionResponseUrl(interactionId: String, interactionToken: String): String = s"$API_BASE/interactions/$interactionId/$interactionToken/callback"
  def guildMemberUrl(guildId: String, userId: String): String = s"$API_BASE/guilds/$guildId/members/$userId"
}

class DiscordApiClient[F[_]: Async](token: String, backend: WebSocketStreamBackend[F, ?])(using Logger[F]) {
  
  def sendMessage(channelId: String, content: String): F[Unit] = {
    val messagePayload = Map("content" -> content).asJson
    val request = basicRequest
      .post(uri"${DiscordApiClient.messagesUrl(channelId)}")
      .header("Authorization", s"Bot $token")
      .header("Content-Type", "application/json")
      .body(messagePayload.noSpaces)
    
    request.send(backend).flatMap { response =>
      if (response.code.isSuccess) {
        Logger[F].info(s"Message sent to channel $channelId: $content")
      } else {
        Logger[F].error(s"Failed to send message: ${response.code} - ${response.body}")
      }
    }
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
        Logger[F].info(s"Slash command registered successfully: ${response.code}")
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
            Logger[F].info(s"Guild slash command registered successfully: ${response.code}")
            Async[F].pure(response.body.toString)
          case 429 =>
            val retryAfter = response.header("Retry-After")
              .flatMap(_.toIntOption)
              .getOrElse(1)
            Logger[F].warn(s"Rate limited, retrying after ${retryAfter} seconds")
            Async[F].sleep(scala.concurrent.duration.Duration(retryAfter, "seconds")) >>
            attemptRegistration()
          case _ =>
            Logger[F].error(s"Failed to register guild slash command: ${response.code} - ${response.body}")
            Async[F].pure(s"Error: ${response.code}")
        }
      }
    }
    
    attemptRegistration()
  }
  
  def getGuildSlashCommands(applicationId: String, guildId: String): F[List[SlashCommand]] = {
    val request = basicRequest
      .get(uri"${DiscordApiClient.guildCommandsUrl(applicationId, guildId)}")
      .header("Authorization", s"Bot $token")
    
    request.send(backend).flatMap { response =>
      if (response.code.isSuccess) {
        import io.circe.parser.*
        decode[List[SlashCommand]](response.body.toString) match {
          case Right(commands) => 
            Logger[F].info(s"Retrieved ${commands.length} existing guild commands")
            Async[F].pure(commands)
          case Left(error) =>
            Logger[F].error(s"Failed to parse existing commands: $error")
            Async[F].pure(List.empty)
        }
      } else {
        Logger[F].error(s"Failed to get existing guild commands: ${response.code} - ${response.body}")
        Async[F].pure(List.empty)
      }
    }
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
    
    request.send(backend).flatMap { httpResponse =>
      if (httpResponse.code.isSuccess) {
        Logger[F].info(s"Interaction response sent successfully: ${httpResponse.code}")
      } else {
        Logger[F].error(s"Failed to send interaction response: ${httpResponse.code} - ${httpResponse.body}")
      }
    }
  }
}