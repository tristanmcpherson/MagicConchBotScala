package dev.raegous.magicconch

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import sttp.ws.WebSocket
import DiscordModels.*

class MessageHandler[F[_]: Async](discordApi: DiscordApiClient[F], voiceManager: VoiceManager[F])(using Logger[F]) {
  
  def handleMessage(message: DiscordMessage, ws: WebSocket[F]): F[Unit] = {
    val isVoiceMessage = message.flags.exists(flag => (flag & 8192) != 0)
    
    val hasAudioAttachment = message.attachments.exists(_.exists(attachment =>
      attachment.content_type.exists(ct => 
        ct.startsWith("audio/") || attachment.duration_secs.isDefined
      )
    ))
    
    if (isVoiceMessage || hasAudioAttachment) {
      handleAudioMessage(message)
    } else {
      handleTextMessage(message, ws)
    }
  }
  
  private def handleTextMessage(message: DiscordMessage, ws: WebSocket[F]): F[Unit] = {
    message.content.toLowerCase match {
      case content if content.startsWith("!magicconch") =>
        val responses = List(
          "Maybe someday.",
          "Nothing.",
          "Neither.",
          "I don't think so.",
          "No.",
          "Yes.",
          "Try asking again."
        )
        val response = responses(scala.util.Random.nextInt(responses.length))
        discordApi.sendMessage(message.channel_id, response)
      case content if content.startsWith("!join") =>
        handleJoinCommand(message, ws)
      case content if content.startsWith("!leave") =>
        handleLeaveCommand(message, ws)
      case content if content.startsWith("!play ") =>
        handlePlayCommand(message, content.drop(6).trim)
      case content if content.startsWith("!stop") =>
        handleStopCommand(message)
      case content if content.startsWith("!queue") =>
        handleQueueCommand(message)
      case content if content.startsWith("!skip") =>
        handleSkipCommand(message)
      case _ => Async[F].pure(())
    }
  }
  
  private def handleJoinCommand(message: DiscordMessage, ws: WebSocket[F]): F[Unit] = {
    // For now, we'll need guild and channel IDs from the message context
    // In a real implementation, you'd extract these from the message or user's voice state
    discordApi.sendMessage(message.channel_id, "Join a voice channel and use `/play <youtube-url>` to start playing music!")
  }
  
  private def handleLeaveCommand(message: DiscordMessage, ws: WebSocket[F]): F[Unit] = {
    // Extract guild ID from message (would need to be added to DiscordMessage model)
    discordApi.sendMessage(message.channel_id, "Left the voice channel!")
  }
  
  private def handlePlayCommand(message: DiscordMessage, url: String): F[Unit] = {
    message.guild_id match {
      case Some(guildId) =>
        if (url.contains("youtube.com") || url.contains("youtu.be")) {
          for {
            trackOpt <- voiceManager.extractAudioFromYoutube(url)
            _ <- trackOpt match {
              case Some(track) =>
                val trackWithUser = track.copy(requestedBy = message.author.username)
                voiceManager.addToQueue(guildId, trackWithUser) >>
                discordApi.sendMessage(message.channel_id, s"Added to queue: ${track.title}")
              case None =>
                discordApi.sendMessage(message.channel_id, "Failed to extract audio from URL")
            }
          } yield ()
        } else {
          discordApi.sendMessage(message.channel_id, "Please provide a valid YouTube URL")
        }
      case None =>
        discordApi.sendMessage(message.channel_id, "This command only works in a server!")
    }
  }
  
  private def handleStopCommand(message: DiscordMessage): F[Unit] = {
    message.guild_id match {
      case Some(guildId) =>
        voiceManager.stopMusic(guildId) >>
        discordApi.sendMessage(message.channel_id, "Music stopped!")
      case None =>
        discordApi.sendMessage(message.channel_id, "This command only works in a server!")
    }
  }
  
  private def handleQueueCommand(message: DiscordMessage): F[Unit] = {
    message.guild_id match {
      case Some(guildId) =>
        voiceManager.getQueue(guildId).flatMap { queue =>
          val queueText = queue.currentTrack match {
            case Some(current) =>
              val upcoming = queue.tracks.take(5).zipWithIndex.map { case (track, idx) =>
                s"${idx + 1}. ${track.title} (requested by ${track.requestedBy})"
              }.mkString("\n")
              s"🎵 **Now Playing:** ${current.title}\n\n**Queue:**\n$upcoming"
            case None =>
              "No music currently playing. Use `!play <youtube-url>` to start!"
          }
          discordApi.sendMessage(message.channel_id, queueText)
        }
      case None =>
        discordApi.sendMessage(message.channel_id, "This command only works in a server!")
    }
  }
  
  private def handleSkipCommand(message: DiscordMessage): F[Unit] = {
    message.guild_id match {
      case Some(guildId) =>
        voiceManager.playNext(guildId).flatMap {
          case Some(nextTrack) =>
            discordApi.sendMessage(message.channel_id, s"⏭️ Skipped! Now playing: ${nextTrack.title}")
          case None =>
            discordApi.sendMessage(message.channel_id, "No more tracks in queue!")
        }
      case None =>
        discordApi.sendMessage(message.channel_id, "This command only works in a server!")
    }
  }
  
  private def handleAudioMessage(message: DiscordMessage): F[Unit] = {
    val responses = List(
      "I hear you loud and clear!",
      "That sounds interesting...",
      "Audio message received!",
      "I'm listening...",
      "Sound waves detected!",
      "Your voice has been noted."
    )
    val response = responses(scala.util.Random.nextInt(responses.length))
    discordApi.sendMessage(message.channel_id, response)
  }
}