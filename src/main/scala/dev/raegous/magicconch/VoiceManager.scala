package dev.raegous.magicconch

import cats.effect.*
import org.typelevel.log4cats.Logger
import sttp.ws.WebSocket
import io.circe.syntax.*
import DiscordModels.*
import cats.implicits.*

class VoiceManager[F[_]: Async: fs2.io.process.Processes](
  initialBotUserId: String, 
  backend: sttp.client4.WebSocketStreamBackend[F, ?]
)(using Logger[F]) {
  
  private val voiceStateRef: Ref[F, Option[BotVoiceState]] = Ref.unsafe(None)
  private val botUserIdRef: Ref[F, String] = Ref.unsafe(initialBotUserId)
  private val musicQueueRef: Ref[F, Map[String, MusicQueue]] = Ref.unsafe(Map.empty) // guildId -> MusicQueue
  private val userVoiceStatesRef: Ref[F, Map[String, Option[String]]] = Ref.unsafe(Map.empty) // userId -> channelId
  private val youtubeExtractor = new YouTubeExtractor[F]()
  private val audioStreamer = new AudioStreamer[F]()
  private val voiceGateway = new VoiceGateway[F](backend, audioStreamer)
  private val activeVoiceConnections: Ref[F, Map[String, sttp.ws.WebSocket[F]]] = Ref.unsafe(Map.empty)

  // Track pending voice connections: guildId -> (sessionId, voiceToken, endpoint)
  private val pendingVoiceConnections: Ref[F, Map[String, (Option[String], Option[String], Option[String])]] =
    Ref.unsafe(Map.empty)
  
  def handleVoiceStateUpdate(voiceState: VoiceStateUpdate): F[Unit] = {
    Logger[F].info(s"Received voice state update: user=${voiceState.user_id}, channel=${voiceState.channel_id}, guild=${voiceState.guild_id}") >>
    // Update user voice state tracking
    userVoiceStatesRef.update(states =>
      states + (voiceState.user_id -> voiceState.channel_id)
    ) >>
    botUserIdRef.get.flatMap { botUserId =>
      if (voiceState.user_id == botUserId) {
        Logger[F].info(s"Bot voice state updated: sessionId=${voiceState.session_id}, channel=${voiceState.channel_id}") >>
        // Update pending connection with session ID
        pendingVoiceConnections.update { pending =>
          val (_, token, endpoint) = pending.getOrElse(voiceState.guild_id, (None, None, None))
          pending + (voiceState.guild_id -> (Some(voiceState.session_id), token, endpoint))
        } >>
        // Try to complete the connection
        attemptVoiceConnection(voiceState.guild_id)
      } else {
        Logger[F].info(s"User ${voiceState.user_id} voice state updated: channel=${voiceState.channel_id}")
      }
    }
  }
  
  def handleVoiceServerUpdate(voiceServer: VoiceServerUpdate): F[Unit] = {
    Logger[F].info(s"Voice server update for guild ${voiceServer.guild_id}: endpoint=${voiceServer.endpoint}, token=${voiceServer.token.take(10)}...") >>
    // Update pending connection with token and endpoint
    pendingVoiceConnections.update { pending =>
      val (sessionId, _, _) = pending.getOrElse(voiceServer.guild_id, (None, None, None))
      pending + (voiceServer.guild_id -> (sessionId, Some(voiceServer.token), voiceServer.endpoint))
    } >>
    // Try to complete the connection
    attemptVoiceConnection(voiceServer.guild_id)
  }

  private def attemptVoiceConnection(guildId: String): F[Unit] = {
    pendingVoiceConnections.get.flatMap { pending =>
      pending.get(guildId) match {
        case Some((Some(sessionId), Some(token), Some(endpoint))) =>
          Logger[F].info(s"All voice connection data received for guild $guildId, connecting...") >>
          botUserIdRef.get.flatMap { botUserId =>
            voiceGateway.connectToVoiceGateway(endpoint, guildId, botUserId, sessionId, token)
              .flatMap { voiceWs =>
                activeVoiceConnections.update(_ + (guildId -> voiceWs)) >>
                // Clear the pending connection
                pendingVoiceConnections.update(_ - guildId) >>
                Logger[F].info(s"Successfully connected to voice channel in guild $guildId")
              }
              .handleErrorWith { error =>
                Logger[F].error(s"Failed to connect to voice gateway: $error")
              }
          }
        case Some((sessionId, token, endpoint)) =>
          Logger[F].info(s"Waiting for more voice connection data for guild $guildId (sessionId: ${sessionId.isDefined}, token: ${token.isDefined}, endpoint: ${endpoint.isDefined})")
        case None =>
          Logger[F].debug(s"No pending voice connection for guild $guildId")
      }
    }
  }
  
  def joinVoiceChannel(guildId: String, channelId: String, ws: WebSocket[F]): F[Unit] = {
    val voiceStateUpdate = Map(
      "op" -> 4.asJson,
      "d" -> Map(
        "guild_id" -> guildId.asJson,
        "channel_id" -> channelId.asJson,
        "self_mute" -> false.asJson,
        "self_deaf" -> false.asJson
      ).asJson
    ).asJson
    
    Logger[F].info(s"Sending voice state update: ${voiceStateUpdate.noSpaces}") >>
    ws.sendText(voiceStateUpdate.noSpaces) >>
    Logger[F].info(s"Sent voice state update to join channel $channelId in guild $guildId") >>
    Logger[F].info("Waiting for VOICE_STATE_UPDATE and VOICE_SERVER_UPDATE events...")
  }
  
  def leaveVoiceChannel(guildId: String, ws: WebSocket[F]): F[Unit] = {
    val voiceStateUpdate = Map(
      "op" -> 4.asJson,
      "d" -> Map(
        "guild_id" -> guildId.asJson,
        "channel_id" -> Option.empty[String].asJson,
        "self_mute" -> false.asJson,
        "self_deaf" -> false.asJson
      ).asJson
    ).asJson
    
    ws.sendText(voiceStateUpdate.noSpaces) >>
    Logger[F].info(s"Sent voice state update to leave voice channel in guild $guildId") >>
    voiceStateRef.set(None)
  }
  
  def getCurrentVoiceState: F[Option[BotVoiceState]] = voiceStateRef.get
  
  def setBotUserId(userId: String): F[Unit] = botUserIdRef.set(userId)
  
  def addToQueue(guildId: String, track: MusicTrack): F[Unit] = {
    musicQueueRef.update { queues =>
      val currentQueue = queues.getOrElse(guildId, MusicQueue(List.empty, None, false))
      val updatedQueue = currentQueue.copy(tracks = currentQueue.tracks :+ track)
      queues + (guildId -> updatedQueue)
    } >> Logger[F].info(s"Added track '${track.title}' to queue for guild $guildId")
  }
  
  def playNext(guildId: String): F[Option[MusicTrack]] = {
    musicQueueRef.modify { queues =>
      val currentQueue = queues.getOrElse(guildId, MusicQueue(List.empty, None, false))
      currentQueue.tracks match {
        case head :: tail =>
          val updatedQueue = currentQueue.copy(
            tracks = tail,
            currentTrack = Some(head),
            isPlaying = true
          )
          val updatedQueues = queues + (guildId -> updatedQueue)
          (updatedQueues, Some(head))
        case Nil =>
          val updatedQueue = currentQueue.copy(currentTrack = None, isPlaying = false)
          val updatedQueues = queues + (guildId -> updatedQueue)
          (updatedQueues, None)
      }
    }
  }
  
  def stopMusic(guildId: String): F[Unit] = {
    musicQueueRef.update { queues =>
      val currentQueue = queues.getOrElse(guildId, MusicQueue(List.empty, None, false))
      val updatedQueue = currentQueue.copy(isPlaying = false, currentTrack = None)
      queues + (guildId -> updatedQueue)
    } >> Logger[F].info(s"Stopped music for guild $guildId")
  }
  
  def clearQueue(guildId: String): F[Unit] = {
    musicQueueRef.update { queues =>
      queues + (guildId -> MusicQueue(List.empty, None, false))
    } >> Logger[F].info(s"Cleared queue for guild $guildId")
  }
  
  def getQueue(guildId: String): F[MusicQueue] = {
    musicQueueRef.get.map(_.getOrElse(guildId, MusicQueue(List.empty, None, false)))
  }
  
  def extractAudioFromYoutube(url: String): F[Option[MusicTrack]] = {
    youtubeExtractor.extractTrackInfo(url)
  }
  
  def getStreamUrl(url: String): F[Option[String]] = {
    youtubeExtractor.getAudioStreamUrl(url)
  }
  
  def addPlaylist(guildId: String, playlistUrl: String, requestedBy: String): F[Int] = {
    youtubeExtractor.extractPlaylistUrls(playlistUrl).flatMap { urls =>
      urls.traverse(url => 
        youtubeExtractor.extractTrackInfo(url).flatMap {
          case Some(track) =>
            val trackWithUser = track.copy(requestedBy = requestedBy)
            addToQueue(guildId, trackWithUser).as(1)
          case None =>
            Async[F].pure(0)
        }
      ).map(_.sum)
    }
  }
  
  def connectToVoiceChannel(
    guildId: String,
    endpoint: String,
    voiceToken: String,
    sessionId: String
  ): F[Unit] = {
    botUserIdRef.get.flatMap { botUserId =>
      voiceGateway.connectToVoiceGateway(endpoint, guildId, botUserId, sessionId, voiceToken)
        .flatMap { voiceWs =>
          activeVoiceConnections.update(_ + (guildId -> voiceWs)) >>
          Logger[F].info(s"Connected to voice channel in guild $guildId")
        }
    }
  }
  
  def startPlayingCurrent(guildId: String): F[Unit] = {
    for {
      queue <- getQueue(guildId)
      _ <- queue.currentTrack match {
        case Some(track) =>
          getStreamUrl(track.url).flatMap {
            case Some(streamUrl) =>
              activeVoiceConnections.get.flatMap(_.get(guildId) match {
                case Some(voiceWs) =>
                  voiceGateway.streamAudio(streamUrl, voiceWs)
                case None =>
                  Logger[F].error(s"No active voice connection for guild $guildId")
              })
            case None =>
              Logger[F].error(s"Failed to get stream URL for track: ${track.url}")
          }
        case None =>
          Logger[F].info("No current track to play")
      }
    } yield ()
  }
  
  def playNextTrack(guildId: String): F[Unit] = {
    playNext(guildId).flatMap {
      case Some(track) =>
        Logger[F].info(s"Now playing: ${track.title}") >>
        startPlayingCurrent(guildId)
      case None =>
        Logger[F].info("Queue is empty")
    }
  }
  
  def getUserVoiceChannel(userId: String): F[Option[String]] = {
    userVoiceStatesRef.get.flatMap { states =>
      Logger[F].info(s"Current voice states: $states") >>
      Logger[F].info(s"Looking for user $userId in voice states") >>
      Async[F].pure(states.get(userId).flatten)
    }
  }
}