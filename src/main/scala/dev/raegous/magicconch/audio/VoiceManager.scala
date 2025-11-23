package dev.raegous.magicconch.audio

import cats.effect.*
import cats.effect.implicits.*
import cats.syntax.all.*
import dev.raegous.magicconch.audio.internals.*
import dev.raegous.magicconch.discord.*
import dev.raegous.magicconch.errors.*
import dev.raegous.magicconch.guilds.GuildSettingsManager
import dev.raegous.magicconch.music.*
import io.circe.syntax.*
import org.typelevel.log4cats.Logger
import sttp.ws.WebSocket

import scala.concurrent.duration.*

object VoiceManager {
  def make[F[_]: Async: fs2.io.process.Processes](
    initialBotUserId: String,
    backend: sttp.client4.WebSocketStreamBackend[F, ?],
    guildSettings: GuildSettingsManager[F]
  )(using Logger[F]): Resource[F, VoiceManager[F]] = {
    for {
      // Create AudioStreamer with Resource pattern for proper lifecycle management
      audioStreamer <- AudioStreamer.make[F](guildSettings)
      voiceGateway <- VoiceGateway.make[F](
        backend,
        audioStreamer
      )
      voiceManager <- Resource.eval {
        for {
          voiceStateRef <- Ref.of[F, Option[BotVoiceState]](None)
          botUserIdRef <- Ref.of[F, String](initialBotUserId)
          musicQueueRef <- Ref.of[F, Map[String, MusicQueue]](Map.empty)
          userVoiceStatesRef <- Ref.of[F, Map[String, Option[String]]](Map.empty)
          searchResultsRef <- Ref.of[F, Map[String, List[YouTubeSearchResult]]](Map.empty)
          activeVoiceConnections <- Ref.of[F, Map[String, sttp.ws.WebSocket[F]]](Map.empty)
          gatewayWebSocketRef <- Ref.of[F, Option[sttp.ws.WebSocket[F]]](None)
          pendingVoiceConnections <- Ref.of[F, Map[String, (Option[String], Option[String], Option[String])]](Map.empty)
          pendingConnectionPromises <- Ref.of[F, Map[String, Deferred[F, Either[VoiceError, Unit]]]](Map.empty)
          activePlaybackFibers <- Ref.of[F, Map[String, Fiber[F, Throwable, Unit]]](Map.empty)
        } yield new VoiceManager[F](
          voiceStateRef,
          botUserIdRef,
          musicQueueRef,
          userVoiceStatesRef,
          searchResultsRef,
          activeVoiceConnections,
          gatewayWebSocketRef,
          pendingVoiceConnections,
          pendingConnectionPromises,
          activePlaybackFibers,
          backend,
          audioStreamer,
          voiceGateway
        )
      }
    } yield voiceManager
  }
}

class VoiceManager[F[_]: Async: fs2.io.process.Processes] private (
  private val voiceStateRef: Ref[F, Option[BotVoiceState]],
  private val botUserIdRef: Ref[F, String],
  private val musicQueueRef: Ref[F, Map[String, MusicQueue]],
  private val userVoiceStatesRef: Ref[F, Map[String, Option[String]]],
  private val searchResultsRef: Ref[F, Map[String, List[YouTubeSearchResult]]],
  private val activeVoiceConnections: Ref[F, Map[String, sttp.ws.WebSocket[F]]],
  private val gatewayWebSocketRef: Ref[F, Option[sttp.ws.WebSocket[F]]],
  private val pendingVoiceConnections: Ref[F, Map[String, (Option[String], Option[String], Option[String])]],
  private val pendingConnectionPromises: Ref[F, Map[String, Deferred[F, Either[VoiceError, Unit]]]],
  private val activePlaybackFibers: Ref[F, Map[String, Fiber[F, Throwable, Unit]]],
  backend: sttp.client4.WebSocketStreamBackend[F, ?],
  audioStreamer: AudioStreamer[F],
  voiceGateway: VoiceGateway[F]
)(using logger: Logger[F]) {

  // Maximum queue size per guild to prevent memory exhaustion
  private val MAX_QUEUE_SIZE = 500

  // Create audio source manager with multiple sources
  // Sources are tried in order, so direct URLs are checked first (fastest)
  private val audioSourceManager = new AudioSourceManager[F](List(
    new DirectUrlAudioSource[F](),
    new YouTubeAudioSource[F]()
  ))

  // Keep YtDlpExtractor for playlist extraction (supports all yt-dlp sources)
  private val ytDlpExtractor = new YtDlpExtractor[F]()

  def handleVoiceStateUpdate(voiceState: VoiceStateUpdate): F[Unit] = {
    voiceState.guild_id.fold(
      Logger[F].debug(s"Voice state update without guild_id (from GUILD_CREATE): user=${voiceState.user_id}")
    ) { guildId =>
      Logger[F].debug(s"[VOICE] State update: user=${voiceState.user_id}, channel=${voiceState.channel_id}") >>
      userVoiceStatesRef.update(states => states + (voiceState.user_id -> voiceState.channel_id)) >>
      botUserIdRef.get.flatMap { botUserId =>
        Option.when(voiceState.user_id == botUserId)(
          voiceState.channel_id.fold(
            // Bot left voice channel - clean up pending connections
            ifEmpty = Logger[F].debug(s"[VOICE] Bot left voice channel, cleaning up pending connections") >>
              pendingVoiceConnections.update(_ - guildId) >>
              pendingConnectionPromises.update(_ - guildId)
          )(
            // Bot joined/moved to voice channel - attempt connection
            _ => Logger[F].debug(s"[VOICE] Bot state updated: session=${voiceState.session_id}, channel=${voiceState.channel_id}") >>
              pendingVoiceConnections.update { pending =>
                val (_, token, endpoint) = pending.getOrElse(guildId, (None, None, None))
                pending + (guildId -> (Some(voiceState.session_id), token, endpoint))
              } >>
              attemptVoiceConnection(guildId)
          )
        ).getOrElse(
          Logger[F].debug(s"[VOICE] User ${voiceState.user_id} -> channel=${voiceState.channel_id}")
        )
      }
    }
  }
  
  def handleVoiceServerUpdate(voiceServer: VoiceServerUpdate): F[Unit] = {
    Logger[F].debug(s"[VOICE] Server update: guild=${voiceServer.guild_id}, endpoint=${voiceServer.endpoint}") >>
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
      pending.get(guildId).fold(
        Logger[F].debug(s"[VOICE] No pending voice connection for guild $guildId")
      ) {
        case (Some(sessionId), Some(token), Some(endpoint)) =>
          Logger[F].info(s"[VOICE] All voice connection data received for guild $guildId, connecting...") >>
          botUserIdRef.get.flatMap { botUserId =>
            voiceGateway.connectToVoiceGateway(endpoint, guildId, botUserId, sessionId, token)
              .flatMap { voiceWs =>
                Logger[F].info(s"[VOICE] Voice WebSocket obtained, storing in activeVoiceConnections for guild $guildId") >>
                activeVoiceConnections.update(_ + (guildId -> voiceWs)) >>
                activeVoiceConnections.get.flatMap { conns =>
                  Logger[F].info(s"[VOICE] activeVoiceConnections now contains: ${conns.keys.mkString(", ")}")
                } >>
                pendingConnectionPromises.get.flatMap { promises =>
                  promises.get(guildId).fold(
                    Logger[F].debug(s"[VOICE] No waiters for guild $guildId connection")
                  ) { deferred =>
                    Logger[F].info(s"[VOICE] Completing connection promise for guild $guildId (signaling waiters)") >>
                    deferred.complete(Right(())).void
                  }
                } >>
                pendingVoiceConnections.update(_ - guildId) >>
                Logger[F].info(s"[VOICE] ✓ Successfully stored voice connection for guild $guildId")
              }
              .handleErrorWith { error =>
                Logger[F].error(s"[VOICE] ✗ Failed to connect to voice gateway: ${error.getMessage}") >>
                Logger[F].error(s"[VOICE] Stack trace: ${error.getStackTrace.take(5).mkString("\n")}") >>
                pendingVoiceConnections.update(_ - guildId) >>
                pendingConnectionPromises.get.flatMap { promises =>
                  promises.get(guildId).traverse_ { deferred =>
                    val voiceError = VoiceConnectionFailed(guildId, error.getMessage)
                    deferred.complete(Left(voiceError)).void
                  }
                }
              }
          }
        case (sessionId, token, endpoint) =>
          Logger[F].info(s"[VOICE] Waiting for more voice connection data for guild $guildId (sessionId: ${sessionId.isDefined}, token: ${token.isDefined}, endpoint: ${endpoint.isDefined})")
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

    Logger[F].info(s"[VOICE] Joining channel...") >>
    Logger[F].debug(s"[VOICE] Channel ID: $channelId") >>
    gatewayWebSocketRef.set(Some(ws)) >> // Store gateway WebSocket for later use (leaving channel)
    ws.sendText(voiceStateUpdate.noSpaces)
  }

  def waitForVoiceConnection(guildId: String): F[Unit] = {
    for {
      // Create a deferred that will be completed when the connection is ready
      deferred <- Deferred[F, Either[VoiceError, Unit]]
      _ <- Logger[F].debug(s"[VOICE] Waiting for voice connection (async)...")

      // Store the deferred so attemptVoiceConnection can complete it
      _ <- pendingConnectionPromises.update(_ + (guildId -> deferred))

      // Wait for the deferred to be completed (or timeout after 15 seconds)
      result <- deferred.get.timeoutTo(
        15.seconds,
        for {
          _ <- Logger[F].error(s"[VOICE] ✗ Timeout waiting for voice connection after 15 seconds")
          connections <- activeVoiceConnections.get
          _ <- Logger[F].error(s"[VOICE] Current connections in map: ${connections.keys.mkString(", ")}")
          _ <- Logger[F].error(s"[VOICE] Looking for guild: $guildId")
          _ <- Logger[F].error(s"[VOICE] This usually means the voice gateway connection failed or is taking too long")
          _ <- pendingConnectionPromises.update(_ - guildId)
        } yield Left(VoiceConnectionTimeout(guildId))
      )

      // Clean up the promise after completion
      _ <- pendingConnectionPromises.update(_ - guildId)

      // Handle the result - raise error if connection failed
      _ <- result.fold(
        error => {
          Logger[F].error(s"[VOICE] Connection failed: ${error.message}") >>
          Async[F].raiseError(error)
        },
        _ => Logger[F].debug(s"[VOICE] Connection ready")
      )
    } yield ()
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
    Logger[F].info(s"[VOICE] Left voice channel") >>
    voiceStateRef.set(None) >>
    pendingVoiceConnections.update(_ - guildId) >>
    pendingConnectionPromises.update(_ - guildId)
  }
  
  def getCurrentVoiceState: F[Option[BotVoiceState]] = voiceStateRef.get
  
  def setBotUserId(userId: String): F[Unit] = botUserIdRef.set(userId)
  
  def addToQueue(guildId: String, track: MusicTrack): F[Either[QueueError, Unit]] = {
    musicQueueRef.modify { queues =>
      val currentQueue = queues.getOrElse(guildId, MusicQueue(List.empty, None, false))
      currentQueue.tracks.length >= MAX_QUEUE_SIZE match {
        case true =>
          (queues, Left(QueueFull(MAX_QUEUE_SIZE)))
        case false =>
          val updatedQueue = currentQueue.copy(tracks = currentQueue.tracks :+ track)
          (queues + (guildId -> updatedQueue), Right(()))
      }
    }.flatTap {
      case Right(_) => Logger[F].info(s"[QUEUE] Added: ${track.title}")
      case Left(QueueFull(max)) => Logger[F].warn(s"[QUEUE] Failed to add track: queue is full ($max tracks)")
    }
  }
  
  def playNext(guildId: String): F[Option[MusicTrack]] = {
    musicQueueRef.modify { queues =>
      val currentQueue = queues.getOrElse(guildId, MusicQueue(List.empty, None, false))
      currentQueue.tracks.headOption.fold[(Map[String, MusicQueue], Option[MusicTrack])] {
        val updatedQueue = currentQueue.copy(currentTrack = None, isPlaying = false)
        val updatedQueues = queues + (guildId -> updatedQueue)
        (updatedQueues, None)
      } { head =>
        val updatedQueue = currentQueue.copy(
          tracks = currentQueue.tracks.tail,
          currentTrack = Some(head),
          isPlaying = true
        )
        val updatedQueues = queues + (guildId -> updatedQueue)
        (updatedQueues, Some(head))
      }
    }
  }
  
  def stopMusic(guildId: String): F[Unit] = {
    for {
      _ <- Logger[F].info(s"[STOP] Stopping music for guild $guildId")

      // Cancel any active playback fiber
      _ <- activePlaybackFibers.get.flatMap { fibers =>
        fibers.get(guildId).fold(
          Logger[F].debug(s"[STOP] No active playback fiber to cancel")
        ) { fiber =>
          Logger[F].info(s"[STOP] Canceling active playback fiber") >>
          fiber.cancel >>
          activePlaybackFibers.update(_ - guildId)
        }
      }

      // Update queue state
      _ <- musicQueueRef.update { queues =>
        val currentQueue = queues.getOrElse(guildId, MusicQueue(List.empty, None, false))
        val updatedQueue = currentQueue.copy(isPlaying = false, currentTrack = None)
        queues + (guildId -> updatedQueue)
      }

      // Disconnect from voice channel
      _ <- gatewayWebSocketRef.get.flatMap(
        _.fold(
          Logger[F].warn(s"[STOP] No gateway WebSocket available to send leave command")
        ) { gatewayWs =>
          Logger[F].info(s"[STOP] Disconnecting from voice channel") >>
          leaveVoiceChannel(guildId, gatewayWs) >>
          activeVoiceConnections.update(_ - guildId)
        }
      )

      _ <- Logger[F].info(s"[STOP] ✓ Stopped music and disconnected from voice channel")
    } yield ()
  }
  
  def clearQueue(guildId: String): F[Unit] = {
    musicQueueRef.update { queues =>
      queues + (guildId -> MusicQueue(List.empty, None, false))
    } >> Logger[F].info(s"Cleared queue for guild $guildId")
  }
  
  def getQueue(guildId: String): F[MusicQueue] = {
    musicQueueRef.get.map(_.getOrElse(guildId, MusicQueue(List.empty, None, false)))
  }

  private def getStreamUrl(url: String): F[Option[String]] = {
    audioSourceManager.getStreamUrl(url)
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
    Logger[F].debug(s"[PLAYBACK] Starting playback") >>
    getQueue(guildId).flatMap { queue =>
      queue.currentTrack.fold(
        Logger[F].warn(s"[PLAYBACK] No current track to play - queue might be empty")
      ) { track =>
        Logger[F].info(s"[PLAYBACK] Playing: ${track.title}") >>
        Logger[F].debug(s"[PLAYBACK] URL: ${track.url}") >>
        getStreamUrl(track.url).flatMap {
          case None =>
            Logger[F].error(s"[PLAYBACK] Failed to extract stream URL (yt-dlp error or unavailable video)") >>
            clearCurrentTrack(guildId)
          case Some(streamUrl) =>
            Logger[F].debug(s"[PLAYBACK] Got stream URL") >>
            activeVoiceConnections.get.flatMap { connections =>
              connections.get(guildId).fold(
                Logger[F].error(s"[PLAYBACK] ✗ No active voice connection for guild $guildId - bot may not be connected to voice") >>
                clearCurrentTrack(guildId)
              ) { voiceWs =>
                Logger[F].debug(s"[PLAYBACK] Starting stream...") >>
                startPlaybackFiber(guildId, streamUrl, voiceWs)
              }
            }
        }
      }
    }
  }
  
  // Helper to clear current track on error
  private def clearCurrentTrack(guildId: String): F[Unit] = {
    musicQueueRef.update { queues =>
      val currentQueue = queues.getOrElse(guildId, MusicQueue(List.empty, None, false))
      val updatedQueue = currentQueue.copy(currentTrack = None, isPlaying = false)
      queues + (guildId -> updatedQueue)
    }
  }

  // Helper to start playback fiber and track it
  private def startPlaybackFiber(guildId: String, streamUrl: String, voiceWs: sttp.ws.WebSocket[F], startPosition: Int = 0): F[Unit] = {
    val playbackAction = {
      val streamF = if (startPosition > 0) {
        voiceGateway.streamAudioFromPosition(streamUrl, voiceWs, startPosition, guildId)
      } else {
        voiceGateway.streamAudio(streamUrl, voiceWs, guildId)
      }

      streamF.handleErrorWith { error =>
        Logger[F].error(s"[PLAYBACK] ✗ Failed to stream audio: ${error.getMessage}") >>
        Logger[F].error(s"[PLAYBACK] Stack trace: ${error.getStackTrace.mkString("\n")}") >>
        clearCurrentTrack(guildId)
      } >>
      Logger[F].info(s"[PLAYBACK] Finished playing") >>
      activePlaybackFibers.update(_ - guildId) >>
      // Check if there's more in queue
      getQueue(guildId).flatMap { queue =>
        queue.tracks.headOption.fold(
          // Queue is empty - leave voice channel and clear queue
          Logger[F].info(s"[PLAYBACK] Queue empty, leaving voice channel") >>
          gatewayWebSocketRef.get.flatMap(
            _.fold(
              Logger[F].warn(s"[PLAYBACK] No gateway WebSocket available to send leave command")
            ) { gatewayWs =>
              leaveVoiceChannel(guildId, gatewayWs) >>
              activeVoiceConnections.update(_ - guildId)
            }
          ) >>
          clearQueue(guildId)
        ) { _ =>
          // More tracks in queue - play next
          Logger[F].info(s"[PLAYBACK] Queue has more tracks, playing next") >>
          playNextTrack(guildId)
        }
      }
    }

    playbackAction.start.flatMap { fiber =>
      activePlaybackFibers.update(_ + (guildId -> fiber))
    }
  }

  def playNextTrack(guildId: String): F[Unit] = {
    Logger[F].info(s"[PLAYBACK] playNextTrack called for guild $guildId") >>
    playNext(guildId).flatMap(
      _.fold(
        Logger[F].warn(s"[PLAYBACK] Queue is empty for guild $guildId")
      ) { track =>
        Logger[F].info(s"[PLAYBACK] ✓ Got next track from queue: ${track.title}") >>
        Logger[F].info(s"[PLAYBACK] Starting playback for: ${track.url}") >>
        startPlayingCurrent(guildId)
      }
    )
  }
  
  def getUserVoiceChannel(userId: String): F[Option[String]] = {
    userVoiceStatesRef.get.flatMap { states =>
      Logger[F].info(s"Current voice states: $states") >>
      Logger[F].info(s"Looking for user $userId in voice states") >>
      Async[F].pure(states.get(userId).flatten)
    }
  }

  def populateVoiceStates(voiceStates: List[VoiceStateUpdate]): F[Unit] = {
    userVoiceStatesRef.update { states =>
      voiceStates.foldLeft(states) { (acc, vs) =>
        acc + (vs.user_id -> vs.channel_id)
      }
    }
  }

  // YouTube search result management
  def storeSearchResults(userId: String, results: List[YouTubeSearchResult]): F[Unit] = {
    searchResultsRef.update(_ + (userId -> results)) >>
    Logger[F].debug(s"[SEARCH] Stored ${results.length} results for user $userId")
  }

  def getSearchResults(userId: String): F[Option[List[YouTubeSearchResult]]] = {
    searchResultsRef.get.map(_.get(userId))
  }

  def clearSearchResults(userId: String): F[Unit] = {
    searchResultsRef.update(_ - userId) >>
    Logger[F].debug(s"[SEARCH] Cleared search results for user $userId")
  }

  // Seek functionality
  def seek(guildId: String, positionSeconds: Int): F[Unit] = {
    getQueue(guildId).flatMap { queue =>
      queue.currentTrack.fold(
        Logger[F].warn(s"[SEEK] No track currently playing to seek")
      ) { track =>
        Logger[F].info(s"[SEEK] Seeking to $positionSeconds seconds in ${track.title}") >>
        stopMusic(guildId) >>
        musicQueueRef.update { queues =>
          val currentQueue = queues.getOrElse(guildId, MusicQueue(List.empty, None, false))
          val updatedQueue = currentQueue.copy(
            currentTrack = Some(track),
            currentPosition = positionSeconds,
            isPlaying = true,
            isPaused = false,
            startTime = Some(System.currentTimeMillis()),
            pauseTime = None
          )
          queues + (guildId -> updatedQueue)
        } >>
        startPlayingCurrentWithPosition(guildId, positionSeconds)
      }
    }
  }

  def isPaused(guildId: String): F[Boolean] = {
    getQueue(guildId).map(_.isPaused)
  }

  def pausePlayback(guildId: String): F[Unit] = {
    for {
      queue <- getQueue(guildId)
      _ <- queue.currentTrack match {
        case None =>
          Logger[F].warn(s"[PAUSE] No track currently playing to pause")
        case Some(track) if queue.isPaused =>
          Logger[F].warn(s"[PAUSE] Track already paused")
        case Some(track) =>
          for {
            currentPos <- getCurrentPosition(guildId)
            _ <- Logger[F].info(s"[PAUSE] Pausing ${track.title} at ${currentPos}s")
            _ <- stopMusic(guildId)
            _ <- musicQueueRef.update { queues =>
              val currentQueue = queues.getOrElse(guildId, MusicQueue(List.empty, None, false))
              val updatedQueue = currentQueue.copy(
                currentTrack = Some(track),
                currentPosition = currentPos,
                isPlaying = false,
                isPaused = true,
                pauseTime = Some(System.currentTimeMillis()),
                startTime = None
              )
              queues + (guildId -> updatedQueue)
            }
          } yield ()
      }
    } yield ()
  }

  def resumePlayback(guildId: String): F[Unit] = {
    for {
      queue <- getQueue(guildId)
      _ <- queue.currentTrack match {
        case None =>
          Logger[F].warn(s"[RESUME] No track to resume")
        case Some(track) if !queue.isPaused =>
          Logger[F].warn(s"[RESUME] Track is not paused")
        case Some(track) =>
          Logger[F].info(s"[RESUME] Resuming ${track.title} from ${queue.currentPosition}s") >>
          musicQueueRef.update { queues =>
            val currentQueue = queues.getOrElse(guildId, MusicQueue(List.empty, None, false))
            val updatedQueue = currentQueue.copy(
              isPlaying = true,
              isPaused = false,
              startTime = Some(System.currentTimeMillis()),
              pauseTime = None
            )
            queues + (guildId -> updatedQueue)
          } >>
          startPlayingCurrentWithPosition(guildId, queue.currentPosition)
      }
    } yield ()
  }

  def skipTrack(guildId: String): F[Unit] = {
    Logger[F].info(s"[SKIP] Skipping current track") >>
    stopMusic(guildId) >>
    playNextTrack(guildId)
  }

  def stopPlayback(guildId: String): F[Unit] = {
    Logger[F].info(s"[STOP] Stopping playback and clearing queue") >>
    stopMusic(guildId) >>
    clearQueue(guildId)
  }

  private def startPlayingCurrentWithPosition(guildId: String, startPosition: Int): F[Unit] = {
    Logger[F].debug(s"[PLAYBACK] Starting playback from position $startPosition seconds") >>
    getQueue(guildId).flatMap { queue =>
      queue.currentTrack.fold(
        Logger[F].warn(s"[PLAYBACK] No current track to play")
      ) { track =>
        Logger[F].info(s"[PLAYBACK] Playing: ${track.title} from ${startPosition}s") >>
        getStreamUrl(track.url).flatMap {
          case None =>
            Logger[F].error(s"[PLAYBACK] Failed to extract stream URL") >>
            clearCurrentTrack(guildId)
          case Some(streamUrl) =>
            activeVoiceConnections.get.flatMap { connections =>
              connections.get(guildId).fold(
                Logger[F].error(s"[PLAYBACK] ✗ No active voice connection for guild $guildId") >>
                clearCurrentTrack(guildId)
              ) { voiceWs =>
                startPlaybackFiber(guildId, streamUrl, voiceWs, startPosition)
              }
            }
        }
      }
    }
  }

  def getCurrentPosition(guildId: String): F[Int] = {
    getQueue(guildId).map { queue =>
      queue.startTime.filter(_ => queue.isPlaying).fold(queue.currentPosition) { startTime =>
        val elapsed = (System.currentTimeMillis() - startTime) / 1000
        queue.currentPosition + elapsed.toInt
      }
    }
  }
}