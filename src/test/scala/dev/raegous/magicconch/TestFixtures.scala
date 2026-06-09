package dev.raegous.magicconch

import cats.effect.*
import dev.raegous.magicconch.discord.*
import dev.raegous.magicconch.music.YouTubeSearchResult
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

/** Common test fixtures and utilities for unit tests
  */
object TestFixtures {

  // No-op logger for tests
  given testLogger[F[_]: Sync]: Logger[F] = NoOpLogger[F]

  // Sample Discord models for testing
  def sampleUser(
      id: String = "123456789",
      username: String = "testuser"
  ): User = {
    User(
      id = id,
      username = username,
      discriminator = "0001",
      bot = Some(false),
      avatar = None,
      global_name = Some(username)
    )
  }

  def sampleGuildMember(
      userId: String = "123456789",
      username: String = "testuser"
  ): GuildMember = {
    GuildMember(
      user = Some(sampleUser(userId, username)),
      nick = None,
      roles = List.empty,
      joined_at = "2024-01-01T00:00:00.000Z"
    )
  }

  def sampleInteraction(
      id: String = "interaction_123",
      customId: Option[String] = None,
      guildId: Option[String] = Some("guild_123"),
      userId: String = "123456789"
  ): Interaction = {
    Interaction(
      id = id,
      `type` = 3, // MESSAGE_COMPONENT
      token = "interaction_token",
      data = customId.map(cid => InteractionData(custom_id = Some(cid))),
      guild_id = guildId,
      channel_id = "channel_123",
      member = Some(sampleGuildMember(userId)),
      user = None
    )
  }

  def sampleMusicTrack(
      url: String = "https://youtube.com/watch?v=test",
      title: String = "Test Song",
      requestedBy: String = "testuser"
  ): MusicTrack = {
    MusicTrack(
      url = url,
      title = title,
      duration = Some(180),
      requestedBy = requestedBy
    )
  }

  def sampleYouTubeSearchResult(
      videoId: String = "test_video",
      title: String = "Test Video",
      channelTitle: String = "Test Channel"
  ): YouTubeSearchResult = {
    YouTubeSearchResult(
      videoId = videoId,
      title = title,
      channelTitle = channelTitle,
      thumbnail = s"https://i.ytimg.com/vi/$videoId/default.jpg",
      url = s"https://youtube.com/watch?v=$videoId"
    )
  }

  def sampleMusicQueue(
      tracks: List[MusicTrack] = List.empty,
      currentTrack: Option[MusicTrack] = None,
      isPlaying: Boolean = false
  ): MusicQueue = {
    MusicQueue(
      tracks = tracks,
      currentTrack = currentTrack,
      isPlaying = isPlaying,
      isPaused = false,
      currentPosition = 0,
      startTime = None,
      pauseTime = None
    )
  }

  def sampleGatewayPayload(
      op: Int,
      d: Option[io.circe.Json] = None,
      s: Option[Int] = None,
      t: Option[String] = None
  ): GatewayPayload = {
    GatewayPayload(op = op, d = d, s = s, t = t)
  }

  def sampleReadyPayload(
      userId: String = "bot_user_123",
      username: String = "TestBot"
  ): ReadyPayload = {
    ReadyPayload(
      user = sampleUser(userId, username),
      session_id = "session_123"
    )
  }

  def sampleHelloPayload(heartbeatInterval: Int = 41250): HelloPayload = {
    HelloPayload(heartbeat_interval = heartbeatInterval)
  }

  def sampleGuildCreate(
      id: String = "guild_123",
      name: String = "Test Guild",
      memberCount: Int = 100
  ): GuildCreate = {
    GuildCreate(
      id = id,
      name = name,
      icon = None,
      splash = None,
      discovery_splash = None,
      owner_id = "owner_123",
      afk_channel_id = None,
      afk_timeout = 300,
      verification_level = 0,
      default_message_notifications = 0,
      explicit_content_filter = 0,
      roles = List.empty,
      emojis = List.empty,
      features = List.empty,
      mfa_level = 0,
      system_channel_id = None,
      system_channel_flags = 0,
      rules_channel_id = None,
      joined_at = "2024-01-01T00:00:00.000Z",
      large = false,
      member_count = memberCount,
      voice_states = Some(List.empty),
      members = Some(List.empty),
      channels = Some(List.empty),
      threads = Some(List.empty),
      presences = Some(List.empty),
      stage_instances = Some(List.empty),
      guild_scheduled_events = Some(List.empty)
    )
  }

  def sampleDiscordMessage(
      id: String = "message_123",
      content: String = "!test command",
      authorId: String = "user_123",
      isBot: Boolean = false
  ): DiscordMessage = {
    DiscordMessage(
      id = id,
      channel_id = "channel_123",
      guild_id = Some("guild_123"),
      author = sampleUser(authorId).copy(bot = Some(isBot)),
      content = content,
      timestamp = "2024-01-01T00:00:00.000Z",
      edited_timestamp = None,
      tts = None,
      mention_everyone = None,
      mentions = None,
      mention_roles = None,
      attachments = None,
      embeds = None,
      pinned = None,
      `type` = Some(0)
    )
  }
}
