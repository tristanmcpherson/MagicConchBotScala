package dev.raegous.magicconch

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.*

case class DiscordMessage(
    id: String,
    channel_id: String,
    author: User,
    content: String,
    timestamp: String,
    attachments: Option[List[Attachment]] = None,
    flags: Option[Int] = None,
    guild_id: Option[String] = None
)

case class User(
    id: String,
    username: String,
    discriminator: String,
    bot: Option[Boolean] = None
)

case class GatewayPayload(
    op: Int,
    d: Option[io.circe.Json],
    s: Option[Int],
    t: Option[String]
)

case class HelloPayload(
    heartbeat_interval: Int
)

case class ReadyPayload(
    user: User,
    session_id: String
)

case class Attachment(
    id: String,
    filename: String,
    content_type: Option[String],
    size: Int,
    url: String,
    proxy_url: Option[String],
    duration_secs: Option[Double], // For voice messages
    waveform: Option[String] // For voice messages
)

case class IdentifyPayload(
    token: String,
    intents: Int,
    properties: Map[String, String]
)

case class VoiceStateUpdate(
    guild_id: String,
    channel_id: Option[String],
    user_id: String,
    session_id: String,
    deaf: Boolean,
    mute: Boolean
)

case class VoiceServerUpdate(
    token: String,
    guild_id: String,
    endpoint: Option[String]
)

case class VoiceIdentify(
    server_id: String,
    user_id: String,
    session_id: String,
    token: String
)

case class BotVoiceState(
    sessionId: String,
    voiceToken: String,
    guildId: String,
    channelId: Option[String]
)

case class MusicTrack(
    url: String,
    title: String,
    duration: Option[Int],
    requestedBy: String
)

case class MusicQueue(
    tracks: List[MusicTrack],
    currentTrack: Option[MusicTrack],
    isPlaying: Boolean
)

case class SlashCommand(
    id: String,
    name: String,
    description: String,
    options: Option[List[SlashCommandOption]] = None
)

case class SlashCommandOption(
    name: String,
    description: String,
    `type`: Int,
    required: Option[Boolean] = None
)

case class InteractionResponse(
    `type`: Int,
    data: Option[InteractionResponseData]
)

case class InteractionResponseData(
    content: Option[String] = None,
    flags: Option[Int] = None
)

case class Interaction(
    id: String,
    `type`: Int,
    token: String,
    data: Option[InteractionData],
    guild_id: Option[String],
    channel_id: String,
    member: Option[GuildMember],
    user: Option[User]
)

case class InteractionData(
    id: String,
    name: String,
    `type`: Int,
    options: Option[List[InteractionOption]] = None
)

case class InteractionOption(
    name: String,
    `type`: Int,
    value: Option[String] = None
)

case class GuildMember(
    user: Option[User],
    nick: Option[String],
    roles: List[String],
    joined_at: String
)

object DiscordIntents {
  val GUILDS = 1 << 0                    // 1
  val GUILD_MEMBERS = 1 << 1             // 2  
  val GUILD_MODERATION = 1 << 2          // 4
  val GUILD_EMOJIS_AND_STICKERS = 1 << 3 // 8
  val GUILD_INTEGRATIONS = 1 << 4        // 16
  val GUILD_WEBHOOKS = 1 << 5            // 32
  val GUILD_INVITES = 1 << 6             // 64
  val GUILD_VOICE_STATES = 1 << 7        // 128
  val GUILD_PRESENCES = 1 << 8           // 256
  val GUILD_MESSAGES = 1 << 9            // 512
  val GUILD_MESSAGE_REACTIONS = 1 << 10  // 1024
  val GUILD_MESSAGE_TYPING = 1 << 11     // 2048
  val DIRECT_MESSAGES = 1 << 12          // 4096
  val DIRECT_MESSAGE_REACTIONS = 1 << 13 // 8192
  val DIRECT_MESSAGE_TYPING = 1 << 14    // 16384
  val MESSAGE_CONTENT = 1 << 15          // 32768
  val GUILD_SCHEDULED_EVENTS = 1 << 16   // 65536
  val AUTO_MODERATION_CONFIGURATION = 1 << 20 // 1048576
  val AUTO_MODERATION_EXECUTION = 1 << 21     // 2097152
  
  val BOT_DEFAULT = GUILDS | GUILD_MESSAGES | MESSAGE_CONTENT | GUILD_VOICE_STATES
}

object DiscordModels {
  given Decoder[DiscordMessage] = deriveDecoder
  given Encoder[DiscordMessage] = deriveEncoder
  
  given Decoder[User] = deriveDecoder
  given Encoder[User] = deriveEncoder
  
  given Decoder[GatewayPayload] = deriveDecoder
  given Encoder[GatewayPayload] = deriveEncoder
  
  given Decoder[HelloPayload] = deriveDecoder
  given Encoder[HelloPayload] = deriveEncoder
  
  given Decoder[ReadyPayload] = deriveDecoder
  given Encoder[ReadyPayload] = deriveEncoder
  
  given Decoder[IdentifyPayload] = deriveDecoder
  given Encoder[IdentifyPayload] = deriveEncoder
  
  given Decoder[Attachment] = deriveDecoder
  given Encoder[Attachment] = deriveEncoder
  
  given Decoder[VoiceStateUpdate] = deriveDecoder
  given Encoder[VoiceStateUpdate] = deriveEncoder
  
  given Decoder[VoiceServerUpdate] = deriveDecoder
  given Encoder[VoiceServerUpdate] = deriveEncoder
  
  given Decoder[VoiceIdentify] = deriveDecoder
  given Encoder[VoiceIdentify] = deriveEncoder
  
  given Decoder[BotVoiceState] = deriveDecoder
  given Encoder[BotVoiceState] = deriveEncoder
  
  given Decoder[MusicTrack] = deriveDecoder
  given Encoder[MusicTrack] = deriveEncoder
  
  given Decoder[MusicQueue] = deriveDecoder
  given Encoder[MusicQueue] = deriveEncoder
  
  given Decoder[SlashCommand] = deriveDecoder
  given Encoder[SlashCommand] = deriveEncoder
  
  given Decoder[SlashCommandOption] = deriveDecoder
  given Encoder[SlashCommandOption] = deriveEncoder
  
  given Decoder[InteractionResponse] = deriveDecoder
  given Encoder[InteractionResponse] = deriveEncoder
  
  given Decoder[InteractionResponseData] = deriveDecoder
  given Encoder[InteractionResponseData] = deriveEncoder
  
  given Decoder[Interaction] = deriveDecoder
  given Encoder[Interaction] = deriveEncoder
  
  given Decoder[InteractionData] = deriveDecoder
  given Encoder[InteractionData] = deriveEncoder
  
  given Decoder[InteractionOption] = deriveDecoder
  given Encoder[InteractionOption] = deriveEncoder
  
  given Decoder[GuildMember] = deriveDecoder
  given Encoder[GuildMember] = deriveEncoder
}