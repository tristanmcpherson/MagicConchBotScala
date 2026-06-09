package dev.raegous.magicconch.discord

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
    guild_id: Option[String] = None,
    embeds: Option[List[MessageEmbed]] = None,
    components: Option[List[MessageComponent]] = None,
    // Additional fields that may be present
    tts: Option[Boolean] = None,
    mention_everyone: Option[Boolean] = None,
    mentions: Option[List[io.circe.Json]] = None,
    mention_roles: Option[List[String]] = None,
    pinned: Option[Boolean] = None,
    `type`: Option[Int] = None,
    edited_timestamp: Option[String] = None
)

case class User(
    id: String,
    username: String,
    discriminator: String,
    bot: Option[Boolean] = None,
    avatar: Option[String] = None,
    global_name: Option[String] = None,
    public_flags: Option[Int] = None,
    primary_guild: Option[io.circe.Json] = None,
    display_name_styles: Option[io.circe.Json] = None,
    collectibles: Option[io.circe.Json] = None,
    clan: Option[io.circe.Json] = None,
    avatar_decoration_data: Option[io.circe.Json] = None
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
    guild_id: Option[String] =
      None, // Optional: present in VOICE_STATE_UPDATE events, absent in GUILD_CREATE.voice_states
    channel_id: Option[String],
    user_id: String,
    session_id: String,
    deaf: Boolean,
    mute: Boolean,
    self_deaf: Option[Boolean] = None,
    self_mute: Option[Boolean] = None,
    self_stream: Option[Boolean] = None,
    self_video: Option[Boolean] = None,
    suppress: Option[Boolean] = None,
    request_to_speak_timestamp: Option[String] = None
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
    isPlaying: Boolean,
    isPaused: Boolean = false, // Track if playback is paused
    currentPosition: Int = 0, // Current playback position in seconds
    startTime: Option[Long] = None, // Epoch milliseconds when playback started
    pauseTime: Option[Long] =
      None // Epoch milliseconds when playback was paused
)

case class SlashCommand(
    id: String,
    application_id: String,
    version: String,
    default_member_permissions: Option[String],
    `type`: Int,
    name: String,
    description: String,
    guild_id: String,
    nsfw: Boolean,
    options: Option[List[SlashCommandOption]] = None
)

case class SlashCommandOption(
    `type`: Int,
    name: String,
    description: String,
    required: Option[Boolean],
    options: Option[Seq[SlashCommandOptionOption]]
)

case class SlashCommandOptionOption(
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
    flags: Option[Int] = None,
    embeds: Option[List[MessageEmbed]] = None,
    components: Option[List[MessageComponent]] = None
)

case class MessageEmbed(
    title: Option[String] = None,
    description: Option[String] = None,
    color: Option[Int] = None,
    fields: Option[List[EmbedField]] = None,
    thumbnail: Option[EmbedThumbnail] = None,
    `type`: Option[String] = None,
    id: Option[String] = None,
    content_scan_version: Option[Int] = None
)

case class EmbedField(
    name: String,
    value: String,
    inline: Option[Boolean] = None
)

case class EmbedThumbnail(url: String)

case class MessageComponent(
    `type`: Int, // 1 = Action Row, 2 = Button, 3 = Select Menu
    components: Option[List[MessageComponent]] = None, // For Action Row
    style: Option[Int] =
      None, // For Button: 1=Primary, 2=Secondary, 3=Success, 4=Danger, 5=Link
    label: Option[String] = None, // Button label
    custom_id: Option[String] = None, // Identifier for handling interactions
    url: Option[String] = None, // For Link-style buttons
    id: Option[Int] = None, // Discord-assigned ID for components in messages
    disabled: Option[Boolean] = None
)

// Type alias for Action Row components
type MessageActionRow = MessageComponent

case class Interaction(
    id: String,
    `type`: Int,
    token: String,
    data: Option[InteractionData],
    guild_id: Option[String],
    channel_id: String,
    member: Option[GuildMember],
    user: Option[User],
    // Additional fields for button/component interactions
    message: Option[DiscordMessage] = None,
    locale: Option[String] = None,
    guild_locale: Option[String] = None,
    context: Option[Int] = None,
    app_permissions: Option[String] = None,
    authorizing_integration_owners: Option[io.circe.Json] = None,
    attachment_size_limit: Option[Long] = None,
    entitlements: Option[List[io.circe.Json]] = None,
    entitlement_sku_ids: Option[List[String]] = None,
    channel: Option[io.circe.Json] = None,
    guild: Option[io.circe.Json] = None,
    application_id: Option[String] = None,
    version: Option[Int] = None
)

case class InteractionData(
    id: Option[io.circe.Json] =
      None, // Can be String (for slash commands) or Int (for components)
    name: Option[String] = None, // Only for slash commands
    `type`: Option[Int] = None, // Only for slash commands
    options: Option[List[InteractionOption]] = None,
    custom_id: Option[String] = None, // For MESSAGE_COMPONENT interactions
    component_type: Option[Int] = None // For MESSAGE_COMPONENT interactions
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
    joined_at: String,
    premium_since: Option[String] = None,
    permissions: Option[String] = None,
    pending: Option[Boolean] = None,
    mute: Option[Boolean] = None,
    deaf: Option[Boolean] = None,
    flags: Option[Int] = None,
    avatar: Option[String] = None,
    banner: Option[io.circe.Json] = None,
    communication_disabled_until: Option[String] = None,
    unusual_dm_activity_until: Option[io.circe.Json] = None,
    display_name_styles: Option[io.circe.Json] = None,
    collectibles: Option[io.circe.Json] = None
)

enum EncryptionMode(val value: String) {
  case AeadAes256GcmRtpSize extends EncryptionMode("aead_aes256_gcm_rtpsize")
  case AeadXChaCha20Poly1305RtpSize
      extends EncryptionMode("aead_xchacha20_poly1305_rtpsize")
}

object EncryptionMode {
  def fromString(s: String): Option[EncryptionMode] = s match {
    case "aead_aes256_gcm_rtpsize"         => Some(AeadAes256GcmRtpSize)
    case "aead_xchacha20_poly1305_rtpsize" => Some(AeadXChaCha20Poly1305RtpSize)
    case _                                 => None
  }

  // Preferred modes in priority order
  val preferredModes: List[EncryptionMode] = List(
    AeadAes256GcmRtpSize, // Preferred by Discord when offered; hardware accelerated on modern CPUs
    AeadXChaCha20Poly1305RtpSize // Required fallback mode
  )
}

case class GuildCreate(
    id: String,
    name: String,
    icon: Option[String],
    splash: Option[String],
    discovery_splash: Option[String],
    owner_id: String,
    afk_channel_id: Option[String],
    afk_timeout: Int,
    verification_level: Int,
    default_message_notifications: Int,
    explicit_content_filter: Int,
    roles: List[io.circe.Json],
    emojis: List[io.circe.Json],
    features: List[String],
    mfa_level: Int,
    system_channel_id: Option[String],
    system_channel_flags: Int,
    rules_channel_id: Option[String],
    joined_at: String,
    large: Boolean,
    member_count: Int,
    voice_states: Option[List[VoiceStateUpdate]],
    members: Option[List[io.circe.Json]],
    channels: Option[List[io.circe.Json]],
    threads: Option[List[io.circe.Json]],
    presences: Option[List[io.circe.Json]],
    stage_instances: Option[List[io.circe.Json]],
    guild_scheduled_events: Option[List[io.circe.Json]],
    // Additional fields that may be present
    max_presences: Option[Int] = None,
    max_members: Option[Int] = None,
    vanity_url_code: Option[String] = None,
    description: Option[String] = None,
    banner: Option[String] = None,
    premium_tier: Option[Int] = None,
    premium_subscription_count: Option[Int] = None,
    preferred_locale: Option[String] = None,
    public_updates_channel_id: Option[String] = None,
    nsfw_level: Option[Int] = None,
    stickers: Option[List[io.circe.Json]] = None,
    premium_progress_bar_enabled: Option[Boolean] = None,
    `lazy`: Option[Boolean] = None,
    application_id: Option[String] = None,
    unavailable: Option[Boolean] = None,
    // Additional fields from newer Discord API versions
    region: Option[String] = None,
    hub_type: Option[io.circe.Json] = None,
    profile: Option[io.circe.Json] = None,
    inventory_settings: Option[io.circe.Json] = None,
    safety_alerts_channel_id: Option[String] = None,
    moderator_reporting: Option[io.circe.Json] = None,
    latest_onboarding_question_id: Option[io.circe.Json] = None,
    incidents_data: Option[io.circe.Json] = None,
    max_stage_video_channel_users: Option[Int] = None,
    max_video_channel_users: Option[Int] = None,
    embedded_activities: Option[List[io.circe.Json]] = None,
    activity_instances: Option[List[io.circe.Json]] = None,
    soundboard_sounds: Option[List[io.circe.Json]] = None,
    home_header: Option[io.circe.Json] = None,
    owner_configured_content_level: Option[Int] = None,
    application_command_counts: Option[io.circe.Json] = None,
    version: Option[Long] = None,
    nsfw: Option[Boolean] = None,
    premium_features: Option[io.circe.Json] = None
)

object DiscordIntents {
  val GUILDS = 1 << 0 // 1
  val GUILD_MEMBERS = 1 << 1 // 2
  val GUILD_MODERATION = 1 << 2 // 4
  val GUILD_EMOJIS_AND_STICKERS = 1 << 3 // 8
  val GUILD_INTEGRATIONS = 1 << 4 // 16
  val GUILD_WEBHOOKS = 1 << 5 // 32
  val GUILD_INVITES = 1 << 6 // 64
  val GUILD_VOICE_STATES = 1 << 7 // 128
  val GUILD_PRESENCES = 1 << 8 // 256
  val GUILD_MESSAGES = 1 << 9 // 512
  val GUILD_MESSAGE_REACTIONS = 1 << 10 // 1024
  val GUILD_MESSAGE_TYPING = 1 << 11 // 2048
  val DIRECT_MESSAGES = 1 << 12 // 4096
  val DIRECT_MESSAGE_REACTIONS = 1 << 13 // 8192
  val DIRECT_MESSAGE_TYPING = 1 << 14 // 16384
  val MESSAGE_CONTENT = 1 << 15 // 32768
  val GUILD_SCHEDULED_EVENTS = 1 << 16 // 65536
  val AUTO_MODERATION_CONFIGURATION = 1 << 20 // 1048576
  val AUTO_MODERATION_EXECUTION = 1 << 21 // 2097152

  val BOT_DEFAULT =
    GUILDS | GUILD_MESSAGES | MESSAGE_CONTENT | GUILD_VOICE_STATES
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

  given Decoder[SlashCommandOptionOption] = deriveDecoder
  given Encoder[SlashCommandOptionOption] = deriveEncoder

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

  given Decoder[GuildCreate] = deriveDecoder
  given Encoder[GuildCreate] = deriveEncoder

  given Decoder[MessageEmbed] = deriveDecoder
  given Encoder[MessageEmbed] = deriveEncoder

  given Decoder[EmbedField] = deriveDecoder
  given Encoder[EmbedField] = deriveEncoder

  given Decoder[EmbedThumbnail] = deriveDecoder
  given Encoder[EmbedThumbnail] = deriveEncoder

  given Decoder[MessageComponent] = deriveDecoder
  given Encoder[MessageComponent] = deriveEncoder
}
