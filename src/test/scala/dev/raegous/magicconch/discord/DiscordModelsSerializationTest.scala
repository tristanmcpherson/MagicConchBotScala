package dev.raegous.magicconch.discord

import io.circe.parser.*
import io.circe.syntax.*
import dev.raegous.magicconch.discord.DiscordModels.*
import dev.raegous.magicconch.discord.DiscordModels.given
import munit.FunSuite

class DiscordModelsSerializationTest extends FunSuite {

  test("User should serialize and deserialize") {
    val user = User(
      id = "123456789",
      username = "testuser",
      discriminator = "0001",
      bot = Some(false),
      avatar = Some("avatar_hash"),
      global_name = Some("Test User")
    )

    val json = user.asJson.noSpaces
    val decoded = decode[User](json)

    decoded match {
      case Right(decodedUser) =>
        assertEquals(decodedUser.id, user.id)
        assertEquals(decodedUser.username, user.username)
        assertEquals(decodedUser.discriminator, user.discriminator)
        assertEquals(decodedUser.bot, user.bot)
      case Left(error) =>
        fail(s"Failed to decode User: $error")
    }
  }

  test("Interaction should deserialize with optional fields") {
    val json = """{
      "id": "interaction_123",
      "type": 3,
      "token": "token_abc",
      "guild_id": "guild_456",
      "channel_id": "channel_789",
      "data": {
        "custom_id": "button_test"
      },
      "member": {
        "user": {
          "id": "user_123",
          "username": "testuser",
          "discriminator": "0001"
        },
        "roles": [],
        "joined_at": "2024-01-01T00:00:00.000Z"
      }
    }"""

    decode[Interaction](json) match {
      case Right(interaction) =>
        assertEquals(interaction.id, "interaction_123")
        assertEquals(interaction.`type`, 3)
        assertEquals(interaction.guild_id, Some("guild_456"))
        assertEquals(interaction.data.flatMap(_.custom_id), Some("button_test"))
        assert(interaction.member.isDefined)
      case Left(error) =>
        fail(s"Failed to decode Interaction: $error")
    }
  }

  test("MessageComponent should serialize with nested components") {
    val button = MessageComponent(
      `type` = 2, // Button
      style = Some(1),
      label = Some("Click Me"),
      custom_id = Some("button_test")
    )

    val actionRow = MessageComponent(
      `type` = 1, // Action Row
      components = Some(List(button))
    )

    val json = actionRow.asJson.noSpaces
    val decoded = decode[MessageComponent](json)

    decoded match {
      case Right(decodedRow) =>
        assertEquals(decodedRow.`type`, 1)
        assert(decodedRow.components.isDefined)
        assertEquals(decodedRow.components.get.length, 1)
        assertEquals(decodedRow.components.get.head.label, Some("Click Me"))
      case Left(error) =>
        fail(s"Failed to decode MessageComponent: $error")
    }
  }

  test("MessageEmbed should serialize and deserialize") {
    val embed = MessageEmbed(
      title = Some("Test Embed"),
      description = Some("This is a test embed"),
      color = Some(0x00FF00),
      fields = Some(List(
        EmbedField(name = "Field 1", value = "Value 1", inline = Some(true)),
        EmbedField(name = "Field 2", value = "Value 2", inline = Some(false))
      ))
    )

    val json = embed.asJson.noSpaces
    val decoded = decode[MessageEmbed](json)

    decoded match {
      case Right(decodedEmbed) =>
        assertEquals(decodedEmbed.title, Some("Test Embed"))
        assertEquals(decodedEmbed.description, Some("This is a test embed"))
        assertEquals(decodedEmbed.color, Some(0x00FF00))
        assert(decodedEmbed.fields.isDefined)
        assertEquals(decodedEmbed.fields.get.length, 2)
      case Left(error) =>
        fail(s"Failed to decode MessageEmbed: $error")
    }
  }

  test("InteractionResponse should serialize correctly") {
    val response = InteractionResponse(
      `type` = 4, // CHANNEL_MESSAGE_WITH_SOURCE
      data = Some(InteractionResponseData(
        content = Some("Test response"),
        flags = Some(64) // Ephemeral
      ))
    )

    val json = response.asJson.noSpaces
    val decoded = decode[InteractionResponse](json)

    decoded match {
      case Right(decodedResponse) =>
        assertEquals(decodedResponse.`type`, 4)
        assert(decodedResponse.data.isDefined)
        assertEquals(decodedResponse.data.flatMap(_.content), Some("Test response"))
        assertEquals(decodedResponse.data.flatMap(_.flags), Some(64))
      case Left(error) =>
        fail(s"Failed to decode InteractionResponse: $error")
    }
  }

  test("MusicQueue should serialize and deserialize") {
    val track1 = MusicTrack(
      url = "https://youtube.com/watch?v=test1",
      title = "Track 1",
      duration = Some(180),
      requestedBy = "user1"
    )

    val track2 = MusicTrack(
      url = "https://youtube.com/watch?v=test2",
      title = "Track 2",
      duration = Some(240),
      requestedBy = "user2"
    )

    val queue = MusicQueue(
      tracks = List(track1, track2),
      currentTrack = Some(track1),
      isPlaying = true,
      isPaused = false,
      currentPosition = 45,
      startTime = Some(1234567890L)
    )

    val json = queue.asJson.noSpaces
    val decoded = decode[MusicQueue](json)

    decoded match {
      case Right(decodedQueue) =>
        assertEquals(decodedQueue.tracks.length, 2)
        assertEquals(decodedQueue.currentTrack.get.title, "Track 1")
        assertEquals(decodedQueue.isPlaying, true)
        assertEquals(decodedQueue.currentPosition, 45)
        assertEquals(decodedQueue.startTime, Some(1234567890L))
      case Left(error) =>
        fail(s"Failed to decode MusicQueue: $error")
    }
  }

  test("VoiceStateUpdate should deserialize with optional fields") {
    val json = """{
      "guild_id": "guild_123",
      "channel_id": "channel_456",
      "user_id": "user_789",
      "session_id": "session_abc",
      "deaf": false,
      "mute": false,
      "self_deaf": true,
      "self_mute": false
    }"""

    decode[VoiceStateUpdate](json) match {
      case Right(voiceState) =>
        assertEquals(voiceState.guild_id, Some("guild_123"))
        assertEquals(voiceState.channel_id, Some("channel_456"))
        assertEquals(voiceState.user_id, "user_789")
        assertEquals(voiceState.session_id, "session_abc")
        assertEquals(voiceState.self_deaf, Some(true))
      case Left(error) =>
        fail(s"Failed to decode VoiceStateUpdate: $error")
    }
  }

  test("VoiceStateUpdate should handle null channel_id (user left voice)") {
    val json = """{
      "guild_id": "guild_123",
      "channel_id": null,
      "user_id": "user_789",
      "session_id": "session_abc",
      "deaf": false,
      "mute": false
    }"""

    decode[VoiceStateUpdate](json) match {
      case Right(voiceState) =>
        assertEquals(voiceState.channel_id, None)
        assertEquals(voiceState.user_id, "user_789")
      case Left(error) =>
        fail(s"Failed to decode VoiceStateUpdate with null channel_id: $error")
    }
  }

  test("GuildCreate should deserialize with complex nested data") {
    val json = """{
      "id": "guild_123",
      "name": "Test Guild",
      "icon": "icon_hash",
      "owner_id": "owner_123",
      "afk_timeout": 300,
      "verification_level": 1,
      "default_message_notifications": 0,
      "explicit_content_filter": 0,
      "roles": [],
      "emojis": [],
      "features": ["COMMUNITY"],
      "mfa_level": 0,
      "system_channel_flags": 0,
      "joined_at": "2024-01-01T00:00:00.000Z",
      "large": false,
      "member_count": 100,
      "voice_states": [{
        "channel_id": "channel_456",
        "user_id": "user_789",
        "session_id": "session_abc",
        "deaf": false,
        "mute": false
      }]
    }"""

    decode[GuildCreate](json) match {
      case Right(guild) =>
        assertEquals(guild.id, "guild_123")
        assertEquals(guild.name, "Test Guild")
        assertEquals(guild.member_count, 100)
        assert(guild.voice_states.isDefined)
        assertEquals(guild.voice_states.get.length, 1)
      case Left(error) =>
        fail(s"Failed to decode GuildCreate: $error")
    }
  }

  test("InteractionData should handle both string and int IDs") {
    val jsonWithStringId = """{
      "id": "command_123",
      "name": "test",
      "type": 1
    }"""

    val jsonWithIntId = """{
      "id": 123,
      "name": "test",
      "type": 2
    }"""

    val decoded1 = decode[InteractionData](jsonWithStringId)
    val decoded2 = decode[InteractionData](jsonWithIntId)

    assert(decoded1.isRight, "Should decode string ID")
    assert(decoded2.isRight, "Should decode int ID")
  }

  test("MessageComponent should handle missing optional fields") {
    val json = """{
      "type": 2
    }"""

    decode[MessageComponent](json) match {
      case Right(component) =>
        assertEquals(component.`type`, 2)
        assertEquals(component.label, None)
        assertEquals(component.custom_id, None)
        assertEquals(component.style, None)
      case Left(error) =>
        fail(s"Failed to decode minimal MessageComponent: $error")
    }
  }

  test("EncryptionMode should have correct string values") {
    assertEquals(EncryptionMode.AeadAes256GcmRtpSize.value, "aead_aes256_gcm_rtpsize")
    assertEquals(EncryptionMode.AeadXChaCha20Poly1305RtpSize.value, "aead_xchacha20_poly1305_rtpsize")
  }

  test("EncryptionMode.fromString should parse correctly") {
    assertEquals(
      EncryptionMode.fromString("aead_aes256_gcm_rtpsize"),
      Some(EncryptionMode.AeadAes256GcmRtpSize)
    )
    assertEquals(
      EncryptionMode.fromString("aead_xchacha20_poly1305_rtpsize"),
      Some(EncryptionMode.AeadXChaCha20Poly1305RtpSize)
    )
    assertEquals(
      EncryptionMode.fromString("unknown"),
      None
    )
  }

  test("DiscordIntents should have correct values") {
    assertEquals(DiscordIntents.GUILDS, 1)
    assertEquals(DiscordIntents.GUILD_MESSAGES, 512)
    assertEquals(DiscordIntents.MESSAGE_CONTENT, 32768)
    assertEquals(DiscordIntents.GUILD_VOICE_STATES, 128)

    val botDefault = DiscordIntents.BOT_DEFAULT
    assert((botDefault & DiscordIntents.GUILDS) != 0, "BOT_DEFAULT should include GUILDS")
    assert((botDefault & DiscordIntents.GUILD_MESSAGES) != 0, "BOT_DEFAULT should include GUILD_MESSAGES")
    assert((botDefault & DiscordIntents.MESSAGE_CONTENT) != 0, "BOT_DEFAULT should include MESSAGE_CONTENT")
    assert((botDefault & DiscordIntents.GUILD_VOICE_STATES) != 0, "BOT_DEFAULT should include GUILD_VOICE_STATES")
  }
}
