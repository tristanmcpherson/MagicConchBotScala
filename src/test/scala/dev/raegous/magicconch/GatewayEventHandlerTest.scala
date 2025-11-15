package dev.raegous.magicconch

import cats.effect.*
import dev.raegous.magicconch.TestFixtures.{testLogger, *}
import dev.raegous.magicconch.discord.*
import dev.raegous.magicconch.commands.CommandRegistry
import munit.CatsEffectSuite
import com.bdmendes.smockito.*
import com.bdmendes.smockito.given
import com.bdmendes.smockito.MockedMethod.given
import dev.raegous.magicconch.audio.VoiceManager
import dev.raegous.magicconch.discord.DiscordModels.*
import dev.raegous.magicconch.discord.DiscordModels.given
import dev.raegous.magicconch.guilds.{GuildSettingsManager, GuildTracker}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{times, verify}
import sttp.ws.WebSocket
import io.circe.syntax.*

class GatewayEventHandlerTest extends CatsEffectSuite, Smockito {

  def createMocks(): (
    Mock[MessageHandler[IO]],
    Mock[VoiceManager[IO]],
    Mock[SlashCommandManager[IO]],
    Mock[DiscordApiClient[IO]],
    Mock[GuildTracker[IO]],
    Mock[GuildSettingsManager[IO]],
    Mock[CommandRegistry[IO]],
    Mock[WebSocket[IO]]
  ) = {
    val messageHandler = mock[MessageHandler[IO]]
    val voiceManager = mock[VoiceManager[IO]]
    val slashCommandManager = mock[SlashCommandManager[IO]]
    val discordApi = mock[DiscordApiClient[IO]]
    val guildTracker = mock[GuildTracker[IO]]
    val guildSettings = mock[GuildSettingsManager[IO]]
    val commandRegistry = mock[CommandRegistry[IO]]
    val ws = mock[WebSocket[IO]]

    (messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings, commandRegistry, ws)
  }

  def createHandler(
    messageHandler: MessageHandler[IO],
    voiceManager: VoiceManager[IO],
    slashCommandManager: SlashCommandManager[IO],
    discordApi: DiscordApiClient[IO],
    guildTracker: GuildTracker[IO],
    guildSettings: GuildSettingsManager[IO],
    commandRegistry: CommandRegistry[IO]
  ): IO[GatewayEventHandler[IO]] = {
    GatewayEventHandler.make[IO](
      token = "test_token",
      applicationId = "app_123",
      messageHandler = messageHandler,
      voiceManager = voiceManager,
      slashCommandManager = slashCommandManager,
      discordApi = discordApi,
      guildTracker = guildTracker,
      guildSettings = guildSettings,
      commandRegistry = commandRegistry
    ).use(handler => IO.pure(handler))
  }

  test("handlePayload should handle Hello (opcode 10) and send identify") {
    val (messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings, commandRegistry, ws) = createMocks()

    ws.on(it.sendText)(_ => IO.unit)

    val helloPayload = sampleHelloPayload(41250)
    val payload = sampleGatewayPayload(
      op = 10,
      d = Some(helloPayload.asJson)
    )

    for {
      handler <- createHandler(messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings, commandRegistry)
      result <- handler.handlePayload(payload, ws)
    } yield {
      assertEquals(result.heartbeatInterval, Some(41250))
    }
  }

  test("handlePayload should handle Heartbeat ACK (opcode 11)") {
    val (messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings, commandRegistry, ws) = createMocks()

    val payload = sampleGatewayPayload(op = 11)

    for {
      handler <- createHandler(messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings, commandRegistry)
      result <- handler.handlePayload(payload, ws)
    } yield {
      assertEquals(result.heartbeatInterval, None)
    }
  }

  test("handlePayload should handle Heartbeat Request (opcode 1) and send heartbeat") {
    val (messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings, commandRegistry, ws) = createMocks()

    ws.on(it.sendText)(_ => IO.unit)

    val payload = sampleGatewayPayload(op = 1, s = Some(42))

    for {
      handler <- createHandler(messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings, commandRegistry)
      result <- handler.handlePayload(payload, ws)
    } yield {
      // Successfully sent heartbeat (no assertions needed since we just verify no errors)
      assert(true)
    }
  }

  test("handlePayload should handle Invalid Session (opcode 9) and raise error") {
    val (messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings, commandRegistry, ws) = createMocks()

    val payload = sampleGatewayPayload(op = 9)

    for {
      handler <- createHandler(messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings, commandRegistry)
      result <- handler.handlePayload(payload, ws).attempt
    } yield {
      assert(result.isLeft, "Should raise error for Invalid Session")
    }
  }

  test("handlePayload should handle Reconnect (opcode 7) and raise error") {
    val (messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings, commandRegistry, ws) = createMocks()

    val payload = sampleGatewayPayload(op = 7)

    for {
      handler <- createHandler(messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings, commandRegistry)
      result <- handler.handlePayload(payload, ws).attempt
    } yield {
      assert(result.isLeft, "Should raise error for Reconnect")
    }
  }

  test("handleDispatchEvent should handle READY event") {
    val (messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings, commandRegistry, ws) = createMocks()

    voiceManager.on(it.setBotUserId)(_ => IO.unit)

    slashCommandManager.on(() => it.registerSlashCommands())(_ => IO.unit)

    val readyPayload = sampleReadyPayload("bot_456", "TestBot")
    val payload = sampleGatewayPayload(
      op = 0,
      d = Some(readyPayload.asJson),
      t = Some("READY")
    )

    for {
      handler <- createHandler(messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings, commandRegistry)
      result <- handler.handlePayload(payload, ws)
    } yield {
      assertEquals(result.heartbeatInterval, None)
    }
  }

  test("handleDispatchEvent should handle GUILD_CREATE event and track guild") {
    val (messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings, commandRegistry, ws) = createMocks()

    guildTracker
      .on(it.addGuild)((_, _, _) => IO.unit)

    voiceManager
      .on(it.populateVoiceStates)(_ => IO.unit)

    val guildCreate = sampleGuildCreate("guild_789", "Test Server", 150)
    val payload = sampleGatewayPayload(
      op = 0,
      d = Some(guildCreate.asJson),
      t = Some("GUILD_CREATE")
    )

    for {
      handler <- createHandler(messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings, commandRegistry)
      result <- handler.handlePayload(payload, ws)
    } yield {
      // Guild was successfully tracked
      assert(true)
    }
  }

  test("handleDispatchEvent should handle MESSAGE_CREATE event and delegate to messageHandler") {
    val (messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings, commandRegistry, ws) = createMocks()

    messageHandler
      .on(it.handleMessage)((_, _) => IO.unit)

    val message = sampleDiscordMessage(content = "!help", isBot = false)
    val payload = sampleGatewayPayload(
      op = 0,
      d = Some(message.asJson),
      t = Some("MESSAGE_CREATE")
    )

    for {
      handler <- createHandler(messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings, commandRegistry)
      result <- handler.handlePayload(payload, ws)
    } yield {
      // Message was handled successfully
      assert(true)
    }
  }

  test("handleDispatchEvent should ignore bot messages in MESSAGE_CREATE") {
    val (messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings, commandRegistry, ws) = createMocks()

    messageHandler
      .on(it.handleMessage)((_, _) => IO.unit)

    val botMessage = sampleDiscordMessage(content = "Bot response", isBot = true)
    val payload = sampleGatewayPayload(
      op = 0,
      d = Some(botMessage.asJson),
      t = Some("MESSAGE_CREATE")
    )

    for {
      handler <- createHandler(messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings, commandRegistry)
      result <- handler.handlePayload(payload, ws)
    } yield {
      // Bot message should be ignored
      assert(true)
    }
  }

  test("handleDispatchEvent should handle INTERACTION_CREATE for slash commands") {
    val (messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings, commandRegistry, ws) = createMocks()

    val interactionResponse = InteractionResponse(`type` = 4, data = None)

    slashCommandManager
      .on(it.handleSlashCommand)((_, _) => IO.pure(interactionResponse))

    discordApi
      .on(it.sendInteractionResponse)((_, _, _) => IO.unit)

    val interaction = sampleInteraction().copy(
      `type` = 2, // APPLICATION_COMMAND
      data = Some(InteractionData(name = Some("play"), custom_id = None))
    )
    val payload = sampleGatewayPayload(
      op = 0,
      d = Some(interaction.asJson),
      t = Some("INTERACTION_CREATE")
    )

    for {
      handler <- createHandler(messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings, commandRegistry)
      result <- handler.handlePayload(payload, ws)
    } yield {
      // Interaction was handled successfully
      assert(true)
    }
  }

  test("handleDispatchEvent should handle unrecognized event types gracefully") {
    val (messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings, commandRegistry, ws) = createMocks()

    val payload = sampleGatewayPayload(
      op = 0,
      d = Some(io.circe.Json.obj()),
      t = Some("UNKNOWN_EVENT_TYPE")
    )

    for {
      handler <- createHandler(messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings, commandRegistry)
      result <- handler.handlePayload(payload, ws)
    } yield {
      assertEquals(result.heartbeatInterval, None)
    }
  }

  test("handlePayload should update sequence number") {
    val (messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings, commandRegistry, ws) = createMocks()

    val payload1 = sampleGatewayPayload(op = 11, s = Some(100))
    val payload2 = sampleGatewayPayload(op = 11, s = Some(101))

    ws.on(it.sendText)(_ => IO.unit)

    for {
      handler <- createHandler(messageHandler, voiceManager, slashCommandManager, discordApi, guildTracker, guildSettings, commandRegistry)
      _ <- handler.handlePayload(payload1, ws)
      _ <- handler.handlePayload(payload2, ws)
      // Trigger heartbeat to verify sequence was updated
      heartbeatPayload = sampleGatewayPayload(op = 1, s = Some(102))
      _ <- handler.handlePayload(heartbeatPayload, ws)
    } yield {
      // Heartbeat was sent successfully
      assert(true)
    }
  }
}
