# Testing Guide

This guide covers the testing approach and utilities in the Magic Conch project.

## Overview

We use **MUnit** with **Cats Effect** integration and **Smockito** for mocking. To reduce boilerplate, we've created reusable mock fixtures.

## Quick Start

### Before (Boilerplate Hell)

```scala
class PlayerInteractionHandlerTest extends CatsEffectSuite, Smockito {

  def createMocks(): (Mock[VoiceManager[IO]], Mock[DiscordApiClient[IO]], Mock[GuildSettingsManager[IO]]) = {
    val voiceManager = mock[VoiceManager[IO]]
    val discordApi = mock[DiscordApiClient[IO]]
    val guildSettings = mock[GuildSettingsManager[IO]]
    (voiceManager, discordApi, guildSettings)
  }

  test("my test") {
    val (voiceManager, discordApi, guildSettings) = createMocks()

    guildSettings
      .on(it.getVolume)(_ => IO.pure(0.5))
      .on(it.setVolume)((_, _) => IO.unit)

    discordApi
      .on(it.sendInteractionResponse)((_, _, _) => IO.unit)

    val handler = new PlayerInteractionHandler[IO](voiceManager, discordApi, guildSettings)
    // ... test code
  }
}
```

### After (Clean & Simple)

```scala
class PlayerInteractionHandlerTest extends CatsEffectSuite, Smockito {

  test("my test") {
    val mocks = PlayerMocks[IO]()

    withVolume(mocks.guildSettings, "guild123", 0.5)
    withDiscordApi(mocks.discordApi)

    val handler = mocks.createHandler()
    // ... test code
  }
}
```

## Available Mock Fixtures

### PlayerMocks

For testing `PlayerInteractionHandler`:

```scala
val mocks = PlayerMocks[IO]()
// Access: mocks.voiceManager, mocks.discordApi, mocks.guildSettings
val handler = mocks.createHandler()
```

### SearchMocks

For testing `SearchInteractionHandler`:

```scala
val mocks = SearchMocks[IO](applicationId = "app_123")
// Access: mocks.voiceManager, mocks.trackExtractor, mocks.discordApi
val handler = mocks.createHandler()
```

### CommandMocks

For testing commands:

```scala
val mocks = CommandMocks[IO]()
// Access: mocks.voiceManager, mocks.trackExtractor, mocks.youtubeSearch, mocks.guildSettings

val playCmd = mocks.createPlayCommand()
val searchCmd = mocks.createSearchCommand()
val stopCmd = mocks.createStopCommand()
val skipCmd = mocks.createSkipCommand()
```

### GatewayMocks

For testing `GatewayEventHandler`:

```scala
val mocks = GatewayMocks[IO](
  token = "test_token",
  applicationId = "app_123"
)
// Access: all gateway dependencies

val handlerResource = mocks.createHandler()
handlerResource.use { handler =>
  // ... test code
}
```

### RouterMocks

For testing `InteractionRouter`:

```scala
val mocks = RouterMocks[IO]()
  .withHandler(searchHandler)
  .withHandler(playerHandler)

val router = mocks.createRouter()
```

## Setup Utilities

The `MockFixtures.Setups` object provides common mock configurations:

### withQueue

Setup VoiceManager with a default queue:

```scala
import MockFixtures.Setups.*

val queue = sampleMusicQueue(
  tracks = List(sampleMusicTrack()),
  isPlaying = true
)
withQueue(mocks.voiceManager, "guild123", queue)
```

### withVolume

Setup GuildSettings with default volume:

```scala
withVolume(mocks.guildSettings, "guild123", 0.5)
// Configures both getVolume and setVolume
```

### withDiscordApi

Setup DiscordApi to accept any interaction response:

```scala
withDiscordApi(mocks.discordApi)
// Configures sendInteractionResponse and editInteractionResponse
```

### withSearchResults

Setup VoiceManager with search results:

```scala
val results = List(sampleYouTubeSearchResult())
withSearchResults(mocks.voiceManager, "user123", results)
```

### withTrackExtraction

Setup TrackExtractor to return a track:

```scala
val track = Some(sampleMusicTrack())
withTrackExtraction(mocks.trackExtractor, "https://youtube.com/...", track)
```

## Test Fixtures (Domain Objects)

The `TestFixtures` object provides sample domain objects:

```scala
import TestFixtures.*

// Users & Members
val user = sampleUser(id = "123", username = "testuser")
val member = sampleGuildMember(userId = "123")

// Interactions
val interaction = sampleInteraction(
  customId = Some("button_id"),
  guildId = Some("guild123"),
  userId = "user123"
)

// Music
val track = sampleMusicTrack(
  url = "https://youtube.com/watch?v=abc",
  title = "Test Song"
)
val queue = sampleMusicQueue(
  tracks = List(track),
  currentTrack = Some(track),
  isPlaying = true
)

// Search Results
val searchResult = sampleYouTubeSearchResult(
  videoId = "abc123",
  title = "Test Video"
)

// Gateway Payloads
val payload = sampleGatewayPayload(op = 0)
val ready = sampleReadyPayload(userId = "bot_123")
val hello = sampleHelloPayload(heartbeatInterval = 41250)
val guildCreate = sampleGuildCreate(id = "guild123", name = "Test Guild")

// Messages
val message = sampleDiscordMessage(
  content = "!play https://...",
  authorId = "user123"
)
```

## Advanced Mock Configuration

When you need more control, access mocks directly:

```scala
val mocks = PlayerMocks[IO]()

// Custom stateful behavior
var pausedState = false
mocks.voiceManager
  .on(it.isPaused)(_ => IO.pure(pausedState))
  .on(it.pausePlayback)(_ => IO {
    pausedState = true
  })

// Multiple return values
var callCount = 0
mocks.voiceManager
  .on(it.getQueue)(_ => IO {
    callCount += 1
    if (callCount == 1) queueBefore else queueAfter
  })

val handler = mocks.createHandler()
```

## Best Practices

### 1. Use Setup Utilities When Possible

```scala
// Good
withVolume(mocks.guildSettings, "guild123", 0.5)

// Avoid (unless you need custom behavior)
mocks.guildSettings
  .on(it.getVolume)(_ => IO.pure(0.5))
  .on(it.setVolume)((_, _) => IO.unit)
```

### 2. Create Handler After Mock Setup

```scala
// Good
val mocks = PlayerMocks[IO]()
withVolume(mocks.guildSettings, "guild123", 0.5)
val handler = mocks.createHandler()

// Bad - handler created before mocks configured
val handler = mocks.createHandler()
withVolume(mocks.guildSettings, "guild123", 0.5)
```

### 3. Use Sample Fixtures for Domain Objects

```scala
// Good
val track = sampleMusicTrack(title = "My Song")

// Avoid - verbose and error-prone
val track = MusicTrack(
  url = "https://...",
  title = "My Song",
  duration = Some(180),
  requestedBy = "user"
)
```

### 4. Test One Thing Per Test

```scala
// Good
test("volume increase should add 10%") { /* ... */ }
test("volume should clamp at 200%") { /* ... */ }

// Avoid
test("volume controls") {
  // Tests 10 different scenarios
}
```

## Running Tests

```bash
# Run all tests
sbt test

# Run specific test suite
sbt "testOnly dev.raegous.magicconch.commands.PlayerInteractionHandlerTest"

# Run specific test
sbt "testOnly *PlayerInteractionHandlerTest -- *volume increase*"

# Run with coverage
sbt coverage test coverageReport
```

## Adding New Mock Fixtures

To add a new mock fixture for a new component:

1. Add a case class to `MockFixtures.scala`:

```scala
case class MyComponentMocks[F[_]: Async](
  dependency1: Mock[Dep1[F]] = mock[Dep1[F]],
  dependency2: Mock[Dep2[F]] = mock[Dep2[F]]
) extends MockFixture[F, MyComponent[F]] {

  def createHandler()(using Logger[F]): MyComponent[F] = {
    new MyComponent[F](dependency1, dependency2)
  }
}
```

2. Add setup utilities if needed:

```scala
object Setups {
  // ... existing setups ...

  def withMyComponentConfig[F[_]](
    mock: Mock[MyComponent[F]],
    value: String
  ): Mock[MyComponent[F]] = {
    mock.on(it.getValue)(_ => IO.pure(value).asInstanceOf[F[String]])
    mock
  }
}
```

3. Use in tests:

```scala
val mocks = MyComponentMocks[IO]()
withMyComponentConfig(mocks.dependency1, "test")
val component = mocks.createHandler()
```

## Common Patterns

### Testing Error Cases

```scala
test("should handle errors gracefully") {
  val mocks = CommandMocks[IO]()

  mocks.trackExtractor
    .on(it.extractTrackInfo)(_ => IO.pure(None)) // Simulate failure

  val cmd = mocks.createPlayCommand()

  cmd.execute(context).map { result =>
    assert(result.isError)
    assert(result.message.contains("Failed"))
  }
}
```

### Testing Stateful Behavior

```scala
test("should track state changes") {
  val mocks = PlayerMocks[IO]()

  var volume = 0.5
  mocks.guildSettings
    .on(it.getVolume)(_ => IO.pure(volume))
    .on(it.setVolume)((_, v) => IO { volume = v })

  val handler = mocks.createHandler()

  for {
    _ <- handler.handle("volume_user_inc10", interaction, None)
    _ <- IO { assert(volume == 0.6) }
  } yield ()
}
```

### Testing Async Interactions

```scala
test("should handle async operations") {
  val mocks = SearchMocks[IO]()

  mocks.trackExtractor
    .on(it.extractTrackInfo)(_ =>
      IO.sleep(100.millis) >> IO.pure(Some(sampleMusicTrack()))
    )

  val handler = mocks.createHandler()

  handler.handle(customId, interaction, None).map { _ =>
    verify(mocks.voiceManager).addToQueue(any, any)
  }
}
```

## Troubleshooting

### "Mock not configured" errors

Make sure you configure mocks **before** creating the handler:

```scala
// Wrong
val handler = mocks.createHandler()
withVolume(mocks.guildSettings, "guild123", 0.5)

// Correct
withVolume(mocks.guildSettings, "guild123", 0.5)
val handler = mocks.createHandler()
```

### Type inference issues with Smockito

If you get type errors, use explicit `.asInstanceOf[F[T]]`:

```scala
mock.on(it.myMethod)(_ => IO.pure(value).asInstanceOf[F[ValueType]])
```

### Verification failures

Remember to use `any[Type]` for arguments you don't care about:

```scala
verify(mocks.discordApi).sendInteractionResponse(
  any[String],  // interaction ID
  any[String],  // token
  any[String]   // response JSON
)
```
