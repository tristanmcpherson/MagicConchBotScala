package dev.raegous.magicconch

import cats.effect.*
import com.bdmendes.smockito.*
import dev.raegous.magicconch.TestFixtures.given
import dev.raegous.magicconch.audio.VoiceManager
import dev.raegous.magicconch.commands.PlayerInteractionHandler
import dev.raegous.magicconch.discord.*
import dev.raegous.magicconch.guilds.{GuildSettingsManager, GuildTracker}
import dev.raegous.magicconch.music.{TrackExtractor, YouTubeSearchClient}
import sttp.ws.WebSocket

/** Reusable mock fixtures to reduce boilerplate in tests
  *
  * Usage:
  * ```scala
  * val mocks = PlayerMocks[IO]()
  *
  * // Configure specific mocks
  * mocks.guildSettings
  *   .on(it.getVolume)(_ => IO.pure(0.5))
  *   .on(it.setVolume)((_, _) => IO.unit)
  *
  * // Create handler with mocks
  * val handler = mocks.createHandler()
  * ```
  */
object MockFixtures extends Smockito {

  /** Base trait for mock fixtures
    */
  trait MockFixture[F[_], T] {
    def createHandler()(using org.typelevel.log4cats.Logger[F]): T
  }

  /** Mock fixture for PlayerInteractionHandler tests
    */
  case class PlayerMocks[F[_]: Async](
      voiceManager: Mock[VoiceManager[F]],
      discordApi: Mock[DiscordApiClient[F]],
      guildSettings: Mock[GuildSettingsManager[F]]
  ) extends MockFixture[F, commands.PlayerInteractionHandler[F]] {

    def createHandler()(using
        org.typelevel.log4cats.Logger[F]
    ): commands.PlayerInteractionHandler[F] = {
      new commands.PlayerInteractionHandler[F](
        voiceManager,
        discordApi,
        guildSettings
      )
    }
  }

  object PlayerMocks {
    def apply[F[_]: Async](): PlayerMocks[F] = {
      PlayerMocks(
        mock[VoiceManager[F]],
        mock[DiscordApiClient[F]],
        mock[GuildSettingsManager[F]]
      )
    }
  }

  /** Mock fixture for SearchInteractionHandler tests
    */
  case class SearchMocks[F[_]: Async](
      voiceManager: Mock[VoiceManager[F]],
      trackExtractor: Mock[TrackExtractor[F]],
      discordApi: Mock[DiscordApiClient[F]],
      applicationId: String
  ) extends MockFixture[F, commands.SearchInteractionHandler[F]] {

    def createHandler()(using
        org.typelevel.log4cats.Logger[F]
    ): commands.SearchInteractionHandler[F] = {
      new commands.SearchInteractionHandler[F](
        voiceManager,
        trackExtractor,
        discordApi,
        applicationId
      )
    }
  }

  object SearchMocks {
    def apply[F[_]: Async](
        applicationId: String = "app_123"
    ): SearchMocks[F] = {
      SearchMocks(
        mock[VoiceManager[F]],
        mock[TrackExtractor[F]],
        mock[DiscordApiClient[F]],
        applicationId
      )
    }
  }

  /** Mock fixture for GatewayEventHandler tests
    */
  case class GatewayMocks[F[_]: Async](
      token: String,
      applicationId: String,
      messageHandler: Mock[MessageHandler[F]],
      voiceManager: Mock[VoiceManager[F]],
      trackExtractor: Mock[TrackExtractor[F]],
      slashCommandManager: Mock[SlashCommandManager[F]],
      discordApi: Mock[DiscordApiClient[F]],
      guildTracker: Mock[GuildTracker[F]],
      guildSettings: Mock[GuildSettingsManager[F]],
      commandRegistry: Mock[commands.CommandRegistry[F]]
  ) {

    def createHandler()(using
        org.typelevel.log4cats.Logger[F]
    ): Resource[F, GatewayEventHandler[F]] = {
      GatewayEventHandler.make[F](
        token,
        applicationId,
        messageHandler,
        voiceManager,
        trackExtractor,
        slashCommandManager,
        discordApi,
        guildTracker,
        guildSettings,
        commandRegistry
      )
    }
  }

  object GatewayMocks {
    def apply[F[_]: Async](
        token: String = "test_token",
        applicationId: String = "app_123"
    ): GatewayMocks[F] = {
      GatewayMocks(
        token,
        applicationId,
        mock[MessageHandler[F]],
        mock[VoiceManager[F]],
        mock[TrackExtractor[F]],
        mock[SlashCommandManager[F]],
        mock[DiscordApiClient[F]],
        mock[GuildTracker[F]],
        mock[GuildSettingsManager[F]],
        mock[commands.CommandRegistry[F]]
      )
    }
  }

  /** Mock fixture for InteractionRouter tests
    */
  case class RouterMocks[F[_]: Async](
      handlers: List[commands.ComponentInteractionHandler[F]]
  ) {

    def createRouter(): commands.InteractionRouter[F] = {
      new commands.InteractionRouter[F](handlers)
    }

    def withHandler(
        handler: commands.ComponentInteractionHandler[F]
    ): RouterMocks[F] = {
      copy(handlers = handlers :+ handler)
    }
  }

  object RouterMocks {
    def apply[F[_]: Async](): RouterMocks[F] = {
      RouterMocks(List.empty)
    }
  }

  /** Mock fixture for Command tests (e.g., PlayCommand)
    */
  case class CommandMocks[F[_]: Async](
      voiceManager: Mock[VoiceManager[F]],
      trackExtractor: Mock[TrackExtractor[F]],
      youtubeSearch: Mock[YouTubeSearchClient[F]],
      guildSettings: Mock[GuildSettingsManager[F]]
  ) {

    def createPlayCommand()(using
        org.typelevel.log4cats.Logger[F]
    ): commands.PlayCommand[F] = {
      new commands.PlayCommand[F](voiceManager, trackExtractor)
    }

    def createSearchCommand()(using
        org.typelevel.log4cats.Logger[F]
    ): commands.SearchCommand[F] = {
      new commands.SearchCommand[F](voiceManager, youtubeSearch)
    }

    def createStopCommand()(using
        org.typelevel.log4cats.Logger[F]
    ): commands.StopCommand[F] = {
      new commands.StopCommand[F](voiceManager)
    }

    def createSkipCommand()(using
        org.typelevel.log4cats.Logger[F]
    ): commands.SkipCommand[F] = {
      new commands.SkipCommand[F](voiceManager)
    }
  }

  object CommandMocks {
    def apply[F[_]: Async](): CommandMocks[F] = {
      CommandMocks(
        mock[VoiceManager[F]],
        mock[TrackExtractor[F]],
        mock[YouTubeSearchClient[F]],
        mock[GuildSettingsManager[F]]
      )
    }
  }

  /** Common mock setup utilities
    */
  object Setups {

    /** Setup VoiceManager with a default queue */
    def withQueue[F[_]](
        voiceManager: Mock[VoiceManager[F]],
        guildId: String,
        queue: MusicQueue
    ): Mock[VoiceManager[F]] =
      voiceManager.on(it.getQueue)(_ =>
        IO.pure(queue).asInstanceOf[F[MusicQueue]]
      )

    /** Setup GuildSettings with default volume */
    def withVolume[F[_]](
        guildSettings: Mock[GuildSettingsManager[F]],
        guildId: String,
        volume: Double
    ): Mock[GuildSettingsManager[F]] =
      guildSettings
        .on(it.getVolume)(_ => IO.pure(volume).asInstanceOf[F[Double]])
        .on(it.setVolume)((_, _) => IO.unit.asInstanceOf[F[Unit]])

    /** Setup DiscordApi to accept any interaction response */
    def withDiscordApi[F[_]](
        discordApi: Mock[DiscordApiClient[F]]
    ): Mock[DiscordApiClient[F]] =
      discordApi
        .on(it.sendInteractionResponse)((_, _, _) =>
          IO.unit.asInstanceOf[F[Unit]]
        )
        .on(it.editInteractionResponse)((_, _, _) =>
          IO.unit.asInstanceOf[F[Unit]]
        )

    /** Setup VoiceManager with search results */
    def withSearchResults[F[_]](
        voiceManager: Mock[VoiceManager[F]],
        userId: String,
        results: List[music.YouTubeSearchResult]
    ): Mock[VoiceManager[F]] =
      voiceManager
        .on(it.getSearchResults)(_ =>
          IO.pure(Some(results))
            .asInstanceOf[F[Option[List[music.YouTubeSearchResult]]]]
        )
        .on(it.clearSearchResults)(_ => IO.unit.asInstanceOf[F[Unit]])

    /** Setup TrackExtractor to return a track */
    def withTrackExtraction[F[_]](
        trackExtractor: Mock[TrackExtractor[F]],
        url: String,
        track: Option[MusicTrack]
    ): Mock[TrackExtractor[F]] =
      trackExtractor.on(it.extractTrackInfo)(_ =>
        IO.pure(track).asInstanceOf[F[Option[MusicTrack]]]
      )
  }
}
