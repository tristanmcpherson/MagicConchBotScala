package dev.raegous.magicconch.commands

import cats.effect.*
import cats.effect.implicits.*
import cats.implicits.*
import dev.raegous.magicconch.*
import dev.raegous.magicconch.audio.VoiceManager
import dev.raegous.magicconch.music.{TrackExtractor, YouTubeSearchClient}
import org.typelevel.log4cats.Logger
import dev.raegous.magicconch.discord.*
import dev.raegous.magicconch.guilds.GuildSettingsManager

class CommandRegistry[F[_]: Async](
    voiceManager: VoiceManager[F],
    trackExtractor: TrackExtractor[F],
    youtubeSearch: YouTubeSearchClient[F],
    guildSettings: GuildSettingsManager[F],
    discordApi: DiscordApiClient[F],
    applicationId: String
)(using Logger[F]) {

  private val commands: Map[String, Command[F]] = Map(
    "play" -> new PlayCommand[F](voiceManager, trackExtractor),
    "stop" -> new StopCommand[F](voiceManager),
    "skip" -> new SkipCommand[F](voiceManager),
    "queue" -> new QueueCommand[F](voiceManager),
    "leave" -> new LeaveCommand[F](voiceManager),
    "magicconch" -> new MagicConchCommand[F](),
    "search" -> new SearchCommand[F](voiceManager, youtubeSearch)
  )

  /** Interaction router for handling component interactions (buttons, select
    * menus)
    */
  val interactionRouter: InteractionRouter[F] = {
    val searchHandler = new SearchInteractionHandler[F](
      voiceManager,
      trackExtractor,
      discordApi,
      applicationId
    )
    val playerHandler =
      new PlayerInteractionHandler[F](voiceManager, discordApi, guildSettings)

    InteractionRouter[F](searchHandler, playerHandler)
  }

  def execute(
      commandName: String,
      context: CommandContext[F]
  ): F[CommandResult] = {
    commands
      .get(commandName.toLowerCase)
      .fold(
        CommandResult(s"❌ Unknown command: $commandName", isError = true)
          .pure[F]
      )(command =>
        Logger[F].info(s"Executing command: $commandName") >>
          command.execute(context)
      )
  }

  def hasCommand(commandName: String): Boolean = {
    commands.contains(commandName.toLowerCase)
  }

  def getCommand(commandName: String): Option[Command[F]] = {
    commands.get(commandName.toLowerCase)
  }

  def listCommands: List[String] = {
    commands.keys.toList.sorted
  }

  /** Get all slash command definitions for Discord registration */
  def getSlashCommands: List[SlashCommand] = {
    commands.values.map(_.toSlashCommand).toList
  }
}
