package dev.raegous.magicconch.discord

import cats.effect.*
import org.typelevel.log4cats.Logger
import sttp.client4.*
import io.circe.syntax.*
import cats.implicits.*
import dev.raegous.magicconch.commands.*
import dev.raegous.magicconch.discord.*
import dev.raegous.magicconch.discord.DiscordModels.*
import dev.raegous.magicconch.discord.DiscordModels.given

class SlashCommandManager[F[_]: Async](
  token: String,
  applicationId: String,
  discordApi: DiscordApiClient[F],
  commandRegistry: CommandRegistry[F]
)(using Logger[F]) {

  // Optional: List of test guild IDs where commands should be registered for faster updates
  // Guild commands update instantly, global commands can take up to 1 hour
  // Set via DISCORD_TEST_GUILDS environment variable (comma-separated guild IDs)
  private val testGuildIds: List[String] =
    sys.env.get("DISCORD_TEST_GUILDS")
      .map(_.split(",").map(_.trim).filter(_.nonEmpty).toList)
      .getOrElse(List("820144843396612127", "644004109057261631"))

  def registerSlashCommands(): F[Unit] = {
    if (testGuildIds.nonEmpty) {
      Logger[F].info(s"Registering commands to ${testGuildIds.length} test guilds: ${testGuildIds.mkString(", ")}") >>
      testGuildIds.traverse_(registerSlashCommandsForGuild) >>
      Logger[F].info("All test guild commands registered")
    } else {
      Logger[F].info("Registering global commands (may take up to 1 hour to propagate)") >>
      registerGlobalSlashCommands()
    }
  }

  private def registerGlobalSlashCommands(): F[Unit] = {
    // Get command definitions from the registry
    val desiredCommands = commandRegistry.getSlashCommands
    val desiredCommandNames = desiredCommands.map(_.name).toSet

    discordApi.getGlobalSlashCommands(applicationId).flatMap { allCommands =>
      // IMPORTANT: Only manage commands owned by THIS bot (filter by application_id)
      val existingCommandNames = allCommands.map(_.name).toSet

      // Commands to ADD: in desired but not in Discord
      val commandsToRegister = desiredCommands.filterNot(cmd => existingCommandNames.contains(cmd.name))

      // Commands to DELETE: in Discord but not in desired (removed from registry)
      // Only delete commands owned by this bot!
      val commandsToDelete = allCommands.filterNot(cmd => desiredCommandNames.contains(cmd.name))

      for {
        _ <- Logger[F].info(s"Retrieved ${allCommands.length} total global commands")

        // Delete stale commands first
        _ <- if (commandsToDelete.nonEmpty) {
          Logger[F].info(s"Deleting ${commandsToDelete.length} stale commands: ${commandsToDelete.map(_.name).mkString(", ")}") >>
          commandsToDelete.traverse_ { cmd =>
            discordApi.deleteGlobalSlashCommand(applicationId, cmd.id, cmd.name)
          }
        } else {
          Async[F].unit
        }

        // Then register new commands
        _ <- if (commandsToRegister.nonEmpty) {
          Logger[F].info(s"Registering ${commandsToRegister.length} new commands: ${commandsToRegister.map(_.name).mkString(", ")}") >>
          commandsToRegister.traverse_(registerGlobalCommand)
        } else {
          Logger[F].info(s"${allCommands.length} commands up to date")
        }
      } yield ()
    }
  }

  private def registerSlashCommandsForGuild(guildId: String): F[Unit] = {
    // Get command definitions from the registry
    val desiredCommands = commandRegistry.getSlashCommands
    val desiredCommandNames = desiredCommands.map(_.name).toSet

    discordApi.getGuildSlashCommands(applicationId, guildId).flatMap { allCommands =>
      // IMPORTANT: Only manage commands owned by THIS bot (filter by application_id)
      val existingCommandNames = allCommands.map(_.name).toSet

      // Commands to ADD: in desired but not in Discord
      val commandsToRegister = desiredCommands.filterNot(cmd => existingCommandNames.contains(cmd.name))

      // Commands to DELETE: in Discord but not in desired (removed from registry)
      // Only delete commands owned by this bot!
      val commandsToDelete = allCommands.filterNot(cmd => desiredCommandNames.contains(cmd.name))

      for {
        _ <- Logger[F].info(s"Guild $guildId: Retrieved ${allCommands.length} total commands")

        // Delete stale commands first
        _ <- if (commandsToDelete.nonEmpty) {
          Logger[F].info(s"Guild $guildId: Deleting ${commandsToDelete.length} stale commands: ${commandsToDelete.map(_.name).mkString(", ")}") >>
          commandsToDelete.traverse_ { cmd =>
            discordApi.deleteGuildSlashCommand(applicationId, guildId, cmd.id, cmd.name)
          }
        } else {
          Async[F].unit
        }

        // Then register new commands
        _ <- if (commandsToRegister.nonEmpty) {
          Logger[F].info(s"Guild $guildId: Registering ${commandsToRegister.length} new commands: ${commandsToRegister.map(_.name).mkString(", ")}") >>
          commandsToRegister.traverse_(cmd => registerGuildCommand(guildId, cmd))
        } else {
          Logger[F].info(s"Guild $guildId: ${allCommands.length} commands up to date")
        }
      } yield ()
    }
  }

  private def registerGlobalCommand(command: SlashCommand): F[Unit] = {
    val commandData = Map(
      "name" -> command.name.asJson,
      "description" -> command.description.asJson,
      "options" -> command.options.asJson
    ).asJson

    discordApi
      .registerGlobalSlashCommand(applicationId, commandData.noSpaces)
      .void
  }

  private def registerGuildCommand(guildId: String, command: SlashCommand): F[Unit] = {
    val commandData = Map(
      "name" -> command.name.asJson,
      "description" -> command.description.asJson,
      "options" -> command.options.asJson
    ).asJson

    discordApi
      .registerGuildSlashCommand(applicationId, guildId, commandData.noSpaces)
      .void
  }

  def handleSlashCommand(interaction: Interaction, gatewayWs: Option[sttp.ws.WebSocket[F]] = None): F[InteractionResponse] = {
    val commandName = interaction.data.flatMap(_.name).getOrElse("")

    (interaction.guild_id, commandRegistry.hasCommand(commandName)) match {
      case (Some(guildId), true) =>
        val userId = interaction.member.flatMap(_.user.map(_.id))
          .orElse(interaction.user.map(_.id))
          .getOrElse("")

        val username = interaction.member.flatMap(_.user.map(_.username))
          .orElse(interaction.user.map(_.username))
          .getOrElse("Unknown")

        // Extract command arguments from interaction options
        val args = extractCommandArgs(interaction)

        val context = CommandContext[F](
          guildId = guildId,
          userId = userId,
          channelId = interaction.channel_id,
          username = username,
          gatewayWs = gatewayWs,
          args = args
        )

        commandRegistry.execute(commandName, context).flatMap { result =>
          Logger[F].info(s"Command result - message: ${result.message}, embeds: ${result.embeds.map(_.size)}, components: ${result.components.map(_.size)}") >>
          Async[F].pure(InteractionResponse(
            `type` = 4,
            data = Some(InteractionResponseData(
              content = Some(result.message),
              embeds = result.embeds,
              components = result.components
            ))
          ))
        }
      case (None, _) =>
        Async[F].pure(InteractionResponse(
          `type` = 4,
          data = Some(InteractionResponseData(content = Some("❌ This command only works in a server!")))
        ))
      case (_, false) =>
        Async[F].pure(InteractionResponse(
          `type` = 4,
          data = Some(InteractionResponseData(content = Some(s"❌ Unknown command: $commandName")))
        ))
    }
  }

  private def extractCommandArgs(interaction: Interaction): Map[String, String] = {
    interaction.data.flatMap(_.options).getOrElse(List.empty).flatMap { option =>
      option.value.map(value => option.name -> value)
    }.toMap
  }
}
