package dev.raegous.magicconch.commands

import cats.data.*
import sttp.ws.WebSocket
import cats.effect.implicits.*
import cats.implicits.*
import dev.raegous.magicconch.*
import dev.raegous.magicconch.discord.*

/** Context containing all data needed to execute a command */
case class CommandContext[F[_]](
    guildId: String,
    userId: String,
    channelId: String,
    username: String,
    gatewayWs: Option[WebSocket[F]],
    args: Map[String, String]
)

/** Result of executing a command */
case class CommandResult(
    message: String,
    isError: Boolean = false,
    embeds: Option[List[MessageEmbed]] = None,
    components: Option[List[MessageComponent]] = None
)

/** Specification for a command argument */
case class CommandArgument(
    name: String,
    description: String,
    required: Boolean = false
)

/** Base trait for all bot commands */
trait Command[F[_]] {

  /** The command name (used in !commandname and /commandname) */
  def name: String

  /** Human-readable description of what the command does */
  def description: String

  /** List of arguments this command accepts */
  def arguments: List[CommandArgument]

  /** Execute the command with the given context */
  def execute(context: CommandContext[F]): F[CommandResult]

  /** Convert this command to a Discord slash command definition */
  def toSlashCommand: SlashCommand = {
    SlashCommand(
      id = "",
      application_id = "",
      version = "",
      default_member_permissions = None,
      `type` = 1,
      name = name,
      description = description,
      guild_id = "",
      nsfw = false,
      options = Some(arguments.map { arg =>
        SlashCommandOption(
          name = arg.name,
          description = arg.description,
          `type` = 3, // STRING type
          required = Some(arg.required),
          options = None
        )
      })
    )
  }

  /** Parse raw arguments from a text command (e.g., "!play url here") */
  def parseTextArgs(argsString: String): Map[String, String] = {
    arguments match {
      case Nil        => Map.empty
      case arg :: Nil =>
        // Single argument - entire string is the value
        Map(arg.name -> argsString.trim)
      case _ =>
        // Multiple arguments - for now, just put everything in the first arg
        // Commands with complex parsing can override this
        Option
          .when(argsString.trim.nonEmpty)(
            Map(arguments.head.name -> argsString.trim)
          )
          .getOrElse(Map.empty)
    }
  }
}
