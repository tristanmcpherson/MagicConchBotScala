package dev.raegous.magicconch

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import sttp.ws.WebSocket
import DiscordModels.*
import commands.*

class MessageHandler[F[_]: Async](
  discordApi: DiscordApiClient[F],
  commandRegistry: CommandRegistry[F]
)(using Logger[F]) {

  def handleMessage(message: DiscordMessage, ws: WebSocket[F]): F[Unit] = {
    val isVoiceMessage = message.flags.exists(flag => (flag & 8192) != 0)

    val hasAudioAttachment = message.attachments.exists(_.exists(attachment =>
      attachment.content_type.exists(ct =>
        ct.startsWith("audio/") || attachment.duration_secs.isDefined
      )
    ))

    if (isVoiceMessage || hasAudioAttachment) {
      handleAudioMessage(message)
    } else {
      handleTextMessage(message, ws)
    }
  }

  private def handleTextMessage(message: DiscordMessage, ws: WebSocket[F]): F[Unit] = {
    val contentLower = message.content.toLowerCase
    val content = message.content

    // Parse command from message (format: !commandname args)
    if (contentLower.startsWith("!")) {
      val parts = content.drop(1).split(" ", 2)
      val commandName = parts(0).toLowerCase
      val argsString = if (parts.length > 1) parts(1) else ""

      message.guild_id match {
        case Some(guildId) if commandRegistry.hasCommand(commandName) =>
          // Get the command and let it parse its own arguments
          commandRegistry.getCommand(commandName) match {
            case Some(command) =>
              val args = command.parseTextArgs(argsString)

              val context = CommandContext[F](
                guildId = guildId,
                userId = message.author.id,
                channelId = message.channel_id,
                username = message.author.username,
                gatewayWs = Some(ws),
                args = args
              )

              commandRegistry.execute(commandName, context).flatMap { result =>
                discordApi.sendRichMessage(
                  message.channel_id,
                  result.message,
                  result.embeds,
                  result.components
                )
              }
            case None =>
              Async[F].pure(())
          }
        case None =>
          discordApi.sendMessage(message.channel_id, "This command only works in a server!")
        case _ =>
          Async[F].pure(())
      }
    } else {
      Async[F].pure(())
    }
  }

  private def handleAudioMessage(message: DiscordMessage): F[Unit] = {
    val responses = List(
      "I hear you loud and clear!",
      "That sounds interesting...",
      "Audio message received!",
      "I'm listening...",
      "Sound waves detected!",
      "Your voice has been noted."
    )
    val response = responses(scala.util.Random.nextInt(responses.length))
    discordApi.sendMessage(message.channel_id, response)
  }
}
