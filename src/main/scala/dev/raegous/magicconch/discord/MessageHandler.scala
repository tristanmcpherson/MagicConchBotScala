package dev.raegous.magicconch

import cats.effect.*
import cats.implicits.*
import cats.data.OptionT
import org.typelevel.log4cats.Logger
import sttp.ws.WebSocket
import dev.raegous.magicconch.discord.*
import commands.*

class MessageHandler[F[_]: Async](
    discordApi: DiscordApiClient[F],
    commandRegistry: CommandRegistry[F]
)(using Logger[F]) {

  def handleMessage(message: DiscordMessage, ws: WebSocket[F]): F[Unit] = {
    val isVoiceMessage = message.flags.exists(flag => (flag & 8192) != 0)

    val hasAudioAttachment = message.attachments.exists(
      _.exists(attachment =>
        attachment.content_type.exists(ct =>
          ct.startsWith("audio/") || attachment.duration_secs.nonEmpty
        )
      )
    )

    Async[F].whenA(!isVoiceMessage && !hasAudioAttachment)(
      handleTextMessage(message, ws)
    )
  }

  private def handleTextMessage(
      message: DiscordMessage,
      ws: WebSocket[F]
  ): F[Unit] = {
    val contentLower = message.content.toLowerCase
    val content = message.content

    Async[F].whenA(contentLower.startsWith("!")) {
      val parts = content.drop(1).split(" ", 2)
      val commandName = parts(0).toLowerCase
      val argsString = parts.lift(1).getOrElse("")

      val result = for {
        guildId <- OptionT.fromOption[F](message.guild_id)
        _ <- OptionT.fromOption[F](
          Option.when(commandRegistry.hasCommand(commandName))(())
        )
        command <- OptionT.fromOption[F](
          commandRegistry.getCommand(commandName)
        )
      } yield {
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
      }

      result.value.flatMap(
        _.fold(
          message.guild_id.fold(
            discordApi.sendMessage(
              message.channel_id,
              "This command only works in a server!"
            )
          )(_ => Async[F].unit)
        )(identity)
      )
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
