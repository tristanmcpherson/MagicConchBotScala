package dev.raegous.magicconch.commands

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger

class MagicConchCommand[F[_]: Async](using Logger[F]) extends Command[F] {
  val name = "magicconch"
  val description = "Ask the Magic Conch Shell a question"
  val arguments = List(
    CommandArgument("question", "Your question for the Magic Conch", required = false)
  )

  private val responses = List(
    "Maybe someday.",
    "Nothing.",
    "Neither.",
    "I don't think so.",
    "No.",
    "Yes.",
    "Try asking again."
  )

  def execute(context: CommandContext[F]): F[CommandResult] = {
    val response = responses(scala.util.Random.nextInt(responses.length))
    Async[F].pure(CommandResult(s"🐚 The Magic Conch says: **$response**"))
  }
}
