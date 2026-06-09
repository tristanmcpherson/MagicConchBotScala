package dev.raegous.magicconch.commands

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import dev.raegous.magicconch.discord.Interaction
import sttp.ws.WebSocket

/** Routes component interactions to their appropriate handlers
  *
  * The router maintains a registry of ComponentInteractionHandlers and
  * dispatches incoming interactions to the first handler that can process them.
  */
class InteractionRouter[F[_]: Async](
    private val handlers: List[ComponentInteractionHandler[F]]
)(using Logger[F]) {

  /** Route a component interaction to the appropriate handler
    *
    * @param interaction
    *   The interaction from Discord
    * @param wsOpt
    *   Optional WebSocket connection to the Discord gateway
    * @return
    *   Effect that processes the interaction
    */
  def route(interaction: Interaction, wsOpt: Option[WebSocket[F]]): F[Unit] = {
    interaction.data.flatMap(_.custom_id) match {
      case Some(customId) =>
        handlers.find(_.canHandle(customId)) match {
          case Some(handler) =>
            Logger[F].debug(
              s"[INTERACTION] Routing $customId to ${handler.getClass.getSimpleName}"
            ) >>
              handler.handle(customId, interaction, wsOpt)
          case None =>
            Logger[F].warn(
              s"[INTERACTION] No handler found for custom_id: $customId"
            )
        }
      case None =>
        Logger[F].warn(
          "[INTERACTION] Message component interaction missing custom_id"
        )
    }
  }

  /** Register a new handler (useful for dynamic handler registration)
    */
  def withHandler(
      handler: ComponentInteractionHandler[F]
  ): InteractionRouter[F] = {
    new InteractionRouter[F](handlers :+ handler)
  }
}

object InteractionRouter {

  /** Create an InteractionRouter with the given handlers
    */
  def apply[F[_]: Async: Logger](
      handlers: ComponentInteractionHandler[F]*
  ): InteractionRouter[F] = {
    new InteractionRouter[F](handlers.toList)
  }
}
