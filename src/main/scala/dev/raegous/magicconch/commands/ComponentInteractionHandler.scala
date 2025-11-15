package dev.raegous.magicconch.commands

import cats.effect.*
import dev.raegous.magicconch.discord.Interaction
import sttp.ws.WebSocket

/**
 * Handler for Discord component interactions (buttons, select menus, etc.)
 *
 * Component interactions are triggered when users interact with message components
 * like buttons or select menus. Each handler is responsible for:
 * 1. Matching on custom_id patterns
 * 2. Parsing parameters from the custom_id
 * 3. Processing the interaction
 * 4. Sending appropriate responses
 */
trait ComponentInteractionHandler[F[_]] {

  /**
   * Check if this handler can process the given custom_id
   *
   * @param customId The custom_id from the component interaction
   * @return true if this handler should process this interaction
   */
  def canHandle(customId: String): Boolean

  /**
   * Handle the component interaction
   *
   * @param customId The custom_id from the component interaction
   * @param interaction The full interaction object from Discord
   * @param wsOpt Optional WebSocket connection to the Discord gateway
   * @return Effect that processes the interaction
   */
  def handle(
    customId: String,
    interaction: Interaction,
    wsOpt: Option[WebSocket[F]]
  ): F[Unit]
}
