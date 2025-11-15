package dev.raegous.magicconch.commands

import cats.effect.*
import cats.effect.unsafe.implicits.global
import dev.raegous.magicconch.TestFixtures.{testLogger, *}
import dev.raegous.magicconch.discord.*
import munit.CatsEffectSuite
import sttp.ws.WebSocket

class InteractionRouterTest extends CatsEffectSuite {

  test("router should route to handler that can handle the custom_id") {
    var handlerCalled = false
    val testHandler = new ComponentInteractionHandler[IO] {
      override def canHandle(customId: String): Boolean = customId.startsWith("test_")

      override def handle(
        customId: String,
        interaction: Interaction,
        wsOpt: Option[WebSocket[IO]]
      ): IO[Unit] = {
        IO { handlerCalled = true }
      }
    }

    val router = InteractionRouter[IO](testHandler)
    val interaction = sampleInteraction(customId = Some("test_button_123"))

    router.route(interaction, None).map { _ =>
      assert(handlerCalled, "Handler should have been called")
    }
  }

  test("router should not call handler if it cannot handle the custom_id") {
    var handlerCalled = false
    val testHandler = new ComponentInteractionHandler[IO] {
      override def canHandle(customId: String): Boolean = customId.startsWith("test_")

      override def handle(
        customId: String,
        interaction: Interaction,
        wsOpt: Option[WebSocket[IO]]
      ): IO[Unit] = {
        IO { handlerCalled = true }
      }
    }

    val router = InteractionRouter[IO](testHandler)
    val interaction = sampleInteraction(customId = Some("other_button_123"))

    router.route(interaction, None).map { _ =>
      assert(!handlerCalled, "Handler should not have been called")
    }
  }

  test("router should route to first matching handler") {
    var firstHandlerCalled = false
    var secondHandlerCalled = false

    val firstHandler = new ComponentInteractionHandler[IO] {
      override def canHandle(customId: String): Boolean = customId.startsWith("test_")

      override def handle(
        customId: String,
        interaction: Interaction,
        wsOpt: Option[WebSocket[IO]]
      ): IO[Unit] = {
        IO { firstHandlerCalled = true }
      }
    }

    val secondHandler = new ComponentInteractionHandler[IO] {
      override def canHandle(customId: String): Boolean = customId.startsWith("test_")

      override def handle(
        customId: String,
        interaction: Interaction,
        wsOpt: Option[WebSocket[IO]]
      ): IO[Unit] = {
        IO { secondHandlerCalled = true }
      }
    }

    val router = InteractionRouter[IO](firstHandler, secondHandler)
    val interaction = sampleInteraction(customId = Some("test_button_123"))

    router.route(interaction, None).map { _ =>
      assert(firstHandlerCalled, "First handler should have been called")
      assert(!secondHandlerCalled, "Second handler should not have been called")
    }
  }

  test("router should handle multiple different handlers") {
    var searchHandlerCalled = false
    var playerHandlerCalled = false

    val searchHandler = new ComponentInteractionHandler[IO] {
      override def canHandle(customId: String): Boolean = customId.startsWith("search_")

      override def handle(
        customId: String,
        interaction: Interaction,
        wsOpt: Option[WebSocket[IO]]
      ): IO[Unit] = {
        IO { searchHandlerCalled = true }
      }
    }

    val playerHandler = new ComponentInteractionHandler[IO] {
      override def canHandle(customId: String): Boolean = customId.startsWith("player_")

      override def handle(
        customId: String,
        interaction: Interaction,
        wsOpt: Option[WebSocket[IO]]
      ): IO[Unit] = {
        IO { playerHandlerCalled = true }
      }
    }

    val router = InteractionRouter[IO](searchHandler, playerHandler)

    for {
      _ <- router.route(sampleInteraction(customId = Some("search_select_123_0")), None)
      _ <- IO { assert(searchHandlerCalled, "Search handler should have been called") }
      _ <- IO { assert(!playerHandlerCalled, "Player handler should not have been called") }

      // Reset and test player handler
      _ <- IO { searchHandlerCalled = false }
      _ <- router.route(sampleInteraction(customId = Some("player_pause_guild123")), None)
      _ <- IO { assert(!searchHandlerCalled, "Search handler should not have been called") }
      _ <- IO { assert(playerHandlerCalled, "Player handler should have been called") }
    } yield ()
  }

  test("router should handle interaction with no custom_id gracefully") {
    var handlerCalled = false
    val testHandler = new ComponentInteractionHandler[IO] {
      override def canHandle(customId: String): Boolean = customId.startsWith("test_")

      override def handle(
        customId: String,
        interaction: Interaction,
        wsOpt: Option[WebSocket[IO]]
      ): IO[Unit] = {
        IO { handlerCalled = true }
      }
    }

    val router = InteractionRouter[IO](testHandler)
    val interaction = sampleInteraction(customId = None)

    router.route(interaction, None).map { _ =>
      assert(!handlerCalled, "Handler should not have been called for interaction with no custom_id")
    }
  }

  test("withHandler should add a new handler to the router") {
    var firstHandlerCalled = false
    var secondHandlerCalled = false

    val firstHandler = new ComponentInteractionHandler[IO] {
      override def canHandle(customId: String): Boolean = customId.startsWith("first_")

      override def handle(
        customId: String,
        interaction: Interaction,
        wsOpt: Option[WebSocket[IO]]
      ): IO[Unit] = {
        IO { firstHandlerCalled = true }
      }
    }

    val secondHandler = new ComponentInteractionHandler[IO] {
      override def canHandle(customId: String): Boolean = customId.startsWith("second_")

      override def handle(
        customId: String,
        interaction: Interaction,
        wsOpt: Option[WebSocket[IO]]
      ): IO[Unit] = {
        IO { secondHandlerCalled = true }
      }
    }

    val router = InteractionRouter[IO](firstHandler).withHandler(secondHandler)

    for {
      _ <- router.route(sampleInteraction(customId = Some("first_test")), None)
      _ <- IO { assert(firstHandlerCalled, "First handler should have been called") }

      _ <- router.route(sampleInteraction(customId = Some("second_test")), None)
      _ <- IO { assert(secondHandlerCalled, "Second handler should have been called") }
    } yield ()
  }
}
