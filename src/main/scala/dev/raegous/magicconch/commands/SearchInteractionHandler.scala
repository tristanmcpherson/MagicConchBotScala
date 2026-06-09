package dev.raegous.magicconch.commands

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger
import dev.raegous.magicconch.*
import dev.raegous.magicconch.audio.VoiceManager
import dev.raegous.magicconch.discord.*
import dev.raegous.magicconch.discord.DiscordModels.given
import dev.raegous.magicconch.errors.*
import dev.raegous.magicconch.music.{
  PlayerControls,
  TrackExtractor,
  YouTubeSearchResult
}
import io.circe.Printer
import io.circe.syntax.*
import sttp.ws.WebSocket

class SearchInteractionHandler[F[_]: Async](
    voiceManager: VoiceManager[F],
    trackExtractor: TrackExtractor[F],
    discordApi: DiscordApiClient[F],
    applicationId: String
)(using Logger[F])
    extends ComponentInteractionHandler[F] {

  override def canHandle(customId: String): Boolean = {
    customId.startsWith("search_select_")
  }

  override def handle(
      customId: String,
      interaction: Interaction,
      wsOpt: Option[WebSocket[F]]
  ): F[Unit] = {
    parseCustomId(customId, 4).fold(
      Logger[F].error(s"[SEARCH] Invalid custom_id format: $customId")
    ) { parts =>
      val userId = parts(2)
      val index = parts(3).toIntOption.getOrElse(0)

      voiceManager.getSearchResults(userId).flatMap { resultsOpt =>
        resultsOpt
          .flatMap(results =>
            Option.when(index >= 0 && index < results.length)(results(index))
          )
          .traverse(selected =>
            processSelectedTrack(selected, userId, interaction, wsOpt)
          )
          .flatMap {
            case Some(_) => Async[F].unit
            case None    => sendExpiredSearchResponse(interaction)
          }
      }
    }
  }

  private def processSelectedTrack(
      selected: YouTubeSearchResult,
      userId: String,
      interaction: Interaction,
      wsOpt: Option[WebSocket[F]]
  ): F[Unit] = {
    val guildId = interaction.guild_id.getOrElse("")
    val username = getUsername(interaction)

    for {
      _ <- Logger[F].info(s"[SEARCH] User selected: ${selected.title}")
      _ <- sendDeferredResponse(interaction)
      trackOpt <- trackExtractor.extractTrackInfo(selected.url)
      queueBefore <- voiceManager.getQueue(guildId)
      wasEmpty = queueBefore.tracks.isEmpty && queueBefore.currentTrack.isEmpty
      _ <- trackOpt
        .traverse { track =>
          addTrackAndAutoPlay(
            track.copy(requestedBy = username),
            guildId,
            userId,
            wsOpt
          )
        }
        .flatMap {
          case Some(_) =>
            for {
              _ <- voiceManager.clearSearchResults(userId)
              queueAfter <- voiceManager.getQueue(guildId)
              components = PlayerControls.createPlayerControls(
                guildId,
                queueAfter
              )
              message = Option
                .when(wasEmpty)(
                  s"Now playing: **${trackOpt.map(_.title).getOrElse("")}**"
                )
                .getOrElse(
                  s"Added to queue: **${trackOpt.map(_.title).getOrElse("")}**"
                )
              _ <- discordApi.editRichInteractionResponse(
                applicationId,
                interaction.token,
                message,
                None,
                components
              )
            } yield ()
          case None =>
            discordApi.editInteractionResponse(
              applicationId,
              interaction.token,
              "Failed to extract audio from the selected video"
            )
        }
    } yield ()
  }

  private def addTrackAndAutoPlay(
      track: MusicTrack,
      guildId: String,
      userId: String,
      wsOpt: Option[WebSocket[F]]
  ): F[Unit] = {
    for {
      queueBefore <- voiceManager.getQueue(guildId)
      addResult <- voiceManager.addToQueue(guildId, track)
      _ <- addResult.fold(
        error =>
          Logger[F].warn(s"[SEARCH] Failed to add track: ${error.message}"),
        _ =>
          Option
            .when(
              queueBefore.tracks.isEmpty && queueBefore.currentTrack.isEmpty
            )(())
            .traverse(_ => autoJoinAndPlay(guildId, userId, wsOpt))
            .void
      )
    } yield ()
  }

  private def autoJoinAndPlay(
      guildId: String,
      userId: String,
      wsOpt: Option[WebSocket[F]]
  ): F[Unit] = {
    wsOpt
      .flatTraverse { ws =>
        voiceManager.getUserVoiceChannel(userId).flatMap {
          _.traverse { channelId =>
            voiceManager.joinVoiceChannel(guildId, channelId, ws) >>
              voiceManager.waitForVoiceConnection(guildId) >>
              voiceManager.playNextTrack(guildId)
          }
        }
      }
      .flatTap {
        case None =>
          Logger[F].warn(s"[SEARCH] User not in voice channel or no WebSocket")
        case _ => Async[F].unit
      }
      .void
  }

  private def sendDeferredResponse(interaction: Interaction): F[Unit] = {
    val jsonPrinter = Printer.noSpaces.copy(dropNullValues = true)

    val response = InteractionResponse(
      `type` = 5, // DEFERRED_CHANNEL_MESSAGE_WITH_SOURCE3
      data = None
    )
    discordApi.sendInteractionResponse(
      interaction.id,
      interaction.token,
      response.asJson.printWith(jsonPrinter)
    )
  }

  private def sendExpiredSearchResponse(interaction: Interaction): F[Unit] = {
    val jsonPrinter = Printer.noSpaces.copy(dropNullValues = true)

    val response = InteractionResponse(
      `type` = 4, // CHANNEL_MESSAGE_WITH_SOURCE
      data = Some(
        InteractionResponseData(
          content = Some("Search results expired. Please run `/search` again."),
          flags = Some(64) // Ephemeral
        )
      )
    )
    discordApi.sendInteractionResponse(
      interaction.id,
      interaction.token,
      response.asJson.printWith(jsonPrinter)
    )
  }

  private def getUsername(interaction: Interaction): String = {
    interaction.member
      .flatMap(_.user.map(_.username))
      .orElse(interaction.user.map(_.username))
      .getOrElse("Unknown")
  }

  private def parseCustomId(
      customId: String,
      expectedParts: Int
  ): Option[Array[String]] = {
    val parts = customId.split("_")
    Option.when(parts.length >= expectedParts)(parts)
  }
}
