package dev.raegous.magicconch.guilds

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger

// volume multiplier: 0.0 = mute, 1.0 = 100%, 2.0 = 200%
case class GuildSettings(volume: Double = 0.5)

class GuildSettingsManager[F[_]: Async] private (
  private val settingsRef: Ref[F, Map[String, GuildSettings]]
)(using Logger[F]) {

  def getSettings(guildId: String): F[GuildSettings] =
    settingsRef.get.map(_.getOrElse(guildId, GuildSettings()))

  def updateSettings(guildId: String, update: GuildSettings => GuildSettings): F[GuildSettings] =
    settingsRef.modify { settings =>
      val current = settings.getOrElse(guildId, GuildSettings())
      val updated = update(current)
      (settings + (guildId -> updated), updated)
    }

  def getVolume(guildId: String): F[Double] =
    getSettings(guildId).map(_.volume)

  def setVolume(guildId: String, volume: Double): F[Unit] = {
    val clampedVolume = volume.max(0.0).min(2.0)
    updateSettings(guildId, _.copy(volume = clampedVolume)).flatMap { _ =>
      Logger[F].info(s"[SETTINGS] Set volume for guild $guildId to ${(clampedVolume * 100).toInt}%")
    }
  }

  def resetSettings(guildId: String): F[Unit] =
    settingsRef.update(_ - guildId) >>
    Logger[F].info(s"[SETTINGS] Reset settings for guild $guildId")
}

object GuildSettingsManager {
  def make[F[_]: Async](using Logger[F]): Resource[F, GuildSettingsManager[F]] =
    Resource.eval(
      Ref.of[F, Map[String, GuildSettings]](Map.empty)
        .map(new GuildSettingsManager[F](_))
    )
}
