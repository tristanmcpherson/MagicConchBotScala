package dev.raegous.magicconch.guilds

import cats.effect.*
import cats.implicits.*
import org.typelevel.log4cats.Logger

/**
 * Settings for a specific guild.
 *
 * @param volume Volume multiplier (0.0 = mute, 1.0 = 100%, 2.0 = 200%)
 */
case class GuildSettings(
  volume: Double = 0.1
  // Future settings can be added here:
  // eq: EQSettings = EQSettings.default,
  // audioQuality: AudioQuality = AudioQuality.High,
  // preferredLanguage: String = "en"
)

/**
 * Manages per-guild settings (volume, EQ, audio quality, etc.)
 *
 * This manager centralizes all guild-specific configuration, separating
 * concerns from VoiceManager (which handles playback) and other components.
 */
class GuildSettingsManager[F[_]: Async] private (
  private val settingsRef: Ref[F, Map[String, GuildSettings]]
)(using Logger[F]) {

  /**
   * Get all settings for a guild (returns defaults if not set)
   */
  def getSettings(guildId: String): F[GuildSettings] =
    settingsRef.get.map(_.getOrElse(guildId, GuildSettings()))

  /**
   * Update settings for a guild using an update function
   */
  def updateSettings(guildId: String, update: GuildSettings => GuildSettings): F[GuildSettings] =
    settingsRef.modify { settings =>
      val current = settings.getOrElse(guildId, GuildSettings())
      val updated = update(current)
      (settings + (guildId -> updated), updated)
    }

  /**
   * Get current volume for a guild
   */
  def getVolume(guildId: String): F[Double] =
    getSettings(guildId).map(_.volume)

  /**
   * Set volume for a guild (automatically clamped to 0.0-2.0)
   */
  def setVolume(guildId: String, volume: Double): F[Unit] = {
    val clampedVolume = volume.max(0.0).min(2.0)
    updateSettings(guildId, _.copy(volume = clampedVolume)).flatMap { _ =>
      Logger[F].info(s"[SETTINGS] Set volume for guild $guildId to ${(clampedVolume * 100).toInt}%")
    }
  }

  /**
   * Reset all settings for a guild to defaults
   */
  def resetSettings(guildId: String): F[Unit] =
    settingsRef.update(_ - guildId) >>
    Logger[F].info(s"[SETTINGS] Reset settings for guild $guildId")
}

object GuildSettingsManager {
  /**
   * Create a new GuildSettingsManager
   */
  def make[F[_]: Async](using Logger[F]): Resource[F, GuildSettingsManager[F]] =
    Resource.eval(
      Ref.of[F, Map[String, GuildSettings]](Map.empty)
        .map(new GuildSettingsManager[F](_))
    )
}
