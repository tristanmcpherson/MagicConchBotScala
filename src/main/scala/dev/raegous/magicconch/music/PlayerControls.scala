package dev.raegous.magicconch.music

import dev.raegous.magicconch.discord.DiscordModels.*
import dev.raegous.magicconch.discord.*

/** Centralized player control components for music playback
  *
  * Creates Discord message components (buttons) for controlling music playback.
  * This ensures consistent player controls across all commands and
  * interactions.
  */
object PlayerControls {

  /** Creates player control components for the current queue state
    *
    * Returns None if no track is currently playing. Returns action rows with
    * playback control buttons if a track is playing.
    *
    * Controls include:
    *   - Pause/Resume button (dynamic based on pause state)
    *   - Stop button
    *   - Skip button (disabled if queue is empty)
    *   - Volume Up button (+10%)
    *   - Volume Down button (-10%)
    */
  def createPlayerControls(
      guildId: String,
      queue: MusicQueue
  ): Option[List[MessageComponent]] = {
    queue.currentTrack.map { _ =>
      val hasQueue = queue.tracks.nonEmpty

      // Row 1: Playback controls
      val pauseResumeButton = MessageComponent(
        `type` = 2, // Button
        style = Some(
          if (queue.isPaused) 3 else 1
        ), // Success (green) if paused, Primary (blue) if playing
        label = Some(if (queue.isPaused) "▶️ Resume" else "⏸️ Pause"),
        custom_id =
          Some(s"player_${if (queue.isPaused) "resume" else "pause"}_$guildId")
      )

      val stopButton = MessageComponent(
        `type` = 2,
        style = Some(4), // Danger (red)
        label = Some("⏹️ Stop"),
        custom_id = Some(s"player_stop_$guildId")
      )

      val skipButton = MessageComponent(
        `type` = 2,
        style = Some(2), // Secondary (gray)
        label = Some("⏭️ Skip"),
        custom_id = Some(s"player_skip_$guildId"),
        disabled = Some(!hasQueue)
      )

      val playbackRow = MessageComponent(
        `type` = 1, // Action Row
        components = Some(List(pauseResumeButton, stopButton, skipButton))
      )

      // Row 2: Volume controls
      val volumeDownButton = MessageComponent(
        `type` = 2, // Button
        style = Some(2), // Secondary
        label = Some("🔉 -10%"),
        custom_id = Some(s"volume_${guildId}_dec10")
      )

      val volumeUpButton = MessageComponent(
        `type` = 2, // Button
        style = Some(2), // Secondary
        label = Some("🔊 +10%"),
        custom_id = Some(s"volume_${guildId}_inc10")
      )

      val volumeRow = MessageComponent(
        `type` = 1, // Action Row
        components = Some(List(volumeDownButton, volumeUpButton))
      )

      List(playbackRow, volumeRow)
    }
  }
}
