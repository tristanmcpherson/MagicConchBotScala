package dev.raegous.magicconch.errors

/** Typed error ADTs for better error handling throughout the application.
  */

/** Base trait for all audio-related errors */
sealed trait AudioError extends Throwable {
  def message: String
  override def getMessage: String = message
}

/** Errors related to voice connections */
sealed trait VoiceError extends AudioError

case class VoiceConnectionFailed(guildId: String, reason: String)
    extends VoiceError {
  override def message: String =
    s"Failed to connect to voice channel in guild $guildId: $reason"
}

case class VoiceConnectionTimeout(guildId: String) extends VoiceError {
  override def message: String =
    s"Voice connection timeout for guild $guildId after 15 seconds"
}

case class VoiceGatewayError(guildId: String, reason: String)
    extends VoiceError {
  override def message: String =
    s"Voice gateway error for guild $guildId: $reason"
}

/** Errors related to audio extraction and streaming */
sealed trait ExtractionError extends AudioError

case class StreamUrlExtractionFailed(url: String, reason: String)
    extends ExtractionError {
  override def message: String =
    s"Failed to extract stream URL from $url: $reason"
}

case class TrackInfoExtractionFailed(url: String, reason: String)
    extends ExtractionError {
  override def message: String =
    s"Failed to extract track info from $url: $reason"
}

case class PlaylistExtractionFailed(url: String, reason: String)
    extends ExtractionError {
  override def message: String =
    s"Failed to extract playlist from $url: $reason"
}

/** Errors related to audio playback */
sealed trait PlaybackError extends AudioError

case class AudioStreamingFailed(trackTitle: String, reason: String)
    extends PlaybackError {
  override def message: String =
    s"Failed to stream audio for track '$trackTitle': $reason"
}

case class NoActiveVoiceConnection(guildId: String) extends PlaybackError {
  override def message: String =
    s"No active voice connection for guild $guildId"
}

/** Errors related to queue management */
sealed trait QueueError extends AudioError

case class QueueFull(maxSize: Int) extends QueueError {
  override def message: String =
    s"Queue is full (maximum size: $maxSize tracks)"
}

case class QueueEmpty(guildId: String) extends QueueError {
  override def message: String = s"Queue is empty for guild $guildId"
}

case class InvalidQueueIndex(index: Int, queueSize: Int) extends QueueError {
  override def message: String =
    s"Invalid queue index $index (queue size: $queueSize)"
}
