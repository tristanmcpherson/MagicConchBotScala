package dev.raegous.magicconch.audio.internals

import scodec.*
import scodec.bits.*
import scodec.codecs.*

/**
 * Discord Voice Protocol models using scodec for type-safe binary encoding.
 *
 * This provides a composable, declarative approach to RTP packet encoding,
 * replacing manual ByteBuffer manipulation with type-safe codecs.
 */
object VoiceProtocol {

  private[internals] final case class RtpTransportState(
    sequence: Int,
    timestamp: Long,
    nonceCounter: Long
  ) {
    def next(frameSize: Int = 960): RtpTransportState =
      RtpTransportState(
        sequence = (sequence + 1) & 0xFFFF,
        timestamp = (timestamp + frameSize.toLong) & 0xFFFFFFFFL,
        nonceCounter = (nonceCounter + 1L) & 0xFFFFFFFFL
      )
  }

  private[internals] object RtpTransportState {
    val initial: RtpTransportState = RtpTransportState(
      sequence = 0,
      timestamp = 0L,
      nonceCounter = 1L
    )
  }

  /**
   * RTP Header for Discord voice packets.
   * Always 12 bytes, used as AAD (Additional Authenticated Data) in AEAD encryption.
   *
   * Structure (big-endian):
   * - Byte 0: V(2) P(1) X(1) CC(4)
   * - Byte 1: M(1) PT(7)
   * - Bytes 2-3: Sequence number (16-bit)
   * - Bytes 4-7: Timestamp (32-bit)
   * - Bytes 8-11: SSRC (32-bit)
   */
  case class RTPHeader(
    version: Int = 2,           // RTP version (always 2)
    padding: Boolean = false,   // Padding flag
    extension: Boolean = false, // Extension flag
    csrcCount: Int = 0,         // Contributing source count
    marker: Boolean = false,    // Marker bit
    payloadType: Int = 0x78,    // Payload type (120 for Opus)
    sequence: Int,              // Sequence number (wraps at 65536)
    timestamp: Long,            // RTP timestamp (increases by frame size)
    ssrc: Long                  // Synchronization source identifier
  )

  object RTPHeader {
    /**
     * Scodec codec for RTP header (12 bytes, big-endian).
     */
    val codec: Codec[RTPHeader] = (
      ("version" | uint(2)) ::
      ("padding" | bool) ::
      ("extension" | bool) ::
      ("csrcCount" | uint(4)) ::
      ("marker" | bool) ::
      ("payloadType" | uint(7)) ::
      ("sequence" | uint16) ::
      ("timestamp" | uint32) ::
      ("ssrc" | uint32)
    ).as[RTPHeader]

    /**
     * Encode RTP header to bytes.
     * Returns Array[Byte] for easy interop with encryption APIs.
     */
    def toBytes(header: RTPHeader): Array[Byte] =
      codec.encode(header).require.bytes.toArray
  }

  /**
   * Nonce generation for Discord voice encryption.
   */
  object Nonce {
    def counterBytes(counter: Long): Array[Byte] =
      Array[Byte](
        ((counter >> 24) & 0xFF).toByte,
        ((counter >> 16) & 0xFF).toByte,
        ((counter >> 8) & 0xFF).toByte,
        (counter & 0xFF).toByte
      )

    /**
     * Create a 24-byte nonce for XChaCha20-Poly1305 (rtpsize mode).
     * Format: 4-byte counter (big-endian) + 20 zero bytes
     */
    def forXChaCha20(counter: Long): Array[Byte] = {
      val nonce = new Array[Byte](24)
      val counterBytes = this.counterBytes(counter)
      System.arraycopy(counterBytes, 0, nonce, 0, counterBytes.length)
      // Remaining 20 bytes are already zero
      nonce
    }

    /**
     * Create a 12-byte nonce for AES256-GCM (rtpsize mode).
     * Format: 4-byte counter (big-endian) + 8 zero bytes
     */
    def forAES256GCM(counter: Long): Array[Byte] = {
      val nonce = new Array[Byte](12)
      val counterBytes = this.counterBytes(counter)
      System.arraycopy(counterBytes, 0, nonce, 0, counterBytes.length)
      // Remaining 8 bytes are already zero
      nonce
    }
  }

  /**
   * Complete RTP packet ready for UDP transmission.
   * Format: [Header (12B)] + [Encrypted Payload] + [Nonce Counter (4B)]
   */
  case class RTPPacket(
    header: RTPHeader,
    encryptedPayload: Array[Byte],  // Opus data encrypted with 16-byte auth tag
    nonceCounter: Long               // 4-byte nonce counter (big-endian)
  ) {
    /**
     * Encode to bytes ready for UDP transmission.
     */
    def toBytes: Array[Byte] = {
      val headerBytes = RTPHeader.toBytes(header)
      val nonceBytes = Nonce.counterBytes(nonceCounter)

      val result = new Array[Byte](headerBytes.length + encryptedPayload.length + 4)
      System.arraycopy(headerBytes, 0, result, 0, headerBytes.length)
      System.arraycopy(encryptedPayload, 0, result, headerBytes.length, encryptedPayload.length)
      System.arraycopy(nonceBytes, 0, result, headerBytes.length + encryptedPayload.length, 4)
      result
    }
  }
}
