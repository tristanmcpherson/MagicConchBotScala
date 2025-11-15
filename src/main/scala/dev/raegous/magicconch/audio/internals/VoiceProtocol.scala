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
    /**
     * Create a 24-byte nonce for XChaCha20-Poly1305 (rtpsize mode).
     * Format: 4-byte counter (big-endian) + 20 zero bytes
     */
    def forXChaCha20(counter: Int): Array[Byte] = {
      val nonce = new Array[Byte](24)
      nonce(0) = (counter >> 24).toByte
      nonce(1) = (counter >> 16).toByte
      nonce(2) = (counter >> 8).toByte
      nonce(3) = counter.toByte
      // Remaining 20 bytes are already zero
      nonce
    }

    /**
     * Create a 12-byte nonce for AES256-GCM (rtpsize mode).
     * Format: 4-byte counter (big-endian) + 8 zero bytes
     */
    def forAES256GCM(counter: Int): Array[Byte] = {
      val nonce = new Array[Byte](12)
      nonce(0) = (counter >> 24).toByte
      nonce(1) = (counter >> 16).toByte
      nonce(2) = (counter >> 8).toByte
      nonce(3) = counter.toByte
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
    nonceCounter: Int                // 4-byte nonce counter (big-endian)
  ) {
    /**
     * Encode to bytes ready for UDP transmission.
     */
    def toBytes: Array[Byte] = {
      val headerBytes = RTPHeader.toBytes(header)
      val nonceBytes = new Array[Byte](4)
      nonceBytes(0) = (nonceCounter >> 24).toByte
      nonceBytes(1) = (nonceCounter >> 16).toByte
      nonceBytes(2) = (nonceCounter >> 8).toByte
      nonceBytes(3) = nonceCounter.toByte

      val result = new Array[Byte](headerBytes.length + encryptedPayload.length + 4)
      System.arraycopy(headerBytes, 0, result, 0, headerBytes.length)
      System.arraycopy(encryptedPayload, 0, result, headerBytes.length, encryptedPayload.length)
      System.arraycopy(nonceBytes, 0, result, headerBytes.length + encryptedPayload.length, 4)
      result
    }
  }
}
