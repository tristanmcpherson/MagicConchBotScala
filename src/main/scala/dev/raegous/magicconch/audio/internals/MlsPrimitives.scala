package dev.raegous.magicconch.audio.internals

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

object MlsPrimitives {
  val MlsVersion10 = 0x0001
  val CipherSuiteP256Aes128GcmSha256P256 = 0x0002
  val HashLength = 32
  val Aes128KeyLength = 16

  final case class DecodeError(message: String)

  final class Reader private (bytes: Array[Byte], private var offset: Int) {
    def remaining: Int = bytes.length - offset
    def position: Int = offset
    def atEnd: Boolean = offset >= bytes.length
    def slice(start: Int, end: Int): Array[Byte] =
      bytes.slice(start, end)

    def readUInt8(): Either[DecodeError, Int] =
      readBytes(1).map(_(0) & 0xFF)

    def readUInt16(): Either[DecodeError, Int] =
      readBytes(2).map(b => ((b(0) & 0xFF) << 8) | (b(1) & 0xFF))

    def readUInt32(): Either[DecodeError, Long] =
      readBytes(4).map { b =>
        ((b(0) & 0xFFL) << 24) |
          ((b(1) & 0xFFL) << 16) |
          ((b(2) & 0xFFL) << 8) |
          (b(3) & 0xFFL)
      }

    def readUInt64(): Either[DecodeError, Long] =
      readBytes(8).map(ByteBuffer.wrap(_).getLong)

    def readVarInt(): Either[DecodeError, Int] =
      if (remaining < 1) Left(DecodeError("missing MLS varint"))
      else {
        val first = bytes(offset) & 0xFF
        (first >>> 6) match {
          case 0 =>
            offset += 1
            Right(first & 0x3F)
          case 1 =>
            readBytes(2).map(b => ((b(0) & 0x3F) << 8) | (b(1) & 0xFF))
          case 2 =>
            readBytes(4).map { b =>
              ((b(0) & 0x3F) << 24) |
                ((b(1) & 0xFF) << 16) |
                ((b(2) & 0xFF) << 8) |
                (b(3) & 0xFF)
            }
          case _ =>
            Left(DecodeError("8-byte MLS varints are unsupported"))
        }
      }

    def readOpaqueVar(): Either[DecodeError, Array[Byte]] =
      readVarInt().flatMap(readBytes)

    def readVectorVar[A](decodeOne: Reader => Either[DecodeError, A]): Either[DecodeError, List[A]] =
      readOpaqueVar().flatMap { vectorBytes =>
        val reader = Reader(vectorBytes)
        val out = scala.collection.mutable.ListBuffer.empty[A]
        var failure = Option.empty[DecodeError]
        while (!reader.atEnd && failure.isEmpty) {
          decodeOne(reader) match {
            case Left(error) => failure = Some(error)
            case Right(value) => out += value
          }
        }
        failure.fold[Either[DecodeError, List[A]]](Right(out.toList))(Left(_))
      }

    def readBytes(length: Int): Either[DecodeError, Array[Byte]] =
      if (length < 0) Left(DecodeError(s"negative read length: $length"))
      else if (remaining < length) Left(DecodeError(s"truncated MLS structure: need $length bytes, have $remaining"))
      else {
        val out = bytes.slice(offset, offset + length)
        offset += length
        Right(out)
      }
  }

  object Reader {
    def apply(bytes: Array[Byte]): Reader =
      new Reader(bytes, 0)
  }

  def uint8(value: Int): Array[Byte] = {
    require(value >= 0 && value <= 0xFF, s"uint8 out of range: $value")
    Array(value.toByte)
  }

  def uint16(value: Int): Array[Byte] = {
    require(value >= 0 && value <= 0xFFFF, s"uint16 out of range: $value")
    Array(((value >>> 8) & 0xFF).toByte, (value & 0xFF).toByte)
  }

  def uint32(value: Long): Array[Byte] = {
    require(value >= 0 && value <= 0xFFFFFFFFL, s"uint32 out of range: $value")
    Array(
      ((value >>> 24) & 0xFF).toByte,
      ((value >>> 16) & 0xFF).toByte,
      ((value >>> 8) & 0xFF).toByte,
      (value & 0xFF).toByte
    )
  }

  def uint64(value: Long): Array[Byte] =
    ByteBuffer.allocate(8).putLong(value).array()

  def varint(value: Int): Array[Byte] = {
    require(value >= 0, s"MLS varint must be non-negative: $value")
    if (value < 0x40) Array(value.toByte)
    else if (value < 0x4000) Array(((value >>> 8) | 0x40).toByte, value.toByte)
    else if (value < 0x40000000) Array(
      ((value >>> 24) | 0x80).toByte,
      (value >>> 16).toByte,
      (value >>> 8).toByte,
      value.toByte
    )
    else throw new IllegalArgumentException(s"MLS varint too large: $value")
  }

  def opaqueVar(bytes: Array[Byte]): Array[Byte] =
    varint(bytes.length) ++ bytes

  def vectorVar(elements: Iterable[Array[Byte]]): Array[Byte] =
    opaqueVar(elements.foldLeft(Array.emptyByteArray)(_ ++ _))

  def hash(bytes: Array[Byte]): Array[Byte] = {
    val digest = java.security.MessageDigest.getInstance("SHA-256")
    digest.digest(bytes)
  }

  def hkdfExtract(salt: Array[Byte], ikm: Array[Byte]): Array[Byte] = {
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(new SecretKeySpec(if (salt.isEmpty) Array.fill[Byte](HashLength)(0) else salt, "HmacSHA256"))
    mac.doFinal(ikm)
  }

  def hkdfExpand(prk: Array[Byte], info: Array[Byte], length: Int): Array[Byte] = {
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(new SecretKeySpec(prk, "HmacSHA256"))

    val out = new ByteArrayOutputStream()
    var previous = Array.emptyByteArray
    var counter = 1

    while (out.size() < length) {
      mac.reset()
      mac.update(previous)
      mac.update(info)
      mac.update(counter.toByte)
      previous = mac.doFinal()
      out.write(previous)
      counter += 1
    }

    out.toByteArray.take(length)
  }

  def expandWithLabel(secret: Array[Byte], label: String, context: Array[Byte], length: Int): Array[Byte] = {
    val fullLabel = s"MLS 1.0 $label".getBytes("UTF-8")
    val hkdfLabel = uint16(length) ++ opaque8(fullLabel) ++ opaqueVar(context)
    hkdfExpand(secret, hkdfLabel, length)
  }

  def deriveSecret(secret: Array[Byte], label: String): Array[Byte] =
    expandWithLabel(secret, label, hash(Array.emptyByteArray), HashLength)

  def deriveSecret(secret: Array[Byte], label: String, transcriptHash: Array[Byte]): Array[Byte] =
    expandWithLabel(secret, label, transcriptHash, HashLength)

  def refHash(label: String, value: Array[Byte]): Array[Byte] =
    hash(opaqueVar(s"MLS 1.0 $label".getBytes("UTF-8")) ++ opaqueVar(value))

  def exporterSecret(exporterMasterSecret: Array[Byte], label: String, context: Array[Byte], length: Int): Array[Byte] =
    expandWithLabel(
      deriveSecret(exporterMasterSecret, label),
      "exporter",
      hash(context),
      length
    )

  def daveUserMediaBaseSecret(exporterSecretValue: Array[Byte], userId: String): Array[Byte] = {
    val userIdLe = littleEndianUInt64(java.lang.Long.parseUnsignedLong(userId))
    exporterSecret(exporterSecretValue, "Discord Secure Frames v0", userIdLe, Aes128KeyLength)
  }

  def daveSenderRatchet(exporterSecretValue: Array[Byte], userId: String): DaveSupport.SenderKeyRatchet =
    new DaveSupport.HkdfSenderKeyRatchet(daveUserMediaBaseSecret(exporterSecretValue, userId))

  private def littleEndianUInt64(value: Long): Array[Byte] = {
    val out = new Array[Byte](8)
    var i = 0
    while (i < 8) {
      out(i) = ((value >>> (8 * i)) & 0xFF).toByte
      i += 1
    }
    out
  }

  private def opaque8(bytes: Array[Byte]): Array[Byte] = {
    require(bytes.length <= 0xFF, s"opaque8 too large: ${bytes.length}")
    Array(bytes.length.toByte) ++ bytes
  }
}
