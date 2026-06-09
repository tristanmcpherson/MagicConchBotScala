package dev.raegous.magicconch.audio.internals

import java.io.ByteArrayOutputStream
import java.nio.{ByteBuffer, ByteOrder}
import javax.crypto.Cipher
import javax.crypto.Mac
import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}
import org.bouncycastle.crypto.engines.AESEngine
import org.bouncycastle.crypto.modes.GCMBlockCipher
import org.bouncycastle.crypto.params.{AEADParameters, KeyParameter}

/** DAVE protocol wire helpers.
  *
  * MLS message parsing and state handling live in MlsMessages, MlsPrimitives,
  * DaveProtocol, and DaveSessionManager. This file implements the
  * Discord-specific voice gateway binary messages and media-frame transform
  * that sit around MLS.
  *
  * Source: https://github.com/discord/dave-protocol/blob/main/protocol.md
  */
object DaveSupport {
  val MaxSupportedProtocolVersion = 1

  val OpPrepareTransition = 21
  val OpExecuteTransition = 22
  val OpReadyForTransition = 23
  val OpPrepareEpoch = 24
  val OpMlsExternalSenderPackage = 25
  val OpMlsKeyPackage = 26
  val OpMlsProposals = 27
  val OpMlsCommitWelcome = 28
  val OpMlsAnnounceCommitTransition = 29
  val OpMlsWelcome = 30
  val OpMlsInvalidCommitWelcome = 31

  val MagicMarker: Short = 0xfafa.toShort
  private val MagicHi = 0xfa.toByte
  private val MagicLo = 0xfa.toByte
  private val GcmTagBytes = 16
  private val TruncatedTagBytes = 8
  private val GcmTagBits = 128
  private val TruncatedGcmTagBits = 64
  private val AesKeyBytes = 16

  sealed trait DaveGatewayBinaryMessage {
    def opcode: Int
  }

  final case class ExternalSender(
      signatureKey: Array[Byte],
      credential: Credential
  )
  final case class Credential(credentialType: Int, identity: Array[Byte])

  final case class MlsExternalSenderPackage(
      sequenceNumber: Int,
      externalSender: ExternalSender
  ) extends DaveGatewayBinaryMessage {
    val opcode: Int = OpMlsExternalSenderPackage
  }

  final case class MlsKeyPackage(keyPackageMessage: Array[Byte])
      extends DaveGatewayBinaryMessage {
    val opcode: Int = OpMlsKeyPackage
  }

  enum ProposalsOperationType(val value: Int) {
    case Append extends ProposalsOperationType(0)
    case Revoke extends ProposalsOperationType(1)
  }

  object ProposalsOperationType {
    def fromByte(value: Int): Either[String, ProposalsOperationType] =
      value match {
        case 0     => Right(Append)
        case 1     => Right(Revoke)
        case other => Left(s"Unknown DAVE proposals operation type: $other")
      }
  }

  final case class MlsProposals(
      sequenceNumber: Int,
      operationType: ProposalsOperationType,
      payload: Array[Byte]
  ) extends DaveGatewayBinaryMessage {
    val opcode: Int = OpMlsProposals
  }

  final case class MlsCommitWelcome(
      commitMessage: Array[Byte],
      welcomeMessage: Option[Array[Byte]]
  ) extends DaveGatewayBinaryMessage {
    val opcode: Int = OpMlsCommitWelcome
  }

  final case class MlsAnnounceCommitTransition(
      sequenceNumber: Int,
      transitionId: Int,
      commitMessage: Array[Byte]
  ) extends DaveGatewayBinaryMessage {
    val opcode: Int = OpMlsAnnounceCommitTransition
  }

  final case class MlsWelcome(
      sequenceNumber: Int,
      transitionId: Int,
      welcomeMessage: Array[Byte]
  ) extends DaveGatewayBinaryMessage {
    val opcode: Int = OpMlsWelcome
  }

  final case class UnencryptedRange(offset: Int, length: Int) {
    require(offset >= 0, "offset must be non-negative")
    require(length >= 0, "length must be non-negative")
    def endExclusive: Int = offset + length
  }

  final case class ProtocolFrameFooter(
      authTag: Array[Byte],
      truncatedNonce: Long,
      unencryptedRanges: List[UnencryptedRange],
      supplementalSize: Int
  )

  final case class ParsedProtocolFrame(
      interleavedFrame: Array[Byte],
      footer: ProtocolFrameFooter
  )

  final case class EncryptedAudioFrameDiagnostics(
      inputLength: Int,
      outputLength: Int,
      truncatedNonce: Long,
      generation: Int,
      supplementalSize: Int,
      markerPresent: Boolean,
      tagLength: Int
  )

  def senderKeyGeneration(truncatedNonce: Long): Int =
    ((truncatedNonce >>> 24) & 0xff).toInt

  trait SenderKeyRatchet {
    def keyFor(truncatedNonce: Long): Array[Byte]
  }

  final class HkdfSenderKeyRatchet(baseSecret: Array[Byte])
      extends SenderKeyRatchet {
    private var currentGeneration = 0
    private var currentSecret = baseSecret.clone()
    private val cachedKeys =
      scala.collection.mutable.Map.empty[Int, Array[Byte]]

    def keyFor(truncatedNonce: Long): Array[Byte] = synchronized {
      val generation = ((truncatedNonce >>> 24) & 0xff).toInt

      while (currentGeneration <= generation) {
        val secretForGeneration = currentSecret
        val generationContext = ByteBuffer
          .allocate(4)
          .order(ByteOrder.BIG_ENDIAN)
          .putInt(currentGeneration)
          .array()
        cachedKeys.update(
          currentGeneration,
          MlsPrimitives.expandWithLabel(
            secretForGeneration,
            "key",
            generationContext,
            AesKeyBytes
          )
        )
        currentSecret = MlsPrimitives.expandWithLabel(
          secretForGeneration,
          "secret",
          generationContext,
          32
        )
        currentGeneration += 1
      }

      cachedKeys
        .getOrElse(
          generation,
          throw new IllegalArgumentException(
            s"DAVE sender key generation $generation has been erased or was not derived"
          )
        )
        .clone()
    }
  }

  trait DaveMlsBackend {
    def reset(): Unit
    def keyPackageMessage(): Array[Byte]
    def setExternalSender(externalSender: ExternalSender): Unit
    def processProposals(
        payload: Array[Byte],
        recognizedUserIds: Set[String]
    ): Option[MlsCommitWelcome]
    def processCommit(commitMessage: Array[Byte]): Boolean
    def processWelcome(
        welcomeMessage: Array[Byte],
        recognizedUserIds: Set[String]
    ): Boolean
    def senderKeyRatchet(userId: String): SenderKeyRatchet
  }

  final class UnsupportedMlsBackend extends DaveMlsBackend {
    private def fail[A]: A =
      throw new UnsupportedOperationException(
        "DAVE MLS support requires an RFC 9420 MLS backend"
      )

    def reset(): Unit = ()
    def keyPackageMessage(): Array[Byte] = fail
    def setExternalSender(externalSender: ExternalSender): Unit = fail
    def processProposals(
        payload: Array[Byte],
        recognizedUserIds: Set[String]
    ): Option[MlsCommitWelcome] = fail
    def processCommit(commitMessage: Array[Byte]): Boolean = fail
    def processWelcome(
        welcomeMessage: Array[Byte],
        recognizedUserIds: Set[String]
    ): Boolean = fail
    def senderKeyRatchet(userId: String): SenderKeyRatchet = fail
  }

  object GatewayBinaryCodec {
    def encode(message: DaveGatewayBinaryMessage): Array[Byte] =
      message match {
        case MlsExternalSenderPackage(sequenceNumber, externalSender) =>
          val out = new ByteArrayOutputStream()
          writeUInt16(out, sequenceNumber)
          out.write(OpMlsExternalSenderPackage)
          writeOpaqueVar(out, externalSender.signatureKey)
          writeUInt16(out, externalSender.credential.credentialType)
          writeOpaqueVar(out, externalSender.credential.identity)
          out.toByteArray

        case MlsKeyPackage(keyPackageMessage) =>
          Array(OpMlsKeyPackage.toByte) ++ keyPackageMessage

        case MlsProposals(sequenceNumber, operationType, payload) =>
          val out = new ByteArrayOutputStream()
          writeUInt16(out, sequenceNumber)
          out.write(OpMlsProposals)
          out.write(operationType.value)
          out.write(payload)
          out.toByteArray

        case MlsCommitWelcome(commitMessage, welcomeMessage) =>
          Array(OpMlsCommitWelcome.toByte) ++ commitMessage ++ welcomeMessage
            .getOrElse(Array.emptyByteArray)

        case MlsAnnounceCommitTransition(
              sequenceNumber,
              transitionId,
              commitMessage
            ) =>
          val out = new ByteArrayOutputStream()
          writeUInt16(out, sequenceNumber)
          out.write(OpMlsAnnounceCommitTransition)
          writeUInt16(out, transitionId)
          out.write(commitMessage)
          out.toByteArray

        case MlsWelcome(sequenceNumber, transitionId, welcomeMessage) =>
          val out = new ByteArrayOutputStream()
          writeUInt16(out, sequenceNumber)
          out.write(OpMlsWelcome)
          writeUInt16(out, transitionId)
          out.write(welcomeMessage)
          out.toByteArray
      }

    def decode(bytes: Array[Byte]): Either[String, DaveGatewayBinaryMessage] =
      if (bytes.isEmpty) Left("Empty DAVE binary message")
      else {
        val first = bytes(0) & 0xff
        first match {
          case OpMlsKeyPackage =>
            Right(MlsKeyPackage(bytes.drop(1)))

          case OpMlsCommitWelcome =>
            Right(MlsCommitWelcome(bytes.drop(1), None))

          case _ if bytes.length >= 3 =>
            val sequenceNumber = readUInt16(bytes, 0)
            val opcode = bytes(2) & 0xff
            decodeSequenced(opcode, sequenceNumber, bytes.drop(3))

          case _ =>
            Left(s"DAVE binary message too short: ${bytes.length} bytes")
        }
      }

    private def decodeSequenced(
        opcode: Int,
        sequenceNumber: Int,
        payload: Array[Byte]
    ): Either[String, DaveGatewayBinaryMessage] =
      opcode match {
        case OpMlsExternalSenderPackage =>
          for {
            sigRead <- readOpaqueVar(payload, 0)
            (signatureKey, credentialOffset) = sigRead
            _ <- Either.cond(
              payload.length >= credentialOffset + 2,
              (),
              "External sender missing credential type"
            )
            credentialType = readUInt16(payload, credentialOffset)
            identityRead <- readOpaqueVar(payload, credentialOffset + 2)
            (identity, _) = identityRead
          } yield MlsExternalSenderPackage(
            sequenceNumber,
            ExternalSender(signatureKey, Credential(credentialType, identity))
          )

        case OpMlsProposals =>
          for {
            _ <- Either.cond(
              payload.nonEmpty,
              (),
              "DAVE proposals message missing operation type"
            )
            operation <- ProposalsOperationType.fromByte(payload(0) & 0xff)
          } yield MlsProposals(sequenceNumber, operation, payload.drop(1))

        case OpMlsAnnounceCommitTransition =>
          if (payload.length < 2)
            Left("DAVE announce commit transition missing transition ID")
          else
            Right(
              MlsAnnounceCommitTransition(
                sequenceNumber,
                readUInt16(payload, 0),
                payload.drop(2)
              )
            )

        case OpMlsWelcome =>
          if (payload.length < 2) Left("DAVE welcome missing transition ID")
          else
            Right(
              MlsWelcome(
                sequenceNumber,
                readUInt16(payload, 0),
                payload.drop(2)
              )
            )

        case other =>
          Left(s"Unsupported DAVE binary opcode: $other")
      }
  }

  object MediaFrameCodec {
    def isProtocolFrame(frame: Array[Byte]): Boolean =
      parse(frame).isRight

    def parse(frame: Array[Byte]): Either[String, ParsedProtocolFrame] = {
      if (frame.length < minSupplementalSize)
        Left("Frame is too short for DAVE supplemental data")
      else if (
        frame(frame.length - 2) != MagicHi || frame(frame.length - 1) != MagicLo
      ) Left("DAVE magic marker missing")
      else {
        val supplementalSize = frame(frame.length - 3) & 0xff
        val supplementalStart = frame.length - supplementalSize
        if (supplementalSize < minSupplementalSize)
          Left(s"DAVE supplemental size too small: $supplementalSize")
        else if (
          supplementalStart <= 0 || supplementalStart >= frame.length - 3
        ) Left(s"Invalid DAVE supplemental size: $supplementalSize")
        else {
          val supplementalPayloadEnd = frame.length - 3
          val interleavedFrame = frame.take(supplementalStart)
          val tag = frame.slice(
            supplementalStart,
            supplementalStart + TruncatedTagBytes
          )
          readUleb128(
            frame,
            supplementalStart + TruncatedTagBytes,
            supplementalPayloadEnd
          ).flatMap { case (nonce, rangesOffset) =>
            readRanges(
              frame,
              rangesOffset,
              supplementalPayloadEnd,
              interleavedFrame.length
            ).map { ranges =>
              ParsedProtocolFrame(
                interleavedFrame,
                ProtocolFrameFooter(tag, nonce, ranges, supplementalSize)
              )
            }
          }
        }
      }
    }

    def encryptAudioFrame(
        frame: Array[Byte],
        truncatedNonce: Long,
        ratchet: SenderKeyRatchet
    ): Array[Byte] =
      encryptFrame(frame, truncatedNonce, Nil, ratchet)

    def diagnostics(
        inputLength: Int,
        encryptedFrame: Array[Byte]
    ): Either[String, EncryptedAudioFrameDiagnostics] =
      parse(encryptedFrame).map { parsed =>
        EncryptedAudioFrameDiagnostics(
          inputLength = inputLength,
          outputLength = encryptedFrame.length,
          truncatedNonce = parsed.footer.truncatedNonce,
          generation = senderKeyGeneration(parsed.footer.truncatedNonce),
          supplementalSize = parsed.footer.supplementalSize,
          markerPresent =
            encryptedFrame.takeRight(2).sameElements(Array(MagicHi, MagicLo)),
          tagLength = parsed.footer.authTag.length
        )
      }

    def encryptFrame(
        frame: Array[Byte],
        truncatedNonce: Long,
        unencryptedRanges: List[UnencryptedRange],
        ratchet: SenderKeyRatchet
    ): Array[Byte] = {
      validateRanges(unencryptedRanges, frame.length).fold(
        error => throw new IllegalArgumentException(error),
        _ => ()
      )

      val encryptedRangePlaintext = encryptedBytes(frame, unencryptedRanges)
      val additionalData = associatedData(frame, unencryptedRanges)
      val cipherOutput = aesGcmEncrypt(
        ratchet.keyFor(truncatedNonce),
        truncatedNonce,
        encryptedRangePlaintext,
        additionalData
      )
      val ciphertext = cipherOutput.dropRight(GcmTagBytes)
      val tag = cipherOutput.takeRight(GcmTagBytes).take(TruncatedTagBytes)
      val interleaved = interleave(frame, ciphertext, unencryptedRanges)
      val nonceBytes = writeUleb128(truncatedNonce)
      val rangeBytes = writeRanges(unencryptedRanges)
      val supplementalSize =
        tag.length + nonceBytes.length + rangeBytes.length + 1 + 2

      require(
        supplementalSize <= 255,
        s"DAVE supplemental data too large: $supplementalSize"
      )

      interleaved ++ tag ++ nonceBytes ++ rangeBytes ++ Array(
        supplementalSize.toByte,
        MagicHi,
        MagicLo
      )
    }

    def decryptFrame(
        frame: Array[Byte],
        ratchet: SenderKeyRatchet
    ): Either[String, Array[Byte]] =
      parse(frame).flatMap { parsed =>
        val aad = associatedData(
          parsed.interleavedFrame,
          parsed.footer.unencryptedRanges
        )
        val encryptedPart = encryptedBytes(
          parsed.interleavedFrame,
          parsed.footer.unencryptedRanges
        )
        val tagPadded =
          parsed.footer.authTag ++ Array.fill[Byte](TruncatedTagBytes)(0)
        try {
          val plaintext = aesGcmDecrypt(
            ratchet.keyFor(parsed.footer.truncatedNonce),
            parsed.footer.truncatedNonce,
            encryptedPart ++ tagPadded.take(TruncatedTagBytes),
            aad
          )
          Right(
            interleave(
              parsed.interleavedFrame,
              plaintext,
              parsed.footer.unencryptedRanges
            )
          )
        } catch {
          case e: Exception =>
            Left(s"DAVE frame decrypt failed: ${e.getMessage}")
        }
      }

    private def minSupplementalSize: Int = TruncatedTagBytes + 1 + 1 + 2

    private def aesGcmEncrypt(
        key: Array[Byte],
        truncatedNonce: Long,
        plaintext: Array[Byte],
        aad: Array[Byte]
    ): Array[Byte] = {
      require(
        key.length == AesKeyBytes,
        s"DAVE AES-GCM media key must be $AesKeyBytes bytes"
      )
      val cipher = Cipher.getInstance("AES/GCM/NoPadding")
      cipher.init(
        Cipher.ENCRYPT_MODE,
        new SecretKeySpec(key, "AES"),
        new GCMParameterSpec(GcmTagBits, fullNonce(truncatedNonce))
      )
      cipher.updateAAD(aad)
      cipher.doFinal(plaintext)
    }

    private def aesGcmDecrypt(
        key: Array[Byte],
        truncatedNonce: Long,
        ciphertextAndTag: Array[Byte],
        aad: Array[Byte]
    ): Array[Byte] = {
      require(
        key.length == AesKeyBytes,
        s"DAVE AES-GCM media key must be $AesKeyBytes bytes"
      )
      val cipher = GCMBlockCipher.newInstance(AESEngine.newInstance())
      cipher.init(
        false,
        new AEADParameters(
          new KeyParameter(key),
          TruncatedGcmTagBits,
          fullNonce(truncatedNonce),
          aad
        )
      )
      val output =
        new Array[Byte](cipher.getOutputSize(ciphertextAndTag.length))
      val processed = cipher.processBytes(
        ciphertextAndTag,
        0,
        ciphertextAndTag.length,
        output,
        0
      )
      val finalized = cipher.doFinal(output, processed)
      output.take(processed + finalized)
    }
  }

  def fullNonce(truncatedNonce: Long): Array[Byte] = {
    require(
      truncatedNonce >= 0 && truncatedNonce <= 0xffffffffL,
      s"DAVE truncated nonce out of uint32 range: $truncatedNonce"
    )
    val nonce = new Array[Byte](12)
    // DAVE expands the truncated nonce by memcpy'ing the uint32 into the last
    // four bytes. Discord clients run little-endian, so the wire value must be
    // copied least-significant byte first here.
    nonce(8) = (truncatedNonce & 0xff).toByte
    nonce(9) = ((truncatedNonce >> 8) & 0xff).toByte
    nonce(10) = ((truncatedNonce >> 16) & 0xff).toByte
    nonce(11) = ((truncatedNonce >> 24) & 0xff).toByte
    nonce
  }

  def writeUleb128(value: Long): Array[Byte] = {
    require(value >= 0, "ULEB128 value must be non-negative")
    val out = new ByteArrayOutputStream()
    var remaining = value
    while (remaining >= 0x80L) {
      out.write(((remaining & 0x7fL) | 0x80L).toInt)
      remaining = remaining >>> 7
    }
    out.write((remaining & 0x7fL).toInt)
    out.toByteArray
  }

  def readUleb128(
      bytes: Array[Byte],
      offset: Int,
      limit: Int
  ): Either[String, (Long, Int)] = {
    var result = 0L
    var shift = 0
    var i = offset

    while (i < limit && shift <= 63) {
      val b = bytes(i) & 0xff
      result |= ((b & 0x7fL).toLong << shift)
      i += 1
      if ((b & 0x80) == 0) return Right((result, i))
      shift += 7
    }

    Left("Invalid or unterminated ULEB128 value")
  }

  private def writeRanges(ranges: List[UnencryptedRange]): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    ranges.foreach { range =>
      out.write(writeUleb128(range.offset))
      out.write(writeUleb128(range.length))
    }
    out.toByteArray
  }

  private def readRanges(
      bytes: Array[Byte],
      offset: Int,
      limit: Int,
      frameLength: Int
  ): Either[String, List[UnencryptedRange]] = {
    var i = offset
    var ranges = List.empty[UnencryptedRange]

    while (i < limit) {
      val parsed = for {
        offsetRead <- readUleb128(bytes, i, limit)
        (rangeOffset, nextOffset) = offsetRead
        lengthRead <- readUleb128(bytes, nextOffset, limit)
        (rangeLength, nextLength) = lengthRead
      } yield {
        i = nextLength
        ranges =
          ranges :+ UnencryptedRange(rangeOffset.toInt, rangeLength.toInt)
      }

      parsed match {
        case Left(error) => return Left(error)
        case Right(_)    => ()
      }
    }

    validateRanges(ranges, frameLength).map(_ => ranges)
  }

  private def validateRanges(
      ranges: List[UnencryptedRange],
      frameLength: Int
  ): Either[String, Unit] = {
    var previousEnd = 0
    ranges.foldLeft[Either[String, Unit]](Right(())) {
      case (Left(error), _)  => Left(error)
      case (Right(_), range) =>
        if (range.offset < previousEnd)
          Left("DAVE unencrypted ranges overlap or are unordered")
        else if (range.endExclusive > frameLength)
          Left("DAVE unencrypted range exceeds frame length")
        else {
          previousEnd = range.endExclusive
          Right(())
        }
    }
  }

  private def encryptedBytes(
      frame: Array[Byte],
      unencryptedRanges: List[UnencryptedRange]
  ): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    var cursor = 0
    unencryptedRanges.foreach { range =>
      if (cursor < range.offset) out.write(frame, cursor, range.offset - cursor)
      cursor = range.endExclusive
    }
    if (cursor < frame.length) out.write(frame, cursor, frame.length - cursor)
    out.toByteArray
  }

  private def associatedData(
      frame: Array[Byte],
      unencryptedRanges: List[UnencryptedRange]
  ): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    unencryptedRanges.foreach(range =>
      out.write(frame, range.offset, range.length)
    )
    out.toByteArray
  }

  private def interleave(
      template: Array[Byte],
      encryptedOrPlaintext: Array[Byte],
      unencryptedRanges: List[UnencryptedRange]
  ): Array[Byte] = {
    val out = template.clone()
    var cursor = 0
    var sourceCursor = 0
    unencryptedRanges.foreach { range =>
      val encryptedLength = range.offset - cursor
      if (encryptedLength > 0) {
        System.arraycopy(
          encryptedOrPlaintext,
          sourceCursor,
          out,
          cursor,
          encryptedLength
        )
        sourceCursor += encryptedLength
      }
      cursor = range.endExclusive
    }
    if (cursor < out.length) {
      System.arraycopy(
        encryptedOrPlaintext,
        sourceCursor,
        out,
        cursor,
        out.length - cursor
      )
    }
    out
  }

  private def writeOpaqueVar(
      out: ByteArrayOutputStream,
      bytes: Array[Byte]
  ): Unit = {
    out.write(writeMlsVarint(bytes.length))
    out.write(bytes)
  }

  private def readOpaqueVar(
      bytes: Array[Byte],
      offset: Int
  ): Either[String, (Array[Byte], Int)] =
    readMlsVarint(bytes, offset).flatMap { case (length, dataOffset) =>
      Either.cond(
        bytes.length >= dataOffset + length,
        (bytes.slice(dataOffset, dataOffset + length), dataOffset + length),
        s"Opaque vector length $length exceeds remaining DAVE payload"
      )
    }

  private def writeMlsVarint(value: Int): Array[Byte] = {
    require(value >= 0, "MLS varint value must be non-negative")
    if (value < 0x40) Array(value.toByte)
    else if (value < 0x4000) Array(((value >> 8) | 0x40).toByte, value.toByte)
    else if (value < 0x40000000)
      Array(
        ((value >> 24) | 0x80).toByte,
        (value >> 16).toByte,
        (value >> 8).toByte,
        value.toByte
      )
    else throw new IllegalArgumentException(s"MLS varint too large: $value")
  }

  private def readMlsVarint(
      bytes: Array[Byte],
      offset: Int
  ): Either[String, (Int, Int)] = {
    if (offset >= bytes.length) Left("Missing MLS varint length")
    else {
      val first = bytes(offset) & 0xff
      (first >> 6) match {
        case 0 =>
          Right((first & 0x3f, offset + 1))
        case 1 =>
          if (bytes.length < offset + 2) Left("Truncated 2-byte MLS varint")
          else
            Right(
              (((first & 0x3f) << 8) | (bytes(offset + 1) & 0xff), offset + 2)
            )
        case 2 =>
          if (bytes.length < offset + 4) Left("Truncated 4-byte MLS varint")
          else {
            val value =
              ((first & 0x3f) << 24) |
                ((bytes(offset + 1) & 0xff) << 16) |
                ((bytes(offset + 2) & 0xff) << 8) |
                (bytes(offset + 3) & 0xff)
            Right((value, offset + 4))
          }
        case _ =>
          Left("Unsupported 8-byte MLS varint length")
      }
    }
  }

  private def readUInt16(bytes: Array[Byte], offset: Int): Int =
    ByteBuffer
      .wrap(bytes, offset, 2)
      .order(ByteOrder.BIG_ENDIAN)
      .getShort() & 0xffff

  private def writeUInt16(out: ByteArrayOutputStream, value: Int): Unit = {
    require(value >= 0 && value <= 0xffff, s"uint16 out of range: $value")
    out.write((value >> 8) & 0xff)
    out.write(value & 0xff)
  }

  private def expandWithLabel(
      secret: Array[Byte],
      label: String,
      context: Array[Byte],
      length: Int
  ): Array[Byte] = {
    val fullLabel = s"MLS 1.0 $label".getBytes("UTF-8")
    val info =
      uint16Bytes(length) ++ opaque8(fullLabel) ++ opaqueVarForHkdf(context)
    hkdfExpand(secret, info, length)
  }

  private def hkdfExpand(
      secret: Array[Byte],
      info: Array[Byte],
      length: Int
  ): Array[Byte] = {
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(new SecretKeySpec(secret, "HmacSHA256"))

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

  private def opaque8(data: Array[Byte]): Array[Byte] = {
    require(data.length <= 255, s"opaque8 too large: ${data.length}")
    Array(data.length.toByte) ++ data
  }

  private def opaqueVarForHkdf(data: Array[Byte]): Array[Byte] =
    mlsVarintForHkdf(data.length) ++ data

  private def mlsVarintForHkdf(value: Int): Array[Byte] = {
    require(value >= 0, s"MLS varint value must be non-negative: $value")
    if (value < 0x40) Array(value.toByte)
    else if (value < 0x4000) Array(((value >> 8) | 0x40).toByte, value.toByte)
    else if (value < 0x40000000)
      Array(
        ((value >> 24) | 0x80).toByte,
        (value >> 16).toByte,
        (value >> 8).toByte,
        value.toByte
      )
    else throw new IllegalArgumentException(s"MLS varint too large: $value")
  }

  private def uint16Bytes(v: Int): Array[Byte] =
    Array(((v >> 8) & 0xff).toByte, (v & 0xff).toByte)

  private def uint32Bytes(v: Int): Array[Byte] =
    Array(
      ((v >> 24) & 0xff).toByte,
      ((v >> 16) & 0xff).toByte,
      ((v >> 8) & 0xff).toByte,
      (v & 0xff).toByte
    )
}
