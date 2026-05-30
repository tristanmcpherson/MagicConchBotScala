package dev.raegous.magicconch.audio.internals

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import munit.FunSuite
import dev.raegous.magicconch.audio.internals.DaveSupport.*

class DaveSupportTest extends FunSuite {
  private val staticRatchet = new SenderKeyRatchet {
    override def keyFor(truncatedNonce: Long): Array[Byte] =
      Array.tabulate[Byte](16)(i => (i + 1).toByte)
  }

  test("ULEB128 should round-trip protocol nonce values") {
    val values = List(0L, 1L, 127L, 128L, 255L, 16384L, 0xFFFFFFFFL)

    values.foreach { value =>
      val encoded = writeUleb128(value)
      assertEquals(readUleb128(encoded, 0, encoded.length), Right((value, encoded.length)))
    }
  }

  test("GatewayBinaryCodec should round-trip external sender packages") {
    val message = MlsExternalSenderPackage(
      sequenceNumber = 513,
      externalSender = ExternalSender(
        signatureKey = Array[Byte](1, 2, 3, 4),
        credential = Credential(credentialType = 1, identity = Array[Byte](5, 6, 7))
      )
    )

    GatewayBinaryCodec.decode(GatewayBinaryCodec.encode(message)) match {
      case Right(decoded: MlsExternalSenderPackage) =>
        assertEquals(decoded.sequenceNumber, message.sequenceNumber)
        assert(decoded.externalSender.signatureKey.sameElements(message.externalSender.signatureKey))
        assertEquals(decoded.externalSender.credential.credentialType, 1)
        assert(decoded.externalSender.credential.identity.sameElements(message.externalSender.credential.identity))
      case other =>
        fail(s"Unexpected decode result: $other")
    }
  }

  test("GatewayBinaryCodec should decode sequenced transition messages") {
    val message = MlsAnnounceCommitTransition(
      sequenceNumber = 7,
      transitionId = 42,
      commitMessage = Array[Byte](9, 8, 7)
    )

    assertEquals(
      GatewayBinaryCodec.decode(GatewayBinaryCodec.encode(message)).map {
        case decoded: MlsAnnounceCommitTransition =>
          (decoded.sequenceNumber, decoded.transitionId, decoded.commitMessage.toList)
        case other =>
          fail(s"Unexpected message type: $other")
      },
      Right((7, 42, List[Byte](9, 8, 7)))
    )
  }

  test("DaveProtocol should build binary MLS key package messages") {
    val payload = (for {
      state <- DaveProtocol.generateKeyState[IO]("1090123456789012345")
      keyPackage <- DaveProtocol.buildKeyPackageMessage[IO](state, "1090123456789012345")
    } yield GatewayBinaryCodec.encode(MlsKeyPackage(keyPackage))).unsafeRunSync()

    assertEquals(payload(0) & 0xFF, OpMlsKeyPackage)
    assertEquals(payload(1) & 0xFF, 0x00)
    assertEquals(payload(2) & 0xFF, 0x01)
    assertEquals(payload(3) & 0xFF, 0x00)
    assertEquals(payload(4) & 0xFF, 0x02)
    assert(MlsMessages.parseKeyPackageMessage(payload.drop(1)).isRight)
    assert(MlsMessages.parseMlsMessage(payload.drop(1)).toOption.forall(_.wireFormat != MlsMessages.WireFormatKeyPackage))
    assert(payload.length > 200)
  }

  test("MediaFrameCodec should add and detect DAVE protocol frame footer") {
    val frame = "opus-frame".getBytes("UTF-8")
    val encrypted = MediaFrameCodec.encryptAudioFrame(frame, truncatedNonce = 3, staticRatchet)

    assert(MediaFrameCodec.isProtocolFrame(encrypted))
    assertEquals(encrypted.takeRight(2).toList, List(0xFA.toByte, 0xFA.toByte))
    assert(encrypted.length > frame.length)
  }

  test("MediaFrameCodec should preserve frame footer metadata across nonce encodings") {
    val frame = Array.tabulate[Byte](321)(i => ((i * 31) & 0xFF).toByte)
    val nonces = List(0L, 1L, 127L, 128L)

    nonces.foreach { nonce =>
      val encrypted = MediaFrameCodec.encryptAudioFrame(frame, nonce, staticRatchet)

      assert(MediaFrameCodec.isProtocolFrame(encrypted))
      assertEquals(MediaFrameCodec.parse(encrypted).map(_.footer.truncatedNonce), Right(nonce))
      assertEquals(MediaFrameCodec.parse(encrypted).map(_.interleavedFrame.length), Right(frame.length))
    }
  }

  test("DAVE full nonce should encode truncated nonce in little-endian tail bytes") {
    val nonce = fullNonce(0x01020304L)

    assertEquals(nonce.take(8).toList, List.fill(8)(0.toByte))
    assertEquals(nonce.drop(8).toList, List(0x04.toByte, 0x03.toByte, 0x02.toByte, 0x01.toByte))
  }

  test("MediaFrameCodec should encode the DAVE frame nonce in the supplemental footer") {
    val frame = Array[Byte](0x78, 0x56, 0x34, 0x12, 0x7F)
    val encrypted = MediaFrameCodec.encryptAudioFrame(frame, truncatedNonce = 1, staticRatchet)

    assertEquals(MediaFrameCodec.parse(encrypted).map(_.footer.truncatedNonce), Right(1L))
  }

  test("MediaFrameCodec should decrypt DAVE media frames and expose footer diagnostics") {
    val frame = Array.tabulate[Byte](96)(i => ((i * 17) & 0xFF).toByte)
    val ratchet = new HkdfSenderKeyRatchet(Array.fill[Byte](32)(0x23.toByte))
    val encrypted = MediaFrameCodec.encryptAudioFrame(frame, truncatedNonce = 0x01020304L, ratchet)

    assertEquals(MediaFrameCodec.decryptFrame(encrypted, ratchet).map(_.toList), Right(frame.toList))
    assertEquals(MediaFrameCodec.diagnostics(frame.length, encrypted).map(_.truncatedNonce), Right(0x01020304L))
    assertEquals(MediaFrameCodec.diagnostics(frame.length, encrypted).map(_.generation), Right(1))
  }

  test("HkdfSenderKeyRatchet should derive stable generation keys from nonce MSB") {
    val baseSecret = Array.fill[Byte](32)(0x11.toByte)
    val ratchet = new HkdfSenderKeyRatchet(baseSecret)

    val generation0 = ratchet.keyFor(0x00000001L)
    val generation0Again = ratchet.keyFor(0x0000FFFFL)
    val generation1 = ratchet.keyFor(0x01000000L)
    val expectedGeneration0 = MlsPrimitives.expandWithLabel(baseSecret, "key", Array[Byte](0, 0, 0, 0), 16)

    assertEquals(generation0.length, 16)
    assert(generation0.sameElements(expectedGeneration0))
    assert(generation0.sameElements(generation0Again))
    assert(!generation0.sameElements(generation1))
  }

  test("MediaFrameCodec should reject non-DAVE frames") {
    val frame = "plain-opus-frame".getBytes("UTF-8")

    assert(!MediaFrameCodec.isProtocolFrame(frame))
  }
}
