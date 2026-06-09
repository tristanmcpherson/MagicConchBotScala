package dev.raegous.magicconch.audio.internals

import munit.FunSuite
import com.goterl.lazysodium.{LazySodiumJava, SodiumJava}

import javax.crypto.Cipher
import javax.crypto.spec.{GCMParameterSpec, SecretKeySpec}

class VoiceProtocolTest extends FunSuite {
  import VoiceProtocol.*

  test("RTPHeader should encode the expected header bytes") {
    val header = RTPHeader(
      sequence = 0x1234,
      timestamp = 0x01020304L,
      ssrc = 0x11223344L
    )

    assertEquals(
      RTPHeader.toBytes(header).toList,
      List[Byte](
        0x80.toByte,
        0x78.toByte,
        0x12.toByte,
        0x34.toByte,
        0x01.toByte,
        0x02.toByte,
        0x03.toByte,
        0x04.toByte,
        0x11.toByte,
        0x22.toByte,
        0x33.toByte,
        0x44.toByte
      )
    )
  }

  test("Nonce should encode counters in big-endian byte order") {
    val counter = 0x01020304L

    assertEquals(Nonce.counterBytes(counter).toList, List[Byte](1, 2, 3, 4))
    assertEquals(
      Nonce.forAES256GCM(counter).take(4).toList,
      List[Byte](1, 2, 3, 4)
    )
    assertEquals(
      Nonce.forXChaCha20(counter).take(4).toList,
      List[Byte](1, 2, 3, 4)
    )
  }

  test(
    "RtpTransportState.initial should start sequence and timestamp at zero with nonce 1"
  ) {
    assertEquals(
      RtpTransportState.initial,
      RtpTransportState(sequence = 0, timestamp = 0L, nonceCounter = 1L)
    )
  }

  test("RTPPacket should append the nonce suffix after payload bytes") {
    val packet = RTPPacket(
      header = RTPHeader(sequence = 1, timestamp = 960L, ssrc = 0x11223344L),
      encryptedPayload = Array[Byte](0x55.toByte, 0x66.toByte),
      nonceCounter = 0x01020304L
    )

    val bytes = packet.toBytes

    assertEquals(bytes.take(12).toList, RTPHeader.toBytes(packet.header).toList)
    assertEquals(
      bytes.slice(12, 14).toList,
      List[Byte](0x55.toByte, 0x66.toByte)
    )
    assertEquals(bytes.takeRight(4).toList, List[Byte](1, 2, 3, 4))
  }

  test(
    "RTPPacket AES-GCM payload should decrypt with header AAD and appended nonce"
  ) {
    val header = RTPHeader(sequence = 7, timestamp = 6720L, ssrc = 0x11223344L)
    val headerBytes = RTPHeader.toBytes(header)
    val key = Array.tabulate[Byte](32)(i => (i + 1).toByte)
    val nonceCounter = 1L
    val plaintext = Array.tabulate[Byte](64)(i => ((i * 7) & 0xff).toByte)

    val encryptCipher = Cipher.getInstance("AES/GCM/NoPadding")
    encryptCipher.init(
      Cipher.ENCRYPT_MODE,
      new SecretKeySpec(key, "AES"),
      new GCMParameterSpec(128, Nonce.forAES256GCM(nonceCounter))
    )
    encryptCipher.updateAAD(headerBytes)
    val encryptedPayload = encryptCipher.doFinal(plaintext)
    val packetBytes = RTPPacket(header, encryptedPayload, nonceCounter).toBytes
    val appendedNonce = packetBytes
      .takeRight(4)
      .foldLeft(0L)((acc, byte) => (acc << 8) | (byte & 0xff).toLong)
    val encryptedFromPacket =
      packetBytes.slice(headerBytes.length, packetBytes.length - 4)

    val decryptCipher = Cipher.getInstance("AES/GCM/NoPadding")
    decryptCipher.init(
      Cipher.DECRYPT_MODE,
      new SecretKeySpec(key, "AES"),
      new GCMParameterSpec(128, Nonce.forAES256GCM(appendedNonce))
    )
    decryptCipher.updateAAD(packetBytes.take(headerBytes.length))

    assertEquals(appendedNonce, nonceCounter)
    assertEquals(
      decryptCipher.doFinal(encryptedFromPacket).toList,
      plaintext.toList
    )
  }

  test(
    "RTPPacket XChaCha20-Poly1305 payload should decrypt with header AAD and appended nonce"
  ) {
    val header = RTPHeader(sequence = 7, timestamp = 6720L, ssrc = 0x11223344L)
    val headerBytes = RTPHeader.toBytes(header)
    val key = Array.tabulate[Byte](32)(i => (i + 1).toByte)
    val nonceCounter = 1L
    val plaintext = Array.tabulate[Byte](64)(i => ((i * 7) & 0xff).toByte)
    val lazySodium = new LazySodiumJava(new SodiumJava())
    val encryptedPayload = new Array[Byte](plaintext.length + 16)
    val encryptedLength = Array(0L)

    val encrypted = lazySodium.cryptoAeadXChaCha20Poly1305IetfEncrypt(
      encryptedPayload,
      encryptedLength,
      plaintext,
      plaintext.length.toLong,
      headerBytes,
      headerBytes.length.toLong,
      null,
      Nonce.forXChaCha20(nonceCounter),
      key
    )
    val packetBytes = RTPPacket(header, encryptedPayload, nonceCounter).toBytes
    val appendedNonce = packetBytes
      .takeRight(4)
      .foldLeft(0L)((acc, byte) => (acc << 8) | (byte & 0xff).toLong)
    val encryptedFromPacket =
      packetBytes.slice(headerBytes.length, packetBytes.length - 4)
    val decryptedPayload = new Array[Byte](plaintext.length)
    val decryptedLength = Array(0L)
    val decrypted = lazySodium.cryptoAeadXChaCha20Poly1305IetfDecrypt(
      decryptedPayload,
      decryptedLength,
      null,
      encryptedFromPacket,
      encryptedFromPacket.length.toLong,
      packetBytes.take(headerBytes.length),
      headerBytes.length.toLong,
      Nonce.forXChaCha20(appendedNonce),
      key
    )

    assert(encrypted)
    assertEquals(encryptedLength(0), encryptedPayload.length.toLong)
    assertEquals(appendedNonce, nonceCounter)
    assert(decrypted)
    assertEquals(decryptedLength(0), plaintext.length.toLong)
    assertEquals(decryptedPayload.toList, plaintext.toList)
  }

  test(
    "RtpTransportState should progress sequence timestamp and nonce monotonically"
  ) {
    val current =
      RtpTransportState(sequence = 41, timestamp = 1920L, nonceCounter = 9L)

    assertEquals(
      current.next(),
      RtpTransportState(sequence = 42, timestamp = 2880L, nonceCounter = 10L)
    )
  }

  test(
    "RtpTransportState should wrap sequence timestamp and nonce at protocol widths"
  ) {
    val current = RtpTransportState(
      sequence = 0xffff,
      timestamp = 0xffffffffL,
      nonceCounter = 0xffffffffL
    )

    assertEquals(
      current.next(),
      RtpTransportState(sequence = 0, timestamp = 959L, nonceCounter = 0L)
    )
  }
}
