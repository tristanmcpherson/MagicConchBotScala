package dev.raegous.magicconch.audio.internals

import munit.FunSuite

class MlsPrimitivesTest extends FunSuite {
  test("MLS varint should round trip supported lengths") {
    val values = List(0, 1, 63, 64, 255, 16383, 16384, 0x3FFFFFFF)

    values.foreach { value =>
      val encoded = MlsPrimitives.varint(value)
      val reader = MlsPrimitives.Reader(encoded)

      assertEquals(reader.readVarInt(), Right(value))
      assert(reader.atEnd)
    }
  }

  test("MLS opaque vector should round trip") {
    val data = Array.tabulate[Byte](300)(i => (i & 0xFF).toByte)
    val encoded = MlsPrimitives.opaqueVar(data)
    val decoded = MlsPrimitives.Reader(encoded).readOpaqueVar()

    decoded match {
      case Right(value) => assert(value.sameElements(data))
      case Left(error) => fail(error.message)
    }
  }

  test("labeled HKDF should be deterministic and label-separated") {
    val secret = Array.fill[Byte](32)(0x42.toByte)
    val context = Array.fill[Byte](32)(0x11.toByte)

    val a = MlsPrimitives.expandWithLabel(secret, "exporter", context, 32)
    val b = MlsPrimitives.expandWithLabel(secret, "exporter", context, 32)
    val c = MlsPrimitives.expandWithLabel(secret, "different", context, 32)

    assert(a.sameElements(b))
    assert(!a.sameElements(c))
  }

  test("MLS ref hash should hash labeled opaque values") {
    val value = "proposal-bytes".getBytes("UTF-8")
    val expected = MlsPrimitives.hash(
      MlsPrimitives.opaqueVar("MLS 1.0 Proposal Reference".getBytes("UTF-8")) ++
        MlsPrimitives.opaqueVar(value)
    )

    assert(MlsPrimitives.refHash("Proposal Reference", value).sameElements(expected))
  }

  test("DAVE media base secret should use little-endian user id context") {
    val exporterSecret = Array.fill[Byte](32)(0x22.toByte)

    val baseA = MlsPrimitives.daveUserMediaBaseSecret(exporterSecret, "149639766382608384")
    val baseB = MlsPrimitives.daveUserMediaBaseSecret(exporterSecret, "149639766382608384")
    val baseC = MlsPrimitives.daveUserMediaBaseSecret(exporterSecret, "149639766382608385")

    assertEquals(baseA.length, 16)
    assert(baseA.sameElements(baseB))
    assert(!baseA.sameElements(baseC))
  }

  test("DaveProtocol should derive a self sender ratchet from exporter secret") {
    val exporterSecret = Array.fill[Byte](32)(0x33.toByte)
    val ratchet = DaveProtocol.deriveSelfRatchetFromExporterSecret(exporterSecret, "149639766382608384")

    val key0 = ratchet.keyFor(0)
    val key0Again = ratchet.keyFor(1)
    val key1 = ratchet.keyFor(0x01000000L)

    assertEquals(key0.length, 16)
    assert(key0.sameElements(key0Again))
    assert(!key0.sameElements(key1))
  }
}
