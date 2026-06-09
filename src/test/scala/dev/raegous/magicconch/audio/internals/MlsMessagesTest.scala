package dev.raegous.magicconch.audio.internals

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import munit.FunSuite

import java.security.{KeyPair, KeyPairGenerator, Signature}
import java.security.interfaces.ECPublicKey
import java.security.spec.ECGenParameterSpec

class MlsMessagesTest extends FunSuite {
  private val userId = "149639766382608384"

  test("generated DAVE key packages should parse and verify") {
    val keyPackage = (for {
      state <- DaveProtocol.generateKeyState[IO](userId)
      bytes <- DaveProtocol.buildKeyPackageMessage[IO](state, userId)
    } yield MlsMessages.parseKeyPackageMessage(bytes)).unsafeRunSync()

    keyPackage match {
      case Right(value) =>
        assertEquals(value.version, MlsPrimitives.MlsVersion10)
        assertEquals(
          value.cipherSuite,
          MlsPrimitives.CipherSuiteP256Aes128GcmSha256P256
        )
        assertEquals(value.leafNode.userId, Some(userId))
        assert(!value.initKey.sameElements(value.leafNode.encryptionKey))
        assertEquals(value.extensions.toList, Nil)
        assertEquals(
          value.leafNode.capabilities.versions,
          List(MlsPrimitives.MlsVersion10)
        )
        assertEquals(
          value.leafNode.capabilities.cipherSuites,
          List(MlsPrimitives.CipherSuiteP256Aes128GcmSha256P256)
        )
        assertEquals(value.leafNode.capabilities.extensions, Nil)
        assertEquals(value.leafNode.capabilities.proposals, Nil)
        assertEquals(value.leafNode.capabilities.credentials, List(1))
        assertEquals(value.leafNode.source, 1)
        assertEquals(value.leafNode.lifetimeNotBefore, Some(0L))
        assertEquals(value.leafNode.lifetimeNotAfter, Some(-1L))
        assertEquals(value.leafNode.extensions.toList, Nil)
        assert(MlsMessages.verifyLeafNodeSignature(value.leafNode))
        assert(MlsMessages.verifyKeyPackageSignature(value))

      case Left(error) =>
        fail(error.message)
    }
  }

  test("wrapped MLS key package messages should still parse and verify") {
    val keyPackage = (for {
      state <- DaveProtocol.generateKeyState[IO](userId)
      bytes <- DaveProtocol.buildKeyPackageMessage[IO](state, userId)
    } yield MlsMessages.parseKeyPackageMessage(
      MlsPrimitives.uint16(MlsPrimitives.MlsVersion10) ++ MlsPrimitives.uint16(
        MlsMessages.WireFormatKeyPackage
      ) ++ bytes
    )).unsafeRunSync()

    keyPackage match {
      case Right(value) =>
        assertEquals(value.leafNode.userId, Some(userId))
        assert(MlsMessages.verifyLeafNodeSignature(value.leafNode))
        assert(MlsMessages.verifyKeyPackageSignature(value))

      case Left(error) =>
        fail(error.message)
    }
  }

  test(
    "DAVE proposal validation should accept signed external add proposals for recognized users"
  ) {
    val externalKey = generateSigningKey()
    val addUserState = DaveProtocol.generateKeyState[IO](userId).unsafeRunSync()
    val addKeyPackageMessage =
      DaveProtocol.buildKeyPackageMessageSync(addUserState, userId)
    val proposal =
      MlsPrimitives.uint16(MlsMessages.ProposalTypeAdd) ++ addKeyPackageMessage
    val publicProposal = signedExternalProposal(externalKey, proposal)
    val proposalBatch = MlsPrimitives.uint8(0) ++ MlsPrimitives.vectorVar(
      List(MlsPrimitives.opaqueVar(publicProposal))
    )
    val externalSender = DaveSupport.ExternalSender(
      signatureKey = uncompressedPoint(externalKey),
      credential = DaveSupport.Credential(
        credentialType = 1,
        identity = "gateway".getBytes("UTF-8")
      )
    )

    val validated = DaveProtocol.validateProposalBatches(
      externalSender = Some(externalSender),
      proposalBatches = List(proposalBatch),
      recognizedUserIds = Set(userId)
    )

    validated match {
      case Right(
            List(
              DaveValidatedProposal(
                _,
                MlsMessages.AddProposal(keyPackage),
                _,
                rawMessage
              )
            )
          ) =>
        assertEquals(keyPackage.leafNode.userId, Some(userId))
        assert(rawMessage.exists(_.sameElements(publicProposal)))

      case other =>
        fail(s"Unexpected validation result: $other")
    }
  }

  test(
    "DAVE proposal validation should accept raw MLSMessage append proposal payloads"
  ) {
    val externalKey = generateSigningKey()
    val addUserState = DaveProtocol.generateKeyState[IO](userId).unsafeRunSync()
    val addKeyPackageMessage =
      DaveProtocol.buildKeyPackageMessageSync(addUserState, userId)
    val proposal =
      MlsPrimitives.uint16(MlsMessages.ProposalTypeAdd) ++ addKeyPackageMessage
    val publicProposal = signedExternalProposal(externalKey, proposal)
    val proposalBatch = MlsPrimitives.uint8(0) ++ publicProposal
    val externalSender = DaveSupport.ExternalSender(
      signatureKey = uncompressedPoint(externalKey),
      credential = DaveSupport.Credential(
        credentialType = 1,
        identity = "gateway".getBytes("UTF-8")
      )
    )

    val validated = DaveProtocol.validateProposalBatches(
      externalSender = Some(externalSender),
      proposalBatches = List(proposalBatch),
      recognizedUserIds = Set(userId)
    )

    assert(validated.exists(_.size == 1))
  }

  test(
    "DAVE proposal validation should accept opaque single MLSMessage append proposal payloads"
  ) {
    val externalKey = generateSigningKey()
    val addUserState = DaveProtocol.generateKeyState[IO](userId).unsafeRunSync()
    val addKeyPackageMessage =
      DaveProtocol.buildKeyPackageMessageSync(addUserState, userId)
    val proposal =
      MlsPrimitives.uint16(MlsMessages.ProposalTypeAdd) ++ addKeyPackageMessage
    val publicProposal = signedExternalProposal(externalKey, proposal)
    val proposalBatch =
      MlsPrimitives.uint8(0) ++ MlsPrimitives.opaqueVar(publicProposal)
    val externalSender = DaveSupport.ExternalSender(
      signatureKey = uncompressedPoint(externalKey),
      credential = DaveSupport.Credential(
        credentialType = 1,
        identity = "gateway".getBytes("UTF-8")
      )
    )

    val validated = DaveProtocol.validateProposalBatches(
      externalSender = Some(externalSender),
      proposalBatches = List(proposalBatch),
      recognizedUserIds = Set(userId)
    )

    assert(validated.exists(_.size == 1))
  }

  test(
    "DAVE proposal validation should reject add proposals for unrecognized users"
  ) {
    val externalKey = generateSigningKey()
    val addUserState = DaveProtocol.generateKeyState[IO](userId).unsafeRunSync()
    val addKeyPackageMessage =
      DaveProtocol.buildKeyPackageMessageSync(addUserState, userId)
    val proposal =
      MlsPrimitives.uint16(MlsMessages.ProposalTypeAdd) ++ addKeyPackageMessage
    val publicProposal = signedExternalProposal(externalKey, proposal)
    val proposalBatch = MlsPrimitives.uint8(0) ++ MlsPrimitives.vectorVar(
      List(MlsPrimitives.opaqueVar(publicProposal))
    )
    val externalSender = DaveSupport.ExternalSender(
      signatureKey = uncompressedPoint(externalKey),
      credential = DaveSupport.Credential(
        credentialType = 1,
        identity = "gateway".getBytes("UTF-8")
      )
    )

    val validated = DaveProtocol.validateProposalBatches(
      externalSender = Some(externalSender),
      proposalBatches = List(proposalBatch),
      recognizedUserIds = Set.empty
    )

    assert(validated.isLeft)
  }

  test(
    "DAVE MLS engines should process external add proposals, commit, welcome, and derive matching media secrets"
  ) {
    val leaderUserId = "149639766382608384"
    val joinerUserId = "1090123456789012345"
    val groupId = "voice-channel"
    val leaderState =
      DaveProtocol.generateKeyState[IO](leaderUserId).unsafeRunSync()
    val joinerState =
      DaveProtocol.generateKeyState[IO](joinerUserId).unsafeRunSync()
    val externalSigner =
      DaveMlsTestHarness.createExternalSigner("gateway".getBytes("UTF-8"))
    val proposalBatch = externalSigner.signedAddProposalBatch(
      groupId,
      DaveProtocol.buildKeyPackageMessageSync(joinerState, joinerUserId)
    )

    val commitWelcome = DaveProtocol.buildCommitWelcomeFromProposals(
      state = leaderState,
      selfUserId = leaderUserId,
      groupId = groupId,
      externalSender = Some(externalSigner.sender()),
      proposals = List(proposalBatch),
      recognizedUserIds = Set(leaderUserId, joinerUserId)
    )

    val welcome = commitWelcome match {
      case Right(Some(value)) =>
        assert(value.welcomeMessage.nonEmpty)
        assert(
          MlsMessages.parseWelcomePayload(value.welcomeMessage.get).isRight
        )
        leaderState.mlsEngine.processCommit(value.commitMessage)
        value.welcomeMessage.get

      case other =>
        fail(s"Expected commit/welcome, got $other")
    }

    DaveProtocol.processWelcomeForSelf(
      state = joinerState,
      selfUserId = joinerUserId,
      groupId = groupId,
      welcomeMessage = welcome,
      recognizedUserIds = Set(leaderUserId, joinerUserId)
    ) match {
      case Right(_)    => ()
      case Left(error) => fail(error)
    }

    assert(
      leaderState.mlsEngine
        .senderBaseSecretFor(leaderUserId)
        .sameElements(joinerState.mlsEngine.senderBaseSecretFor(leaderUserId))
    )
    assert(
      leaderState.mlsEngine
        .senderBaseSecretFor(joinerUserId)
        .sameElements(joinerState.mlsEngine.senderBaseSecretFor(joinerUserId))
    )
  }

  private def signedExternalProposal(
      externalKey: KeyPair,
      proposal: Array[Byte]
  ): Array[Byte] = {
    val framedContent =
      MlsPrimitives.opaqueVar("voice-channel".getBytes("UTF-8")) ++
        MlsPrimitives.uint64(0L) ++
        MlsPrimitives.uint8(MlsMessages.SenderTypeExternal) ++
        MlsPrimitives.uint32(0L) ++
        MlsPrimitives.opaqueVar(Array.emptyByteArray) ++
        MlsPrimitives.uint8(MlsMessages.ContentTypeProposal) ++
        proposal
    val tbs =
      MlsPrimitives.uint16(MlsPrimitives.MlsVersion10) ++
        MlsPrimitives.uint16(MlsMessages.WireFormatPublicMessage) ++
        framedContent
    val signature = signWithLabel("FramedContentTBS", tbs, externalKey)

    MlsPrimitives.uint16(MlsPrimitives.MlsVersion10) ++
      MlsPrimitives.uint16(MlsMessages.WireFormatPublicMessage) ++
      framedContent ++
      MlsPrimitives.opaqueVar(signature)
  }

  private def signWithLabel(
      label: String,
      content: Array[Byte],
      keyPair: KeyPair
  ): Array[Byte] = {
    val signer = Signature.getInstance("SHA256withECDSA")
    signer.initSign(keyPair.getPrivate)
    signer.update(
      MlsPrimitives.opaqueVar(
        s"MLS 1.0 $label".getBytes("UTF-8")
      ) ++ MlsPrimitives.opaqueVar(content)
    )
    signer.sign()
  }

  private def generateSigningKey(): KeyPair = {
    val gen = KeyPairGenerator.getInstance("EC")
    gen.initialize(new ECGenParameterSpec("secp256r1"))
    gen.generateKeyPair()
  }

  private def uncompressedPoint(kp: KeyPair): Array[Byte] = {
    val ec = kp.getPublic.asInstanceOf[ECPublicKey]
    Array(0x04.toByte) ++ padTo32(ec.getW.getAffineX.toByteArray) ++ padTo32(
      ec.getW.getAffineY.toByteArray
    )
  }

  private def padTo32(bytes: Array[Byte]): Array[Byte] =
    if (bytes.length == 32) bytes
    else if (bytes.length > 32) bytes.drop(bytes.length - 32)
    else Array.fill[Byte](32 - bytes.length)(0) ++ bytes
}
