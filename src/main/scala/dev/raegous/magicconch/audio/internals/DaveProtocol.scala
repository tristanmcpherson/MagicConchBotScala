package dev.raegous.magicconch.audio.internals

import cats.effect.Sync

import java.nio.ByteBuffer
import java.security.{KeyPair, KeyPairGenerator, SecureRandom}
import java.security.interfaces.ECPublicKey
import java.security.spec.ECGenParameterSpec

case class DaveKeyState(
    signingKeyPair: KeyPair,
    hpkeKeyPair: KeyPair,
    initKeyPair: KeyPair,
    mlsEngine: DaveMlsEngine
)

final case class DaveMlsGroupState(
    groupId: String,
    selfUserId: String,
    epoch: Long,
    treeHash: Array[Byte],
    confirmedTranscriptHash: Array[Byte],
    interimTranscriptHash: Array[Byte],
    epochSecret: Array[Byte],
    exporterSecret: Array[Byte]
) {
  def selfSenderRatchet: DaveSupport.SenderKeyRatchet =
    MlsPrimitives.daveSenderRatchet(exporterSecret, selfUserId)
}

final case class DaveValidatedProposal(
    ref: Array[Byte],
    proposal: MlsMessages.Proposal,
    groupId: Array[Byte],
    rawMessage: Option[Array[Byte]]
)

object DaveProtocol {
  private val ProtocolVersionMls10 = 0x0001
  private val CipherSuiteDhkemp256Aes128GcmSha256P256 = 0x0002
  private val WireFormatKeyPackage = 0x0005
  private val CredentialTypeBasic = 0x0001
  private val LeafNodeSourceKeyPackage = 0x01
  private val KeyPackageLifetimeNotBefore = Array.fill[Byte](8)(0)
  private val KeyPackageLifetimeNotAfter = Array.fill[Byte](8)(0xff.toByte)

  def generateKeyState[F[_]: Sync](): F[DaveKeyState] = Sync[F].blocking {
    val gen = KeyPairGenerator.getInstance("EC")
    gen.initialize(new ECGenParameterSpec("secp256r1"), new SecureRandom())
    val placeholderUserId = "0"
    DaveKeyState(
      signingKeyPair = gen.generateKeyPair(),
      hpkeKeyPair = gen.generateKeyPair(),
      initKeyPair = gen.generateKeyPair(),
      mlsEngine = DaveMlsEngine.create(placeholderUserId)
    )
  }

  def generateKeyState[F[_]: Sync](selfUserId: String): F[DaveKeyState] =
    Sync[F].blocking {
      generateKeyStateSync(selfUserId)
    }

  def generateKeyStateSync(selfUserId: String): DaveKeyState = {
    val gen = KeyPairGenerator.getInstance("EC")
    gen.initialize(new ECGenParameterSpec("secp256r1"), new SecureRandom())
    DaveKeyState(
      signingKeyPair = gen.generateKeyPair(),
      hpkeKeyPair = gen.generateKeyPair(),
      initKeyPair = gen.generateKeyPair(),
      mlsEngine = DaveMlsEngine.create(selfUserId)
    )
  }

  def buildKeyPackageMessage[F[_]: Sync](
      state: DaveKeyState,
      userId: String
  ): F[Array[Byte]] =
    Sync[F].blocking(state.mlsEngine.keyPackageMessage())

  def buildKeyPackageMessageSync(
      state: DaveKeyState,
      userId: String
  ): Array[Byte] =
    state.mlsEngine.keyPackageMessage()

  def buildCommitWelcomeFromProposals(
      state: DaveKeyState,
      selfUserId: String,
      groupId: String,
      externalSender: Option[DaveSupport.ExternalSender],
      proposals: List[Array[Byte]],
      recognizedUserIds: Set[String]
  ): Either[String, Option[DaveSupport.MlsCommitWelcome]] =
    for {
      pending <- validateProposalBatches(
        externalSender,
        proposals,
        recognizedUserIds
      )
      sender <- externalSender.toRight("Missing DAVE external sender package")
      _ <-
        if (pending.isEmpty) Right(())
        else
          attempt(
            state.mlsEngine.initializeLocalGroup(pending.head.groupId, sender)
          )
      _ <- applyProposalBatchesToMls(state.mlsEngine, pending, proposals)
      result <-
        if (pending.isEmpty) Right(None)
        else
          attempt(
            state.mlsEngine.commitPending(
              pending.exists(_.proposal.isInstanceOf[MlsMessages.AddProposal])
            )
          ).map(Some(_))
    } yield result

  def processCommitForSelf(
      state: DaveKeyState,
      selfUserId: String,
      groupId: String,
      commitMessage: Array[Byte]
  ): Either[String, Option[DaveSupport.SenderKeyRatchet]] =
    for {
      mlsMessage <- MlsMessages
        .parseMlsMessage(commitMessage)
        .left
        .map(_.message)
      publicMessage <- MlsMessages
        .parsePublicMessage(mlsMessage)
        .left
        .map(_.message)
      _ <- Either.cond(
        publicMessage.framedContent.contentType == MlsMessages.ContentTypeCommit,
        (),
        "MLS message is not a commit"
      )
      commit <- MlsMessages
        .parseCommit(publicMessage.framedContent.content)
        .left
        .map(_.message)
      _ <- Either.cond(
        commit.proposals.nonEmpty,
        (),
        "DAVE commits must reference at least one proposal"
      )
      _ <- Either.cond(
        commit.proposals.forall(_.isInstanceOf[MlsMessages.ProposalReference]),
        (),
        "DAVE commits must reference cached gateway proposals and must not include inline proposals"
      )
      result <- attempt(state.mlsEngine.processCommit(commitMessage))
        .map(Some(_))
    } yield result

  def processWelcomeForSelf(
      state: DaveKeyState,
      selfUserId: String,
      groupId: String,
      welcomeMessage: Array[Byte],
      recognizedUserIds: Set[String]
  ): Either[String, DaveSupport.SenderKeyRatchet] =
    for {
      welcome <- MlsMessages
        .parseWelcomePayload(welcomeMessage)
        .left
        .map(_.message)
      _ <- Either.cond(
        welcome.cipherSuite == CipherSuiteDhkemp256Aes128GcmSha256P256,
        (),
        s"Unsupported MLS welcome ciphersuite: ${welcome.cipherSuite}"
      )
      ownRef = keyPackageRef(state, selfUserId)
      _ <- Either.cond(
        welcome.secrets.exists(secret => secret.newMember.sameElements(ownRef)),
        (),
        "MLS welcome does not contain an encrypted group secret for this key package"
      )
      result <- attempt(state.mlsEngine.processWelcome(welcomeMessage))
    } yield result

  def validateProposalBatches(
      externalSender: Option[DaveSupport.ExternalSender],
      proposalBatches: List[Array[Byte]],
      recognizedUserIds: Set[String]
  ): Either[String, List[DaveValidatedProposal]] = {
    val sender = externalSender.toRight("Missing DAVE external sender package")
    sender.flatMap { gatewaySender =>
      val pending = scala.collection.mutable.LinkedHashMap
        .empty[Vector[Byte], DaveValidatedProposal]
      proposalBatches
        .foldLeft[Either[String, Unit]](Right(())) {
          case (Left(error), _)  => Left(error)
          case (Right(_), bytes) =>
            MlsMessages.parseProposalBatch(bytes).left.map(_.message).flatMap {
              batch =>
                if (batch.isRevoke) {
                  batch.revokedRefs.foreach(ref => pending.remove(ref.toVector))
                  Right(())
                } else {
                  batch.proposals
                    .zip(batch.rawProposalMessages.map(Some(_)))
                    .foldLeft[Either[String, Unit]](Right(())) {
                      case (Left(error), _) => Left(error)
                      case (Right(_), (publicMessage, rawMessage)) =>
                        validateGatewayProposal(
                          gatewaySender,
                          publicMessage,
                          rawMessage,
                          recognizedUserIds
                        ).map { proposal =>
                          pending.update(proposal.ref.toVector, proposal)
                        }
                    }
                }
            }
        }
        .map(_ => pending.values.toList)
    }
  }

  def deriveSelfRatchetFromExporterSecret(
      exporterSecret: Array[Byte],
      selfUserId: String
  ): DaveSupport.SenderKeyRatchet =
    MlsPrimitives.daveSenderRatchet(exporterSecret, selfUserId)

  def deriveExporterSecret(
      epochSecret: Array[Byte],
      confirmedTranscriptHash: Array[Byte]
  ): Array[Byte] = {
    val exporterSecret = MlsPrimitives.deriveSecret(
      epochSecret,
      "exporter",
      confirmedTranscriptHash
    )
    require(confirmedTranscriptHash.length == MlsPrimitives.HashLength)
    exporterSecret
  }

  def keyPackageRef(state: DaveKeyState, userId: String): Array[Byte] = {
    val keyPackageBytes = state.mlsEngine.keyPackageMessage()
    MlsMessages.parseKeyPackageMessage(keyPackageBytes) match {
      case Right(_) =>
        MlsPrimitives.refHash("KeyPackage Reference", keyPackageBytes)

      case Left(error) =>
        throw new IllegalStateException(error.message)
    }
  }

  def proposalRef(publicMessage: MlsMessages.PublicMessage): Array[Byte] =
    MlsPrimitives.refHash(
      "Proposal Reference",
      publicMessage.tbs.drop(2) ++ MlsPrimitives.opaqueVar(
        publicMessage.signature
      )
    )

  private def validateGatewayProposal(
      externalSender: DaveSupport.ExternalSender,
      publicMessage: MlsMessages.PublicMessage,
      rawMessage: Option[Array[Byte]],
      recognizedUserIds: Set[String]
  ): Either[String, DaveValidatedProposal] =
    for {
      _ <- Either.cond(
        publicMessage.framedContent.sender.senderType == MlsMessages.SenderTypeExternal,
        (),
        "DAVE proposal was not sent by an external sender"
      )
      _ <- Either.cond(
        publicMessage.framedContent.sender.index.contains(0L),
        (),
        "DAVE proposal external sender index was not 0"
      )
      _ <- Either.cond(
        publicMessage.framedContent.contentType == MlsMessages.ContentTypeProposal,
        (),
        "DAVE proposal message did not contain MLS proposal content"
      )
      _ <- Either.cond(
        externalSender.credential.credentialType == CredentialTypeBasic,
        (),
        s"Unsupported external sender credential type: ${externalSender.credential.credentialType}"
      )
      _ <- Either.cond(
        MlsMessages.verifyPublicMessageSignature(
          publicMessage,
          externalSender.signatureKey
        ),
        (),
        "DAVE external proposal signature verification failed"
      )
      proposal <- MlsMessages
        .parseProposal(publicMessage.framedContent.content)
        .left
        .map(_.message)
      _ <- validateProposalPayload(proposal, recognizedUserIds)
    } yield DaveValidatedProposal(
      proposalRef(publicMessage),
      proposal,
      publicMessage.framedContent.groupId,
      rawMessage.map(_.clone())
    )

  private def validateProposalPayload(
      proposal: MlsMessages.Proposal,
      recognizedUserIds: Set[String]
  ): Either[String, Unit] =
    proposal match {
      case MlsMessages.AddProposal(keyPackage) =>
        for {
          _ <- Either.cond(
            keyPackage.version == ProtocolVersionMls10,
            (),
            s"Unsupported key package MLS version: ${keyPackage.version}"
          )
          _ <- Either.cond(
            keyPackage.cipherSuite == CipherSuiteDhkemp256Aes128GcmSha256P256,
            (),
            s"Unsupported key package cipher suite: ${keyPackage.cipherSuite}"
          )
          userId <- keyPackage.leafNode.userId.toRight(
            "Add proposal key package did not contain a Discord user ID credential"
          )
          _ <- Either.cond(
            recognizedUserIds.contains(userId),
            (),
            s"Add proposal user $userId is not in the recognized voice participant set"
          )
          _ <- Either.cond(
            MlsMessages.verifyLeafNodeSignature(keyPackage.leafNode),
            (),
            s"Add proposal key package for $userId has an invalid leaf signature"
          )
          _ <- Either.cond(
            MlsMessages.verifyKeyPackageSignature(keyPackage),
            (),
            s"Add proposal key package for $userId has an invalid key-package signature"
          )
        } yield ()

      case _: MlsMessages.RemoveProposal =>
        Right(())

      case MlsMessages.RawProposal(proposalType, _) =>
        Left(
          s"DAVE only accepts Add and Remove proposals, got proposal type $proposalType"
        )
    }

  private def applyProposalBatchesToMls(
      engine: DaveMlsEngine,
      validatedProposals: List[DaveValidatedProposal],
      proposalBatches: List[Array[Byte]]
  ): Either[String, Unit] = {
    val rawMessagesByRef = validatedProposals
      .flatMap(proposal =>
        proposal.rawMessage.map(raw => proposal.ref.toVector -> raw)
      )
      .toMap

    proposalBatches.foldLeft[Either[String, Unit]](Right(())) {
      case (Left(error), _)  => Left(error)
      case (Right(_), bytes) =>
        MlsMessages.parseProposalBatch(bytes).left.map(_.message).flatMap {
          batch =>
            if (batch.isRevoke) {
              batch.revokedRefs.foldLeft[Either[String, Unit]](Right(())) {
                case (Left(error), _) => Left(error)
                case (Right(_), ref)  => attempt(engine.revokeProposal(ref))
              }
            } else {
              batch.proposals.foldLeft[Either[String, Unit]](Right(())) {
                case (Left(error), _)          => Left(error)
                case (Right(_), publicMessage) =>
                  val messageBytes = rawMessagesByRef.getOrElse(
                    proposalRef(publicMessage).toVector,
                    MlsPrimitives.uint16(MlsPrimitives.MlsVersion10) ++
                      MlsPrimitives
                        .uint16(MlsMessages.WireFormatPublicMessage) ++
                      publicMessage.tbs.drop(4) ++
                      MlsPrimitives.opaqueVar(publicMessage.signature)
                  )
                  attempt(engine.cacheProposal(messageBytes))
              }
            }
        }
    }
  }

  private def attempt[A](thunk: => A): Either[String, A] =
    try Right(thunk)
    catch {
      case e: Exception =>
        Left(Option(e.getMessage).getOrElse(e.getClass.getSimpleName))
    }

  private def buildKeyPackage(
      state: DaveKeyState,
      userId: String
  ): Array[Byte] = {
    val sigPub = uncompressedPoint(state.signingKeyPair)
    val hpkePub = uncompressedPoint(state.hpkeKeyPair)
    val initPub = uncompressedPoint(state.initKeyPair)

    val leafTbs = buildLeafNodeTbs(sigPub, hpkePub, userId)
    val leafSig = signWithLabel("LeafNodeTBS", leafTbs, state.signingKeyPair)
    val leafNode = leafTbs ++ opaqueVar(leafSig)

    val kpTbs = buildKeyPackageTbs(initPub, leafNode)
    val kpSig = signWithLabel("KeyPackageTBS", kpTbs, state.signingKeyPair)
    kpTbs ++ opaqueVar(kpSig)
  }

  private def buildLeafNodeTbs(
      sigPub: Array[Byte],
      encPub: Array[Byte],
      userId: String
  ): Array[Byte] = {
    opaqueVar(encPub) ++
      opaqueVar(sigPub) ++
      uint16Bytes(CredentialTypeBasic) ++ opaqueVar(
        discordUserIdIdentity(userId)
      ) ++
      buildCapabilities() ++
      Array(LeafNodeSourceKeyPackage.toByte) ++
      KeyPackageLifetimeNotBefore ++ KeyPackageLifetimeNotAfter ++
      vectorVar(Nil)
  }

  private def buildCapabilities(): Array[Byte] =
    vectorVar(List(uint16Bytes(ProtocolVersionMls10))) ++
      vectorVar(List(uint16Bytes(CipherSuiteDhkemp256Aes128GcmSha256P256))) ++
      vectorVar(Nil) ++
      vectorVar(Nil) ++
      vectorVar(List(uint16Bytes(CredentialTypeBasic)))

  private def buildKeyPackageTbs(
      initKey: Array[Byte],
      leafNode: Array[Byte]
  ): Array[Byte] =
    uint16Bytes(ProtocolVersionMls10) ++
      uint16Bytes(CipherSuiteDhkemp256Aes128GcmSha256P256) ++
      opaqueVar(initKey) ++
      leafNode ++
      vectorVar(Nil)

  private def buildMlsMessage(keyPackage: Array[Byte]): Array[Byte] =
    uint16Bytes(ProtocolVersionMls10) ++
      uint16Bytes(WireFormatKeyPackage) ++
      keyPackage

  private def signWithLabel(
      label: String,
      content: Array[Byte],
      kp: KeyPair
  ): Array[Byte] = {
    val fullLabel = s"MLS 1.0 $label".getBytes("UTF-8")
    val msg = opaqueVar(fullLabel) ++ opaqueVar(content)

    val sig = java.security.Signature.getInstance("SHA256withECDSA")
    sig.initSign(kp.getPrivate)
    sig.update(msg)
    sig.sign()
  }

  private def uncompressedPoint(kp: KeyPair): Array[Byte] = {
    val ec = kp.getPublic.asInstanceOf[ECPublicKey]
    Array(0x04.toByte) ++ padTo32(ec.getW.getAffineX.toByteArray) ++ padTo32(
      ec.getW.getAffineY.toByteArray
    )
  }

  private def padTo32(bytes: Array[Byte]): Array[Byte] = bytes.length match {
    case 32          => bytes
    case n if n > 32 => bytes.drop(n - 32)
    case n           => Array.fill[Byte](32 - n)(0) ++ bytes
  }

  private def discordUserIdIdentity(userId: String): Array[Byte] =
    uint64Bytes(java.lang.Long.parseUnsignedLong(userId))

  private def opaqueVar(data: Array[Byte]): Array[Byte] =
    mlsVarint(data.length) ++ data

  private def vectorVar(elements: List[Array[Byte]]): Array[Byte] = {
    val data = elements.foldLeft(Array.emptyByteArray)(_ ++ _)
    mlsVarint(data.length) ++ data
  }

  private def mlsVarint(value: Int): Array[Byte] = {
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

  private def uint64Bytes(v: Long): Array[Byte] = {
    val bb = ByteBuffer.allocate(8)
    bb.putLong(v)
    bb.array()
  }
}
