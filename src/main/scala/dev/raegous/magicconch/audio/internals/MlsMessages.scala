package dev.raegous.magicconch.audio.internals

import java.math.BigInteger
import java.security.{KeyFactory, Signature}
import java.security.interfaces.ECPublicKey
import java.security.spec.{ECGenParameterSpec, ECPoint, ECPublicKeySpec}

object MlsMessages {
  import MlsPrimitives.*

  val WireFormatPublicMessage = 0x0001
  val WireFormatPrivateMessage = 0x0002
  val WireFormatWelcome = 0x0003
  val WireFormatGroupInfo = 0x0004
  val WireFormatKeyPackage = 0x0005

  val SenderTypeMember = 1
  val SenderTypeExternal = 2
  val SenderTypeNewMemberProposal = 3
  val SenderTypeNewMemberCommit = 4

  val ContentTypeApplication = 1
  val ContentTypeProposal = 2
  val ContentTypeCommit = 3

  val ProposalTypeAdd = 1
  val ProposalTypeRemove = 3

  val ProposalOrRefTypeProposal = 1
  val ProposalOrRefTypeReference = 2

  final case class MlsMessage(version: Int, wireFormat: Int, body: Array[Byte])
  final case class Sender(senderType: Int, index: Option[Long])
  final case class FramedContent(
    groupId: Array[Byte],
    epoch: Long,
    sender: Sender,
    authenticatedData: Array[Byte],
    contentType: Int,
    content: Array[Byte]
  )
  final case class PublicMessage(
    framedContent: FramedContent,
    signature: Array[Byte],
    confirmationTag: Option[Array[Byte]],
    membershipTag: Option[Array[Byte]],
    tbs: Array[Byte]
  )
  final case class KeyPackage(
    version: Int,
    cipherSuite: Int,
    initKey: Array[Byte],
    leafNode: LeafNode,
    extensions: Array[Byte],
    signature: Array[Byte],
    tbs: Array[Byte]
  )
  final case class Capabilities(
    versions: List[Int],
    cipherSuites: List[Int],
    extensions: List[Int],
    proposals: List[Int],
    credentials: List[Int]
  )
  final case class LeafNode(
    encryptionKey: Array[Byte],
    signatureKey: Array[Byte],
    credentialType: Int,
    credentialIdentity: Array[Byte],
    capabilities: Capabilities,
    source: Int,
    lifetimeNotBefore: Option[Long],
    lifetimeNotAfter: Option[Long],
    extensions: Array[Byte],
    signature: Array[Byte],
    tbs: Array[Byte]
  ) {
    def userId: Option[String] =
      Option.when(credentialType == 1 && credentialIdentity.length == 8) {
        java.lang.Long.toUnsignedString(java.nio.ByteBuffer.wrap(credentialIdentity).getLong)
      }
  }
  sealed trait Proposal
  final case class AddProposal(keyPackage: KeyPackage) extends Proposal
  final case class RemoveProposal(removed: Long) extends Proposal
  final case class RawProposal(proposalType: Int, payload: Array[Byte]) extends Proposal

  sealed trait ProposalOrRef
  final case class InlineProposal(proposal: Proposal) extends ProposalOrRef
  final case class ProposalReference(ref: Array[Byte]) extends ProposalOrRef
  final case class Commit(proposals: List[ProposalOrRef], hasPath: Boolean)
  final case class Welcome(cipherSuite: Int, secrets: List[WelcomeSecret], encryptedGroupInfo: Array[Byte])
  final case class WelcomeSecret(newMember: Array[Byte], kemOutput: Array[Byte], ciphertext: Array[Byte])

  final case class ProposalBatch(
    isRevoke: Boolean,
    proposals: List[PublicMessage],
    revokedRefs: List[Array[Byte]],
    rawProposalMessages: List[Array[Byte]] = Nil
  )

  def parseMlsMessage(bytes: Array[Byte]): Either[DecodeError, MlsMessage] = {
    val reader = Reader(bytes)
    for {
      version <- reader.readUInt16()
      wireFormat <- reader.readUInt16()
      body <- reader.readBytes(reader.remaining)
    } yield MlsMessage(version, wireFormat, body)
  }

  def parsePublicMessage(message: MlsMessage): Either[DecodeError, PublicMessage] =
    if (message.wireFormat != WireFormatPublicMessage) Left(DecodeError(s"expected public message, got wire format ${message.wireFormat}"))
    else {
      val reader = Reader(message.body)
      val contentStart = reader.position
      for {
        content <- parseFramedContent(reader)
        contentEnd = reader.position
        signature <- reader.readOpaqueVar()
        confirmation <- if (content.contentType == ContentTypeCommit) reader.readOpaqueVar().map(Some(_)) else Right(None)
        membership <- content.sender.senderType match {
          case SenderTypeMember => reader.readOpaqueVar().map(Some(_))
          case _ => Right(None)
        }
        _ <- if (reader.atEnd) Right(()) else Left(DecodeError(s"trailing public-message bytes: ${reader.remaining}"))
      } yield PublicMessage(content, signature, confirmation, membership, uint16(message.version) ++ uint16(message.wireFormat) ++ reader.slice(contentStart, contentEnd))
    }.orElse(parseDaveCompactExternalProposal(message))

  private def readRawPublicMessage(reader: Reader): Either[DecodeError, (PublicMessage, Array[Byte])] = {
    val start = reader.position
    for {
      version <- reader.readUInt16()
      wireFormat <- reader.readUInt16()
      _ <- Either.cond(wireFormat == WireFormatPublicMessage, (), DecodeError(s"expected public message, got wire format $wireFormat"))
      contentStart = reader.position
      content <- parseFramedContent(reader)
      contentEnd = reader.position
      signature <- reader.readOpaqueVar()
      confirmation <- if (content.contentType == ContentTypeCommit) reader.readOpaqueVar().map(Some(_)) else Right(None)
      membership <- content.sender.senderType match {
        case SenderTypeMember => reader.readOpaqueVar().map(Some(_))
        case _ => Right(None)
      }
    } yield {
      val raw = reader.slice(start, reader.position)
      val tbs = uint16(version) ++ uint16(wireFormat) ++ reader.slice(contentStart, contentEnd)
      (PublicMessage(content, signature, confirmation, membership, tbs), raw)
    }
  }

  def parseProposalBatch(bytes: Array[Byte]): Either[DecodeError, ProposalBatch] = {
    val reader = Reader(bytes)
    for {
      op <- reader.readUInt8()
      batch <- op match {
        case 0 =>
          parseAppendProposalMessages(reader.readBytes(reader.remaining).toOption.getOrElse(Array.emptyByteArray))
        case 1 =>
          parseRevokedProposalRefs(reader.readBytes(reader.remaining).toOption.getOrElse(Array.emptyByteArray))
        case other =>
          Left(DecodeError(s"unknown DAVE proposal operation: $other"))
      }
    } yield batch
  }

  private def parseAppendProposalMessages(bytes: Array[Byte]): Either[DecodeError, ProposalBatch] = {
    val vectorReader = Reader(bytes)
    val vectorResult =
      vectorReader
        .readVectorVar(readRawPublicMessage)
        .flatMap(messages =>
          if (vectorReader.atEnd) Right(messages)
          else Left(DecodeError(s"trailing vector proposal bytes: ${vectorReader.remaining}"))
        )
        .map(messages => ProposalBatch(isRevoke = false, messages.map(_._1), Nil, messages.map(_._2)))

    vectorResult
      .orElse(parseOpaqueProposalMessageVector(bytes))
      .orElse(parseOpaqueSingleProposalMessage(bytes))
      .orElse {
        parseMlsMessage(bytes)
        .flatMap(parsePublicMessage)
        .map(message => ProposalBatch(isRevoke = false, List(message), Nil, List(bytes)))
      }
  }

  private def parseOpaqueProposalMessageVector(bytes: Array[Byte]): Either[DecodeError, ProposalBatch] = {
    val outerReader = Reader(bytes)
    for {
      vectorBytes <- outerReader.readOpaqueVar()
      _ <- if (outerReader.atEnd) Right(()) else Left(DecodeError(s"trailing outer opaque proposal bytes: ${outerReader.remaining}"))
      messages <- readOpaqueProposalMessages(vectorBytes)
    } yield ProposalBatch(isRevoke = false, messages.map(_._1), Nil, messages.map(_._2))
  }

  private def readOpaqueProposalMessages(bytes: Array[Byte]): Either[DecodeError, List[(PublicMessage, Array[Byte])]] = {
    val reader = Reader(bytes)
    val out = scala.collection.mutable.ListBuffer.empty[(PublicMessage, Array[Byte])]
    while (!reader.atEnd) {
      val parsed = for {
        messageBytes <- reader.readOpaqueVar()
        message <- parseMlsMessage(messageBytes).flatMap(parsePublicMessage)
      } yield out += (message -> messageBytes)

      parsed match {
        case Left(error) => return Left(error)
        case Right(_) => ()
      }
    }
    Right(out.toList)
  }

  private def parseOpaqueSingleProposalMessage(bytes: Array[Byte]): Either[DecodeError, ProposalBatch] = {
    val reader = Reader(bytes)
    for {
      messageBytes <- reader.readOpaqueVar()
      _ <- if (reader.atEnd) Right(()) else Left(DecodeError(s"trailing opaque proposal bytes: ${reader.remaining}"))
      message <- parseMlsMessage(messageBytes).flatMap(parsePublicMessage)
    } yield ProposalBatch(isRevoke = false, List(message), Nil, List(messageBytes))
  }

  private def parseDaveCompactExternalProposal(message: MlsMessage): Either[DecodeError, PublicMessage] =
    if (message.wireFormat != WireFormatPublicMessage) Left(DecodeError(s"expected public message, got wire format ${message.wireFormat}"))
    else {
      val reader = Reader(message.body)
      val contentStart = reader.position
      for {
        groupId <- reader.readOpaqueVar()
        epoch <- reader.readUInt64()
        sender <- parseSender(reader)
        _ <- Either.cond(sender.senderType == SenderTypeExternal, (), DecodeError("compact DAVE proposal sender is not external"))
        contentType <- reader.readUInt8()
        content <- contentType match {
          case ContentTypeProposal => readProposalBytes(reader)
          case other => Left(DecodeError(s"unsupported compact DAVE framed content type: $other"))
        }
        contentEnd = reader.position
        signature <- reader.readOpaqueVar()
        _ <- if (reader.atEnd) Right(()) else Left(DecodeError(s"trailing compact DAVE proposal bytes: ${reader.remaining}"))
      } yield {
        val contentWithEmptyAuthData =
          reader.slice(contentStart, contentStart + groupId.length + MlsPrimitives.varint(groupId.length).length + 8 + 5) ++
            MlsPrimitives.opaqueVar(Array.emptyByteArray) ++
            reader.slice(contentStart + groupId.length + MlsPrimitives.varint(groupId.length).length + 8 + 5, contentEnd)
        PublicMessage(
          FramedContent(groupId, epoch, sender, Array.emptyByteArray, contentType, content),
          signature,
          confirmationTag = None,
          membershipTag = None,
          tbs = uint16(message.version) ++ uint16(message.wireFormat) ++ contentWithEmptyAuthData
        )
      }
    }

  private def parseRevokedProposalRefs(bytes: Array[Byte]): Either[DecodeError, ProposalBatch] = {
    val vectorReader = Reader(bytes)
    val vectorResult =
      vectorReader
        .readVectorVar(_.readOpaqueVar())
        .flatMap(refs =>
          if (vectorReader.atEnd) Right(refs)
          else Left(DecodeError(s"trailing vector revoke bytes: ${vectorReader.remaining}"))
        )
        .map(refs => ProposalBatch(isRevoke = true, Nil, refs))

    vectorResult.orElse {
      if (bytes.length > 0 && bytes.length % HashLength == 0) {
        Right(ProposalBatch(isRevoke = true, Nil, bytes.grouped(HashLength).map(_.toArray).toList))
      } else {
        Left(DecodeError(s"invalid DAVE revoke proposal-ref payload length: ${bytes.length}"))
      }
    }
  }

  def parseProposal(content: Array[Byte]): Either[DecodeError, Proposal] = {
    val reader = Reader(content)
    for {
      proposalType <- reader.readUInt16()
      proposal <- proposalType match {
        case ProposalTypeAdd => parseKeyPackage(reader).map(AddProposal.apply)
        case ProposalTypeRemove => reader.readUInt32().map(RemoveProposal.apply)
        case other => reader.readBytes(reader.remaining).map(RawProposal(other, _))
      }
    } yield proposal
  }

  def parseKeyPackageMessage(bytes: Array[Byte]): Either[DecodeError, KeyPackage] =
    parseWrappedKeyPackageMessage(bytes).orElse(parseKeyPackagePayload(bytes))

  def parseKeyPackagePayload(bytes: Array[Byte]): Either[DecodeError, KeyPackage] = {
    val reader = Reader(bytes)
    parseKeyPackage(reader).flatMap(keyPackage =>
      Either.cond(reader.atEnd, keyPackage, DecodeError(s"trailing key-package bytes: ${reader.remaining}"))
    )
  }

  def parseWelcomeMessage(bytes: Array[Byte]): Either[DecodeError, Welcome] =
    parseMlsMessage(bytes).flatMap { message =>
      if (message.wireFormat != WireFormatWelcome) Left(DecodeError(s"expected welcome message, got ${message.wireFormat}"))
      else parseWelcome(Reader(message.body))
    }

  def parseWelcomePayload(bytes: Array[Byte]): Either[DecodeError, Welcome] =
    parseWelcome(Reader(bytes))

  def verifyKeyPackageSignature(keyPackage: KeyPackage): Boolean =
    verifyWithLabel(keyPackage.leafNode.signatureKey, "KeyPackageTBS", keyPackage.tbs, keyPackage.signature)

  def verifyLeafNodeSignature(leafNode: LeafNode): Boolean =
    verifyWithLabel(leafNode.signatureKey, "LeafNodeTBS", leafNode.tbs, leafNode.signature)

  def verifyPublicMessageSignature(publicMessage: PublicMessage, signatureKey: Array[Byte]): Boolean =
    verifyWithLabel(signatureKey, "FramedContentTBS", publicMessage.tbs, publicMessage.signature)

  def parseCommit(content: Array[Byte]): Either[DecodeError, Commit] = {
    val reader = Reader(content)
    for {
      proposals <- reader.readVectorVar(parseProposalOrRef)
      hasPathByte <- reader.readUInt8()
      _ <- hasPathByte match {
        case 0 => Right(())
        case 1 => skipUpdatePath(reader)
        case other => Left(DecodeError(s"invalid MLS optional UpdatePath marker: $other"))
      }
      _ <- if (reader.atEnd) Right(()) else Left(DecodeError(s"trailing commit bytes: ${reader.remaining}"))
    } yield Commit(proposals, hasPathByte == 1)
  }

  private def parseFramedContent(reader: Reader): Either[DecodeError, FramedContent] =
    for {
      groupId <- reader.readOpaqueVar()
      epoch <- reader.readUInt64()
      sender <- parseSender(reader)
      authData <- reader.readOpaqueVar()
      contentType <- reader.readUInt8()
      content <- contentType match {
        case ContentTypeApplication => reader.readOpaqueVar()
        case ContentTypeProposal => readProposalBytes(reader)
        case ContentTypeCommit => readCommitBytes(reader)
        case other => Left(DecodeError(s"unsupported framed content type: $other"))
      }
    } yield FramedContent(groupId, epoch, sender, authData, contentType, content)

  private def parseSender(reader: Reader): Either[DecodeError, Sender] =
    reader.readUInt8().flatMap {
      case 0 => reader.readUInt32().map(idx => Sender(SenderTypeExternal, Some(idx)))
      case SenderTypeMember => reader.readUInt32().map(idx => Sender(SenderTypeMember, Some(idx)))
      case SenderTypeExternal => reader.readUInt32().map(idx => Sender(SenderTypeExternal, Some(idx)))
      case SenderTypeNewMemberProposal => Right(Sender(SenderTypeNewMemberProposal, None))
      case SenderTypeNewMemberCommit => Right(Sender(SenderTypeNewMemberCommit, None))
      case other => Left(DecodeError(s"unsupported sender type: $other"))
    }

  private def parseKeyPackage(reader: Reader): Either[DecodeError, KeyPackage] = {
    val start = reader.position
    for {
      version <- reader.readUInt16()
      cipherSuite <- reader.readUInt16()
      initKey <- reader.readOpaqueVar()
      leafNode <- parseLeafNode(reader)
      extensions <- reader.readOpaqueVar()
      tbsEnd = reader.position
      signature <- reader.readOpaqueVar()
    } yield KeyPackage(version, cipherSuite, initKey, leafNode, extensions, signature, reader.slice(start, tbsEnd))
  }

  private def parseWrappedKeyPackageMessage(bytes: Array[Byte]): Either[DecodeError, KeyPackage] =
    parseMlsMessage(bytes).flatMap(message =>
      Either
        .cond(message.wireFormat == WireFormatKeyPackage, message.body, DecodeError(s"expected key package message, got ${message.wireFormat}"))
        .flatMap(parseKeyPackagePayload)
    )

  private def parseLeafNode(reader: Reader): Either[DecodeError, LeafNode] = {
    val start = reader.position
    for {
      encryptionKey <- reader.readOpaqueVar()
      signatureKey <- reader.readOpaqueVar()
      credentialType <- reader.readUInt16()
      identity <- reader.readOpaqueVar()
      capabilities <- parseCapabilities(reader)
      source <- reader.readUInt8()
      lifetime <- source match {
        case 1 =>
          for {
            notBefore <- reader.readUInt64()
            notAfter <- reader.readUInt64()
          } yield (Some(notBefore), Some(notAfter))
        case 2 => Right((None, None))
        case 3 => reader.readOpaqueVar().map(_ => (None, None))
        case other => Left(DecodeError(s"unsupported leaf node source: $other"))
      }
      extensions <- reader.readOpaqueVar()
      tbsEnd = reader.position
      signature <- reader.readOpaqueVar()
    } yield LeafNode(
      encryptionKey,
      signatureKey,
      credentialType,
      identity,
      capabilities,
      source,
      lifetime._1,
      lifetime._2,
      extensions,
      signature,
      reader.slice(start, tbsEnd)
    )
  }

  private def parseCapabilities(reader: Reader): Either[DecodeError, Capabilities] =
    for {
      versions <- reader.readVectorVar(_.readUInt16())
      cipherSuites <- reader.readVectorVar(_.readUInt16())
      extensions <- reader.readVectorVar(_.readUInt16())
      proposals <- reader.readVectorVar(_.readUInt16())
      credentials <- reader.readVectorVar(_.readUInt16())
    } yield Capabilities(versions, cipherSuites, extensions, proposals, credentials)

  private def parseWelcome(reader: Reader): Either[DecodeError, Welcome] =
    for {
      cipherSuite <- reader.readUInt16()
      secrets <- reader.readVectorVar(parseWelcomeSecret)
      encryptedGroupInfo <- reader.readOpaqueVar()
      _ <- if (reader.atEnd) Right(()) else Left(DecodeError(s"trailing welcome bytes: ${reader.remaining}"))
    } yield Welcome(cipherSuite, secrets, encryptedGroupInfo)

  private def parseWelcomeSecret(reader: Reader): Either[DecodeError, WelcomeSecret] =
    for {
      newMember <- reader.readOpaqueVar()
      kemOutput <- reader.readOpaqueVar()
      ciphertext <- reader.readOpaqueVar()
    } yield WelcomeSecret(newMember, kemOutput, ciphertext)

  private def parseProposalOrRef(reader: Reader): Either[DecodeError, ProposalOrRef] =
    reader.readUInt8().flatMap {
      case ProposalOrRefTypeProposal => parseProposalFromReader(reader).map(InlineProposal.apply)
      case ProposalOrRefTypeReference => reader.readBytes(HashLength).map(ProposalReference.apply)
      case other => Left(DecodeError(s"unsupported ProposalOrRef type: $other"))
    }

  private def parseProposalFromReader(reader: Reader): Either[DecodeError, Proposal] =
    for {
      proposalType <- reader.readUInt16()
      proposal <- proposalType match {
        case ProposalTypeAdd => parseKeyPackage(reader).map(AddProposal.apply)
        case ProposalTypeRemove => reader.readUInt32().map(RemoveProposal.apply)
        case other => reader.readBytes(reader.remaining).map(RawProposal(other, _))
      }
    } yield proposal

  private def readProposalBytes(reader: Reader): Either[DecodeError, Array[Byte]] = {
    val start = reader.position
    parseProposalFromReader(reader).map(_ => reader.slice(start, reader.position))
  }

  private def readCommitBytes(reader: Reader): Either[DecodeError, Array[Byte]] = {
    val start = reader.position
    for {
      _ <- reader.readVectorVar(parseProposalOrRef)
      hasPath <- reader.readUInt8()
      _ <- hasPath match {
        case 0 => Right(())
        case 1 => skipUpdatePath(reader)
        case other => Left(DecodeError(s"invalid MLS optional UpdatePath marker: $other"))
      }
    } yield reader.slice(start, reader.position)
  }

  private def skipUpdatePath(reader: Reader): Either[DecodeError, Unit] =
    for {
      _ <- parseLeafNode(reader)
      _ <- reader.readVectorVar(skipUpdatePathNode)
    } yield ()

  private def skipUpdatePathNode(reader: Reader): Either[DecodeError, Unit] =
    for {
      _ <- reader.readOpaqueVar() // encryption_key
      _ <- reader.readVectorVar(skipHpkeCiphertext)
      _ <- reader.readOpaqueVar() // extensions
    } yield ()

  private def skipHpkeCiphertext(reader: Reader): Either[DecodeError, Unit] =
    for {
      _ <- reader.readOpaqueVar() // kem_output
      _ <- reader.readOpaqueVar() // ciphertext
    } yield ()

  private def verifyWithLabel(publicKeyBytes: Array[Byte], label: String, content: Array[Byte], signature: Array[Byte]): Boolean =
    try {
      val publicKey = ecPublicKey(publicKeyBytes)
      val fullLabel = s"MLS 1.0 $label".getBytes("UTF-8")
      val signedContent = MlsPrimitives.opaqueVar(fullLabel) ++ MlsPrimitives.opaqueVar(content)
      val verifier =
        if (signature.length == 64) Signature.getInstance("SHA256withECDSAinP1363Format")
        else Signature.getInstance("SHA256withECDSA")
      verifier.initVerify(publicKey)
      verifier.update(signedContent)
      verifier.verify(signature)
    } catch {
      case _: Exception => false
    }

  private def ecPublicKey(uncompressed: Array[Byte]): ECPublicKey = {
    require(uncompressed.length == 65 && uncompressed(0) == 0x04.toByte, "expected uncompressed P-256 point")
    val paramsGenerator = java.security.AlgorithmParameters.getInstance("EC")
    paramsGenerator.init(new ECGenParameterSpec("secp256r1"))
    val params = paramsGenerator.getParameterSpec(classOf[java.security.spec.ECParameterSpec])
    val x = new BigInteger(1, uncompressed.slice(1, 33))
    val y = new BigInteger(1, uncompressed.slice(33, 65))
    KeyFactory.getInstance("EC")
      .generatePublic(new ECPublicKeySpec(new ECPoint(x, y), params))
      .asInstanceOf[ECPublicKey]
  }
}
