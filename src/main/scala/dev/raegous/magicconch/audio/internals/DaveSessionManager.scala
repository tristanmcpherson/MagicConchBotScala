package dev.raegous.magicconch.audio.internals

import cats.effect.Async
import cats.syntax.all.*

import java.util.Arrays
import java.util.concurrent.locks.StampedLock
import scala.collection.mutable

sealed trait DaveGatewayAction

object DaveGatewayAction {
  final case class SendMlsKeyPackage(payload: Array[Byte]) extends DaveGatewayAction
  final case class SendMlsCommitWelcome(payload: Array[Byte]) extends DaveGatewayAction
  final case class SendReadyForTransition(transitionId: Int) extends DaveGatewayAction
  final case class SendInvalidCommitWelcome(transitionId: Int) extends DaveGatewayAction
}

/**
 * In-repo DAVE session coordinator.
 *
 * This class deliberately does not depend on any native DAVE implementation.
 * It owns the local key package, tracks Discord voice gateway
 * DAVE transitions, and exposes media encryption once an internal MLS backend
 * has derived the self sender ratchet.
 */
final class DaveSessionManager[F[_]: Async] private (
  selfUserId: String,
  channelId: String,
  initialKeyState: DaveKeyState
) extends AutoCloseable {

  import DaveGatewayAction.*
  import DaveSessionManager.*

  private val lock = new StampedLock
  private val recognizedUserIds = mutable.Set[String](selfUserId)
  private val pendingTransitions = mutable.Map.empty[Int, Int]
  private val proposals = mutable.ArrayBuffer.empty[Array[Byte]]

  private var keyState: DaveKeyState = initialKeyState
  private var externalSender: Option[DaveSupport.ExternalSender] = None
  private var selfRatchet: Option[DaveSupport.SenderKeyRatchet] = None
  private var selfRatchetTransitionId: Option[Int] = None
  private var mediaRatchetActive: Boolean = false
  private var executedTransitionId: Option[Int] = None
  private var currentProtocolVersion: Int = 0
  private var nonceCounter: Long = 0L
  private var assignedAudioSsrc: Option[Int] = None
  private var lastError: Option[String] = None
    private var keyPackageSentAfterExternalSender: Boolean = false
    private var pendingLocalGroupInitialized: Boolean = false
    private var encryptedFrameCount: Long = 0L
    private var sessionStatus: DaveSessionManager.SessionStatus = DaveSessionManager.SessionStatus.Inactive
    private var closed: Boolean = false

  def maxSupportedProtocolVersion: Int =
    DaveSupport.MaxSupportedProtocolVersion

  def isMediaReady: F[Boolean] =
    readLocked(currentProtocolVersion == 0 || (sessionStatus == DaveSessionManager.SessionStatus.Active && activeSelfRatchet().fold(false)(_ => true)))

  def debugState: F[String] =
    readLocked {
      s"protocol=$currentProtocolVersion, status=${sessionStatus.name}, ready=${sessionStatus == DaveSessionManager.SessionStatus.Active && activeSelfRatchet().fold(false)(_ => true)}, selfRatchet=${selfRatchet.fold(false)(_ => true)}, mediaRatchetActive=$mediaRatchetActive, " +
        s"externalSender=${externalSender.fold(false)(_ => true)}, recognizedUsers=${recognizedUserIds.size}, " +
        s"selfRecognized=${recognizedUserIds.contains(selfUserId)}, " +
        s"pendingProposals=${proposals.size}, pendingTransitions=${pendingTransitions.keys.toList.sorted.mkString("[", ",", "]")}, " +
        s"lastProposalBytes=${proposals.lastOption.fold(0)(_.length)}, " +
        s"selfRatchetTransition=${selfRatchetTransitionId.fold("none")(_.toString)}, executedTransition=${executedTransitionId.fold("none")(_.toString)}, " +
        s"audioSsrcAssigned=${assignedAudioSsrc.fold(false)(_ => true)}, encryptedFrames=$encryptedFrameCount, " +
        s"localGroupInitialized=$pendingLocalGroupInitialized, " +
        s"keyPackageAfterExternalSender=$keyPackageSentAfterExternalSender, lastError=${lastError.getOrElse("none")}"
    }

  def assignAudioSsrc(ssrc: Int): F[Unit] =
    writeLocked {
      assignedAudioSsrc = Some(ssrc)
    }

  def addUser(userId: String): F[Unit] =
    writeLocked {
      if (!closed) recognizedUserIds += userId
    }

  def addUsers(userIds: List[String]): F[List[DaveGatewayAction]] =
    writeLocked {
      if (!closed) recognizedUserIds ++= userIds
      tryCommitPendingProposals()
    }

  def removeUser(userId: String): F[Unit] =
    writeLocked {
      if (!closed) recognizedUserIds -= userId
    }

  def onSelectProtocolAck(protocolVersion: Int): F[List[DaveGatewayAction]] =
    writeLocked {
      daveProtocolInit(protocolVersion)
    }

  def onPrepareTransition(transitionId: Int, protocolVersion: Int): F[List[DaveGatewayAction]] =
    writeLocked {
      prepareRatchets(transitionId, protocolVersion)
      if (transitionId == InitTransitionId) executeTransition(transitionId)
      Option.when(transitionId != InitTransitionId)(SendReadyForTransition(transitionId)).toList
    }

  def onExecuteTransition(transitionId: Int): F[Unit] =
    writeLocked {
      executeTransition(transitionId)
    }

  def onPrepareEpoch(transitionId: Int, epoch: Long, protocolVersion: Int): F[List[DaveGatewayAction]] =
    writeLocked {
      if (epoch == MlsNewGroupEpoch) {
        proposals.clear()
          clearRatchetState()
          pendingLocalGroupInitialized = false
          keyPackageSentAfterExternalSender = false
          prepareRatchets(transitionId, protocolVersion)
        List(sendFreshMlsKeyPackage())
      } else {
        prepareRatchets(transitionId, protocolVersion)
        Nil
      }
    }

  def onExternalSenderPackage(payload: Array[Byte]): F[List[DaveGatewayAction]] =
    writeLocked {
      DaveSupport.GatewayBinaryCodec.decode(Array[Byte](0, 0, DaveSupport.OpMlsExternalSenderPackage.toByte) ++ payload)
        .toOption
        .collect { case msg: DaveSupport.MlsExternalSenderPackage => msg.externalSender }
        .foreach(sender => externalSender = Some(sender))
      ensurePendingLocalGroup()
      sendKeyPackageAfterExternalSender() ++ tryCommitPendingProposals()
    }

  def onMlsProposals(payload: Array[Byte]): F[List[DaveGatewayAction]] =
    writeLocked {
      proposals += payload.clone()
      tryCommitPendingProposals()
    }

  def onMlsCommitTransition(transitionId: Int, payload: Array[Byte]): F[List[DaveGatewayAction]] =
    writeLocked {
      DaveProtocol.processCommitForSelf(keyState, selfUserId, channelId, payload) match {
        case Right(Some(ratchet)) =>
          lastError = None
          sessionStatus = DaveSessionManager.SessionStatus.Active
          prepareRatchets(transitionId, currentProtocolVersion)
          setSelfRatchet(transitionId, ratchet)
          List(SendReadyForTransition(transitionId))
        case Right(None) =>
          pendingTransitions -= transitionId
          Nil
        case Left(error) =>
          recoverFromInvalidTransition(transitionId, s"Failed to process MLS commit transition: $error")
      }
    }

  def onMlsWelcome(transitionId: Int, payload: Array[Byte]): F[List[DaveGatewayAction]] =
    writeLocked {
      DaveProtocol.processWelcomeForSelf(keyState, selfUserId, channelId, payload, recognizedUserIds.toSet) match {
        case Right(ratchet) =>
          lastError = None
          sessionStatus = DaveSessionManager.SessionStatus.Active
          prepareRatchets(transitionId, currentProtocolVersion)
          setSelfRatchet(transitionId, ratchet)
          List(SendReadyForTransition(transitionId))
        case Left(error) =>
          recoverFromInvalidTransition(transitionId, s"Failed to process MLS welcome: $error")
      }
    }

  def encryptAudio(ssrc: Int, opusPayload: Array[Byte]): F[Array[Byte]] =
    writeLocked(encryptAudioLocked(ssrc, opusPayload, selfCheck = false).frame)

  def encryptAudioWithSelfCheck(ssrc: Int, opusPayload: Array[Byte]): F[DaveSessionManager.EncryptedAudio] =
    writeLocked(encryptAudioLocked(ssrc, opusPayload, selfCheck = true))

  private def encryptAudioLocked(ssrc: Int, opusPayload: Array[Byte], selfCheck: Boolean): DaveSessionManager.EncryptedAudio = {
    assignedAudioSsrc.filter(_ == ssrc).getOrElse(
      throw new RuntimeException(s"DAVE audio SSRC mismatch: assigned=${assignedAudioSsrc.fold("none")(_.toString)}, actual=$ssrc")
    )
    val ratchet = activeSelfRatchet().getOrElse(
      throw new RuntimeException("DAVE media ratchet is not established")
    )
    Option.when(Arrays.equals(opusPayload, DaveSessionManager.OpusSilenceFrame))(
      DaveSessionManager.EncryptedAudio(opusPayload.clone(), Right(true), daveApplied = false)
    ).getOrElse {
      nonceCounter = (nonceCounter + 1) & 0xFFFFFFFFL
      encryptedFrameCount += 1L
      val nonce = nonceCounter & 0xFFFFFFFFL
      val encryptedFrame = DaveSupport.MediaFrameCodec.encryptAudioFrame(opusPayload, nonce, ratchet)
      val checked = Option.when(selfCheck) {
        DaveSupport.MediaFrameCodec
          .decryptFrame(encryptedFrame, ratchet)
          .map(decrypted => Arrays.equals(decrypted, opusPayload))
      }.getOrElse(Right(true))
      DaveSessionManager.EncryptedAudio(encryptedFrame, checked, daveApplied = true)
    }
  }

  private def daveProtocolInit(protocolVersion: Int): List[DaveGatewayAction] =
    if (protocolVersion > 0) {
      currentProtocolVersion = protocolVersion
      resetPendingState()
      List(sendFreshMlsKeyPackage())
    } else {
      currentProtocolVersion = 0
      resetPendingState()
      mediaRatchetActive = true
      sessionStatus = DaveSessionManager.SessionStatus.Active
      Nil
    }

  private def tryCommitPendingProposals(): List[DaveGatewayAction] =
    if (proposals.isEmpty) Nil
    else {
      // Full RFC 9420 commit generation is implemented in DaveProtocol, not in a native dependency.
      DaveProtocol.buildCommitWelcomeFromProposals(keyState, selfUserId, channelId, externalSender, proposals.toList, recognizedUserIds.toSet) match {
        case Right(Some(commitWelcome)) =>
          lastError = None
          proposals.clear()
          sessionStatus = DaveSessionManager.SessionStatus.AwaitingResponse
          refreshMediaRatchetState()
          val commitAction = SendMlsCommitWelcome(commitWelcome.commitMessage ++ commitWelcome.welcomeMessage.getOrElse(Array.emptyByteArray))
          List(commitAction)
        case Right(None) =>
          lastError = Some("Proposal processing produced no pending MLS proposals to commit")
          Nil
        case Left(error) =>
          lastError = Some(error)
          Nil
      }
    }

  private def prepareRatchets(transitionId: Int, protocolVersion: Int): Unit = {
    currentProtocolVersion = protocolVersion
    mediaRatchetActive = protocolVersion == 0
    if (transitionId != InitTransitionId) pendingTransitions.update(transitionId, protocolVersion)
    refreshMediaRatchetState()
  }

  private def executeTransition(transitionId: Int): Unit = {
    val protocolVersion = pendingTransitions.remove(transitionId).getOrElse(currentProtocolVersion)
    currentProtocolVersion = protocolVersion
    executedTransitionId = Some(transitionId)
    Option.when(transitionId == InitTransitionId && protocolVersion > 0 && pendingLocalGroupInitialized && selfRatchet.fold(true)(_ => false)) {
      sessionStatus = DaveSessionManager.SessionStatus.Pending
    }
    refreshMediaRatchetState()
  }

  private def sendFreshMlsKeyPackage(): DaveGatewayAction.SendMlsKeyPackage = {
    keyState = DaveProtocol.generateKeyStateSync(selfUserId)
    pendingLocalGroupInitialized = false
    ensurePendingLocalGroup()
    SendMlsKeyPackage(DaveProtocol.buildKeyPackageMessageSync(keyState, selfUserId))
  }

  private def setSelfRatchet(transitionId: Int, ratchet: DaveSupport.SenderKeyRatchet): Unit = {
    selfRatchet = Some(ratchet)
    selfRatchetTransitionId = Some(transitionId)
    nonceCounter = 0L
    refreshMediaRatchetState()
  }

  private def clearRatchetState(): Unit = {
    selfRatchet = None
    selfRatchetTransitionId = None
    nonceCounter = 0L
    sessionStatus = Option.when(currentProtocolVersion == 0)(DaveSessionManager.SessionStatus.Active).getOrElse(DaveSessionManager.SessionStatus.Inactive)
    refreshMediaRatchetState()
  }

  private def activeSelfRatchet(): Option[DaveSupport.SenderKeyRatchet] =
    selfRatchet.filter(_ => mediaRatchetActive)

  private def refreshMediaRatchetState(): Unit = {
    mediaRatchetActive = currentProtocolVersion == 0 || (sessionStatus == DaveSessionManager.SessionStatus.Active && executedTransitionId.exists(selfRatchetTransitionId.contains))
  }

  private def resetPendingState(): Unit = {
    clearRatchetState()
    pendingTransitions.clear()
    proposals.clear()
    pendingLocalGroupInitialized = false
    keyPackageSentAfterExternalSender = false
    executedTransitionId = None
    sessionStatus = Option.when(currentProtocolVersion == 0)(DaveSessionManager.SessionStatus.Active).getOrElse(DaveSessionManager.SessionStatus.Inactive)
  }

  private def recoverFromInvalidTransition(transitionId: Int, errorMessage: String): List[DaveGatewayAction] = {
    lastError = Some(errorMessage)
    resetPendingState()
    SendInvalidCommitWelcome(transitionId) :: List(sendFreshMlsKeyPackage())
  }

  private def sendKeyPackageAfterExternalSender(): List[DaveGatewayAction] =
    Option.when(currentProtocolVersion > 0 && pendingLocalGroupInitialized && !keyPackageSentAfterExternalSender) {
      val keyPackage = sendFreshMlsKeyPackage()
      keyPackageSentAfterExternalSender = true
      keyPackage
    }.toList

  private def ensurePendingLocalGroup(): Unit =
    if (!pendingLocalGroupInitialized && currentProtocolVersion > 0) {
      externalSender.foreach { sender =>
        try {
          keyState.mlsEngine.initializeLocalGroup(discordGroupIdBytes(channelId), sender)
          pendingLocalGroupInitialized = true
          sessionStatus = DaveSessionManager.SessionStatus.Pending
          lastError = None
        } catch {
          case e: Exception =>
            lastError = Some(s"Failed to create pending MLS group: ${Option(e.getMessage).getOrElse(e.getClass.getSimpleName)}")
        }
      }
    }

  private def discordGroupIdBytes(value: String): Array[Byte] =
    java.nio.ByteBuffer.allocate(8).putLong(java.lang.Long.parseUnsignedLong(value)).array()

  private def readLocked[A](thunk: => A): F[A] =
    Async[F].blocking {
      val stamp = lock.readLock()
      try thunk
      finally lock.unlockRead(stamp)
    }

  private def writeLocked[A](thunk: => A): F[A] =
    Async[F].blocking {
      val stamp = lock.writeLock()
      try thunk
      finally lock.unlockWrite(stamp)
    }

  override def close(): Unit = {
    val stamp = lock.writeLock()
    try {
      closed = true
      recognizedUserIds.clear()
      proposals.clear()
      externalSender = None
      selfRatchet = None
      selfRatchetTransitionId = None
      mediaRatchetActive = false
      executedTransitionId = None
      keyPackageSentAfterExternalSender = false
      pendingLocalGroupInitialized = false
      sessionStatus = DaveSessionManager.SessionStatus.Inactive
    } finally {
      lock.unlockWrite(stamp)
    }
  }
}

object DaveSessionManager {
  enum SessionStatus(val name: String) {
    case Inactive extends SessionStatus("INACTIVE")
    case Pending extends SessionStatus("PENDING")
    case AwaitingResponse extends SessionStatus("AWAITING_RESPONSE")
    case Active extends SessionStatus("ACTIVE")
  }

  final case class EncryptedAudio(frame: Array[Byte], selfCheck: Either[String, Boolean], daveApplied: Boolean)

  val OpusSilenceFrame: Array[Byte] = Array(0xF8.toByte, 0xFF.toByte, 0xFE.toByte)

  private val MlsNewGroupEpoch = 1L
  private val InitTransitionId = 0

  def create[F[_]: Async](selfUserId: String, channelId: String): F[DaveSessionManager[F]] =
    DaveProtocol.generateKeyState[F](selfUserId).map { keyState =>
      new DaveSessionManager[F](selfUserId, channelId, keyState)
    }
}
