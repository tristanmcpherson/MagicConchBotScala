package dev.raegous.magicconch.audio.internals

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import munit.FunSuite

class DaveSessionManagerTest extends FunSuite {
  private val userId = "149639766382608384"
  private val joinerUserId = "1090123456789012345"
  private val leaderUserId = "209012345678901234"
  private val channelId = "820144843396612127"

  test(
    "external sender package should initialize the pending group and send a fresh replacement key package once"
  ) {
    val actions = (for {
      manager <- DaveSessionManager.create[IO](userId, channelId)
      initial <- manager.onSelectProtocolAck(1)
      firstExternal <- manager.onExternalSenderPackage(externalSenderPayload)
      secondExternal <- manager.onExternalSenderPackage(externalSenderPayload)
      state <- manager.debugState
    } yield (initial, firstExternal, secondExternal, state)).unsafeRunSync()

    val (initial, firstExternal, secondExternal, state) = actions
    val initialKeyPackages = initial.collect {
      case DaveGatewayAction.SendMlsKeyPackage(payload) => payload
    }
    val firstExternalKeyPackages = firstExternal.collect {
      case DaveGatewayAction.SendMlsKeyPackage(payload) => payload
    }
    val secondExternalKeyPackages = secondExternal.collect {
      case DaveGatewayAction.SendMlsKeyPackage(payload) => payload
    }

    assertEquals(initialKeyPackages.length, 1)
    assertEquals(firstExternalKeyPackages.length, 1)
    assert(!initialKeyPackages.head.sameElements(firstExternalKeyPackages.head))
    assertEquals(secondExternalKeyPackages.length, 0)
    assert(state.contains("localGroupInitialized=true"))
    assert(state.contains("keyPackageAfterExternalSender=true"))
  }

  test("prepare epoch one should still send a fresh key package") {
    val actions = (for {
      manager <- DaveSessionManager.create[IO](userId, channelId)
      resent <- manager.onPrepareEpoch(0, 1, 1)
    } yield resent).unsafeRunSync()

    assertEquals(
      actions.count(_.isInstanceOf[DaveGatewayAction.SendMlsKeyPackage]),
      1
    )
  }

  test(
    "init transition zero should not mark protocol one pending group ready"
  ) {
    val isReady = (for {
      manager <- DaveSessionManager.create[IO](userId, channelId)
      _ <- manager.onSelectProtocolAck(1)
      _ <- manager.onExternalSenderPackage(externalSenderPayload)
      _ <- manager.onPrepareTransition(0, 1)
      ready <- manager.isMediaReady
    } yield ready).unsafeRunSync()

    assert(!isReady)
  }

  test(
    "committing initial proposals should wait for gateway transition execution before media is ready"
  ) {
    val ready = (for {
      manager <- DaveSessionManager.create[IO](userId, channelId)
      _ <- manager.onSelectProtocolAck(1)
      joinerState <- DaveProtocol.generateKeyState[IO](joinerUserId)
      externalSigner <- IO.blocking(
        DaveMlsTestHarness.createExternalSigner("gateway".getBytes("UTF-8"))
      )
      _ <- manager.onExternalSenderPackage(
        DaveSupport.GatewayBinaryCodec
          .encode(
            DaveSupport.MlsExternalSenderPackage(1, externalSigner.sender())
          )
          .drop(3)
      )
      proposalBatch <- IO.blocking(
        externalSigner.signedAddProposalBatch(
          channelId,
          DaveProtocol.buildKeyPackageMessageSync(joinerState, joinerUserId)
        )
      )
      actions <- manager.addUsers(List(joinerUserId)) *> manager.onMlsProposals(
        proposalBatch
      )
      isReady <- manager.isMediaReady
      state <- manager.debugState
    } yield (actions, isReady, state)).unsafeRunSync()

    assert(
      ready._1.exists(_.isInstanceOf[DaveGatewayAction.SendMlsCommitWelcome])
    )
    assert(!ready._2)
    assert(ready._3.contains("status=AWAITING_RESPONSE"))
    assert(ready._3.contains("selfRatchet=false"))
  }

  test(
    "committing protocol raw-vector proposals should preserve exact MLS messages"
  ) {
    val results = (for {
      manager <- DaveSessionManager.create[IO](userId, channelId)
      joinerState <- DaveProtocol.generateKeyState[IO](joinerUserId)
      externalSigner <- IO.blocking(
        DaveMlsTestHarness.createExternalSigner("gateway".getBytes("UTF-8"))
      )
      _ <- manager.onSelectProtocolAck(1)
      _ <- manager.onExternalSenderPackage(
        DaveSupport.GatewayBinaryCodec
          .encode(
            DaveSupport.MlsExternalSenderPackage(1, externalSigner.sender())
          )
          .drop(3)
      )
      proposalBatch <- IO.blocking(
        externalSigner.signedRawAddProposalBatch(
          channelId,
          DaveProtocol.buildKeyPackageMessageSync(joinerState, joinerUserId)
        )
      )
      parsed <- IO.fromEither(
        MlsMessages
          .parseProposalBatch(proposalBatch)
          .left
          .map(error => new RuntimeException(error.message))
      )
      actions <- manager.addUsers(List(joinerUserId)) *> manager.onMlsProposals(
        proposalBatch
      )
      state <- manager.debugState
    } yield (parsed, actions, state)).unsafeRunSync()

    assertEquals(results._1.proposals.length, 1)
    assertEquals(results._1.rawProposalMessages.length, 1)
    assert(
      results._2.exists(_.isInstanceOf[DaveGatewayAction.SendMlsCommitWelcome])
    )
    assert(results._3.contains("status=AWAITING_RESPONSE"))
  }

  test("encryptAudio should require the assigned RTP SSRC") {
    val results = (for {
      manager <- activeManagerWithWelcome
      beforeAssign <- manager.encryptAudio(42, Array[Byte](1, 2, 3)).attempt
      _ <- manager.assignAudioSsrc(42)
      assigned <- manager.encryptAudio(42, Array[Byte](1, 2, 3)).attempt
      mismatched <- manager.encryptAudio(43, Array[Byte](1, 2, 3)).attempt
    } yield (beforeAssign, assigned, mismatched)).unsafeRunSync()

    assert(results._1.isLeft)
    assert(results._2.exists(DaveSupport.MediaFrameCodec.isProtocolFrame))
    assert(
      results._3.left.exists(_.getMessage.contains("DAVE audio SSRC mismatch"))
    )
  }

  test(
    "encryptAudio should pass Opus silence frames through without DAVE wrapping"
  ) {
    val encrypted = (for {
      manager <- activeManagerWithWelcome
      _ <- manager.assignAudioSsrc(42)
      frame <- manager.encryptAudioWithSelfCheck(
        42,
        DaveSessionManager.OpusSilenceFrame
      )
    } yield frame).unsafeRunSync()

    assert(!encrypted.daveApplied)
    assert(encrypted.frame.sameElements(DaveSessionManager.OpusSilenceFrame))
    assert(!DaveSupport.MediaFrameCodec.isProtocolFrame(encrypted.frame))
  }

  test(
    "invalid MLS welcome should clear stale media readiness, reset pending state, and request recovery"
  ) {
    val results = (for {
      manager <- activeManagerWithWelcome
      readyBefore <- manager.isMediaReady
      _ <- manager.onPrepareTransition(12, 1)
      actions <- manager.onMlsWelcome(12, Array[Byte](1, 2, 3))
      readyAfter <- manager.isMediaReady
      state <- manager.debugState
    } yield (readyBefore, actions, readyAfter, state)).unsafeRunSync()

    val (readyBefore, actions, readyAfter, state) = results

    assert(readyBefore)
    assertEquals(
      actions.collect {
        case DaveGatewayAction.SendInvalidCommitWelcome(transitionId) =>
          transitionId
      },
      List(12)
    )
    assertEquals(
      actions.count(_.isInstanceOf[DaveGatewayAction.SendMlsKeyPackage]),
      1
    )
    assert(!readyAfter)
    assert(state.contains("selfRatchet=false"))
    assert(state.contains("mediaRatchetActive=false"))
    assert(state.contains("pendingTransitions=[]"))
    assert(state.contains("pendingProposals=0"))
    assert(state.contains("localGroupInitialized=true"))
    assert(state.contains("keyPackageAfterExternalSender=false"))
  }

  test(
    "invalid MLS commit transition should clear stale media readiness, reset pending state, and request recovery"
  ) {
    val results = (for {
      manager <- activeManagerWithWelcome
      readyBefore <- manager.isMediaReady
      _ <- manager.onPrepareTransition(12, 1)
      actions <- manager.onMlsCommitTransition(12, Array[Byte](1, 2, 3))
      readyAfter <- manager.isMediaReady
      state <- manager.debugState
    } yield (readyBefore, actions, readyAfter, state)).unsafeRunSync()

    val (readyBefore, actions, readyAfter, state) = results

    assert(readyBefore)
    assertEquals(
      actions.collect {
        case DaveGatewayAction.SendInvalidCommitWelcome(transitionId) =>
          transitionId
      },
      List(12)
    )
    assertEquals(
      actions.count(_.isInstanceOf[DaveGatewayAction.SendMlsKeyPackage]),
      1
    )
    assert(!readyAfter)
    assert(state.contains("selfRatchet=false"))
    assert(state.contains("mediaRatchetActive=false"))
    assert(state.contains("pendingTransitions=[]"))
    assert(state.contains("pendingProposals=0"))
    assert(state.contains("localGroupInitialized=true"))
    assert(state.contains("keyPackageAfterExternalSender=false"))
  }

  test(
    "media readiness should require the executed transition to match the welcome ratchet transition"
  ) {
    val results = (for {
      manager <- DaveSessionManager.create[IO](userId, channelId)
      leaderState <- DaveProtocol.generateKeyState[IO](leaderUserId)
      externalSigner <- IO.blocking(
        DaveMlsTestHarness.createExternalSigner("gateway".getBytes("UTF-8"))
      )
      _ <- manager.onSelectProtocolAck(1)
      replacementActions <- manager.onExternalSenderPackage(
        DaveSupport.GatewayBinaryCodec
          .encode(
            DaveSupport.MlsExternalSenderPackage(1, externalSigner.sender())
          )
          .drop(3)
      )
      joinerKeyPackage <- IO.fromOption(
        replacementActions.collectFirst {
          case DaveGatewayAction.SendMlsKeyPackage(payload) => payload
        }
      )(
        new RuntimeException(
          "Expected replacement DAVE key package after external sender"
        )
      )
      proposalBatch <- IO.blocking(
        externalSigner.signedAddProposalBatch(channelId, joinerKeyPackage)
      )
      welcome <- IO.fromEither(
        DaveProtocol
          .buildCommitWelcomeFromProposals(
            state = leaderState,
            selfUserId = leaderUserId,
            groupId = channelId,
            externalSender = Some(externalSigner.sender()),
            proposals = List(proposalBatch),
            recognizedUserIds = Set(userId, leaderUserId)
          )
          .flatMap(
            _.flatMap(_.welcomeMessage)
              .toRight("Expected MLS welcome for joiner")
          )
          .left
          .map(new RuntimeException(_))
      )
      _ <- manager.onMlsWelcome(12, welcome)
      readyBeforeExecute <- manager.isMediaReady
      _ <- manager.onPrepareTransition(13, 1)
      _ <- manager.onExecuteTransition(13)
      readyAfterWrongExecute <- manager.isMediaReady
      _ <- manager.onExecuteTransition(12)
      readyAfterMatchingExecute <- manager.isMediaReady
    } yield (
      readyBeforeExecute,
      readyAfterWrongExecute,
      readyAfterMatchingExecute
    )).unsafeRunSync()

    assertEquals(results, (false, false, true))
  }

  private def activeManagerWithWelcome: IO[DaveSessionManager[IO]] =
    for {
      manager <- DaveSessionManager.create[IO](userId, channelId)
      leaderState <- DaveProtocol.generateKeyState[IO](leaderUserId)
      externalSigner <- IO.blocking(
        DaveMlsTestHarness.createExternalSigner("gateway".getBytes("UTF-8"))
      )
      _ <- manager.onSelectProtocolAck(1)
      replacementActions <- manager.onExternalSenderPackage(
        DaveSupport.GatewayBinaryCodec
          .encode(
            DaveSupport.MlsExternalSenderPackage(1, externalSigner.sender())
          )
          .drop(3)
      )
      joinerKeyPackage <- IO.fromOption(
        replacementActions.collectFirst {
          case DaveGatewayAction.SendMlsKeyPackage(payload) => payload
        }
      )(
        new RuntimeException(
          "Expected replacement DAVE key package after external sender"
        )
      )
      proposalBatch <- IO.blocking(
        externalSigner.signedAddProposalBatch(channelId, joinerKeyPackage)
      )
      welcome <- IO.fromEither(
        DaveProtocol
          .buildCommitWelcomeFromProposals(
            state = leaderState,
            selfUserId = leaderUserId,
            groupId = channelId,
            externalSender = Some(externalSigner.sender()),
            proposals = List(proposalBatch),
            recognizedUserIds = Set(userId, leaderUserId)
          )
          .flatMap(
            _.flatMap(_.welcomeMessage)
              .toRight("Expected MLS welcome for joiner")
          )
          .left
          .map(new RuntimeException(_))
      )
      _ <- manager.onMlsWelcome(12, welcome)
      _ <- manager.onExecuteTransition(12)
    } yield manager

  private def externalSenderPayload: Array[Byte] =
    DaveSupport.GatewayBinaryCodec
      .encode(
        DaveSupport.MlsExternalSenderPackage(
          sequenceNumber = 1,
          externalSender = DaveSupport.ExternalSender(
            signatureKey = Array.fill[Byte](65)(1),
            credential = DaveSupport.Credential(
              credentialType = 1,
              identity = "gateway".getBytes("UTF-8")
            )
          )
        )
      )
      .drop(3)
}
