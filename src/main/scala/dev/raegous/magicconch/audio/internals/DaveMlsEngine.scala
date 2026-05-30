package dev.raegous.magicconch.audio.internals

import java.lang.reflect.{Field, InvocationTargetException}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.security.SecureRandom
import java.util.{ArrayList, Arrays, HashMap, List => JList}
import scala.annotation.tailrec

final class DaveMlsEngine private (
  selfUserId: String,
  suite: AnyRef,
  leafKeyPair: AnyRef,
  initKeyPair: AnyRef,
  signaturePrivateKey: Array[Byte],
  leafNode: AnyRef,
  keyPackage: AnyRef
) {
  private val random = new SecureRandom()

  private var group: AnyRef = null
  private var pendingOutboundGroup: AnyRef = null
  private var lastCommitMessage: Array[Byte] = null

  def keyPackageMessage(): Array[Byte] =
    DaveMlsEngine.encode(keyPackage)

  def keyPackageMlsMessage(): Array[Byte] =
    DaveMlsEngine.encode(DaveMlsEngine.staticInvoke("org.bouncycastle.mls.codec.MLSMessage", "keyPackage", keyPackage))

  def initializeLocalGroup(groupId: String, externalSender: DaveSupport.ExternalSender): Unit =
    initializeLocalGroup(groupId.getBytes(StandardCharsets.UTF_8), externalSender)

  def initializeLocalGroup(groupId: Array[Byte], externalSender: DaveSupport.ExternalSender): Unit = {
    group = DaveMlsEngine.newInstance(
      "org.bouncycastle.mls.protocol.Group",
      groupId,
      suite,
      leafKeyPair,
      signaturePrivateKey,
      leafNode,
      externalSenderExtension(externalSender)
    )
    pendingOutboundGroup = null
    lastCommitMessage = null
  }

  def cacheProposal(messageBytes: Array[Byte]): Unit =
    DaveMlsEngine.invoke(requireGroup(), "handle", messageBytes, null)

  def revokeProposal(ref: Array[Byte]): Unit =
    Option(group).foreach { activeGroup =>
      val field = DaveMlsEngine.classFor("org.bouncycastle.mls.protocol.Group").getDeclaredField("pendingProposals")
      field.setAccessible(true)
      val pending = field.get(activeGroup).asInstanceOf[JList[Object]]
      pending.removeIf { cached =>
        try {
          val refField = cached.getClass.getDeclaredField("proposalRef")
          refField.setAccessible(true)
          Arrays.equals(refField.get(cached).asInstanceOf[Array[Byte]], ref)
        } catch {
          case e: ReflectiveOperationException => throw new RuntimeException(e)
        }
      }
      ()
    }

  def commitPending(includeWelcome: Boolean): DaveSupport.MlsCommitWelcome = {
    val commit = DaveMlsEngine.invoke(
      requireGroup(),
      "commit",
      newSecret(randomBytes(hashLength)),
      DaveMlsEngine.newInstance(
        "org.bouncycastle.mls.protocol.Group$CommitOptions",
        new ArrayList[AnyRef](),
        java.lang.Boolean.TRUE,
        java.lang.Boolean.FALSE,
        DaveMlsEngine.newInstance("org.bouncycastle.mls.protocol.Group$LeafNodeOptions")
      ),
      DaveMlsEngine.newInstance("org.bouncycastle.mls.protocol.Group$MessageOptions", java.lang.Boolean.FALSE, Array.emptyByteArray, Integer.valueOf(0)),
      DaveMlsEngine.newInstance("org.bouncycastle.mls.protocol.Group$CommitParameters", java.lang.Short.valueOf(normalCommitParams))
    )
    val commitMessage = DaveMlsEngine.fieldValue[AnyRef](commit, "message")

    pendingOutboundGroup = DaveMlsEngine.fieldValue[AnyRef](commit, "group")
    val commitBytes = DaveMlsEngine.encode(commitMessage)
    lastCommitMessage = commitBytes.clone()

    val welcome = Option(DaveMlsEngine.fieldValue[AnyRef](commitMessage, "welcome"))
      .filter(_ => includeWelcome)
      .map(DaveMlsEngine.encode)

    DaveSupport.MlsCommitWelcome(commitBytes, welcome)
  }

  def processCommit(commitMessage: Array[Byte]): DaveSupport.SenderKeyRatchet = {
    Option(lastCommitMessage)
      .filter(Arrays.equals(_, commitMessage))
      .fold {
        group = DaveMlsEngine.invoke(
          requireGroup(),
          "handle",
          commitMessage,
          null,
          DaveMlsEngine.newInstance("org.bouncycastle.mls.protocol.Group$CommitParameters", java.lang.Short.valueOf(normalCommitParams))
        )
      } { _ =>
        group = Option(pendingOutboundGroup).getOrElse(
          throw new IllegalStateException("DAVE own commit was announced without a pending outbound group")
        )
      }
    pendingOutboundGroup = null
    lastCommitMessage = null
    selfSenderRatchet()
  }

  def processWelcome(welcomeMessageBytes: Array[Byte]): DaveSupport.SenderKeyRatchet = {
    val welcome = decodeWelcome(welcomeMessageBytes)

    group = DaveMlsEngine.newInstance(
      "org.bouncycastle.mls.protocol.Group",
      DaveMlsEngine.invoke(hpke, "serializePrivateKey", DaveMlsEngine.invoke(initKeyPair, "getPrivate")),
      leafKeyPair,
      signaturePrivateKey,
      keyPackage,
      welcome,
      null,
      new HashMap[AnyRef, Array[Byte]](),
      new HashMap[AnyRef, Array[Byte]]()
    )
    pendingOutboundGroup = null
    lastCommitMessage = null
    selfSenderRatchet()
  }

  def selfSenderRatchet(): DaveSupport.SenderKeyRatchet =
    new DaveSupport.HkdfSenderKeyRatchet(senderBaseSecretFor(selfUserId))

  def senderBaseSecretFor(userId: String): Array[Byte] =
    DaveMlsEngine.invoke(
      DaveMlsEngine.invoke(requireGroup(), "getKeySchedule"),
      "MLSExporter",
      "Discord Secure Frames v0",
      DaveMlsEngine.littleEndianUInt64(java.lang.Long.parseUnsignedLong(userId)),
      Integer.valueOf(DaveMlsEngine.DaveMediaBaseSecretLength)
    ).asInstanceOf[Array[Byte]]

  private def decodeWelcome(welcomeBytes: Array[Byte]): AnyRef =
    isWrappedWelcome(welcomeBytes) match {
      case true =>
        val message = DaveMlsEngine.decode(welcomeBytes, "org.bouncycastle.mls.codec.MLSMessage")
        Either.cond(
          String.valueOf(DaveMlsEngine.fieldValue[AnyRef](message, "wireFormat")) == "mls_welcome",
          DaveMlsEngine.fieldValue[AnyRef](message, "welcome"),
          s"Expected MLS welcome, got ${DaveMlsEngine.fieldValue[AnyRef](message, "wireFormat")}"
        ).fold(error => throw new IllegalArgumentException(error), identity)

      case false =>
        DaveMlsEngine.decode(welcomeBytes, "org.bouncycastle.mls.codec.Welcome")
    }

  private def isWrappedWelcome(welcomeBytes: Array[Byte]): Boolean =
    welcomeBytes.length >= 4 &&
      welcomeBytes(0) == 0.toByte &&
      welcomeBytes(1) == 1.toByte &&
      welcomeBytes(2) == 0.toByte &&
      welcomeBytes(3) == 3.toByte

  private def requireGroup(): AnyRef =
    Option(group).getOrElse(throw new IllegalStateException("MLS group has not been initialized"))

  private def externalSenderExtension(sender: DaveSupport.ExternalSender): JList[AnyRef] = {
    val senders = new ArrayList[AnyRef]()
    senders.add(DaveMlsEngine.newInstance(
      "org.bouncycastle.mls.codec.ExternalSender",
      sender.signatureKey,
      DaveMlsEngine.staticInvoke("org.bouncycastle.mls.codec.Credential", "forBasic", sender.credential.identity)
    ))

    val extensions = new ArrayList[AnyRef]()
    extensions.add(DaveMlsEngine.staticInvoke("org.bouncycastle.mls.codec.Extension", "externalSender", senders))
    extensions
  }

  private def hashLength: Int =
    DaveMlsEngine.invoke(DaveMlsEngine.invoke(suite, "getKDF"), "getHashLength").asInstanceOf[Int]

  private def normalCommitParams: Short =
    DaveMlsEngine.classFor("org.bouncycastle.mls.protocol.Group").getField("NORMAL_COMMIT_PARAMS").getShort(null)

  private def hpke: AnyRef =
    DaveMlsEngine.invoke(suite, "getHPKE")

  private def newSecret(bytes: Array[Byte]): AnyRef =
    DaveMlsEngine.newInstance("org.bouncycastle.mls.crypto.Secret", bytes)

  private def randomBytes(length: Int): Array[Byte] = {
    val bytes = new Array[Byte](length)
    random.nextBytes(bytes)
    bytes
  }
}

object DaveMlsEngine {
  private val DaveSuiteId: Short = 0x0002.toShort
  private val DaveMediaBaseSecretLength = 16

  def create(selfUserId: String): DaveMlsEngine = {
    @tailrec
    def retry(attempt: Int, last: Option[ArrayIndexOutOfBoundsException]): DaveMlsEngine =
      attempt >= 8 match {
        case true => throw last.getOrElse(new ArrayIndexOutOfBoundsException("DAVE MLS key package creation failed"))
        case false =>
          try createOnce(selfUserId)
          catch {
            case e: ArrayIndexOutOfBoundsException => retry(attempt + 1, Some(e))
          }
      }

    retry(0, None)
  }

  private def createOnce(selfUserId: String): DaveMlsEngine = {
    val suite = staticInvoke("org.bouncycastle.mls.crypto.MlsCipherSuite", "getSuite", java.lang.Short.valueOf(DaveSuiteId))
    val signingKeyPair = invoke(suite, "generateSignatureKeyPair")
    val hpke = invoke(suite, "getHPKE")
    val leafKeyPair = invoke(hpke, "generatePrivateKey")
    val initKeyPair = invoke(hpke, "generatePrivateKey")
    val signaturePrivateKey = invoke(suite, "serializeSignaturePrivateKey", invoke(signingKeyPair, "getPrivate")).asInstanceOf[Array[Byte]]

    val leafNode = newInstance(
      "org.bouncycastle.mls.TreeKEM.LeafNode",
      suite,
      invoke(hpke, "serializePublicKey", invoke(leafKeyPair, "getPublic")),
      invoke(suite, "serializeSignaturePublicKey", invoke(signingKeyPair, "getPublic")),
      staticInvoke("org.bouncycastle.mls.codec.Credential", "forBasic", discordUserIdIdentity(selfUserId)),
      daveCapabilities(),
      newInstance("org.bouncycastle.mls.TreeKEM.LifeTime"),
      new ArrayList[AnyRef](),
      signaturePrivateKey
    )

    val keyPackage = newInstance(
      "org.bouncycastle.mls.codec.KeyPackage",
      suite,
      invoke(hpke, "serializePublicKey", invoke(initKeyPair, "getPublic")),
      leafNode,
      new ArrayList[AnyRef](),
      signaturePrivateKey
    )

    new DaveMlsEngine(selfUserId, suite, leafKeyPair, initKeyPair, signaturePrivateKey, leafNode, keyPackage)
  }

  private def daveCapabilities(): AnyRef = {
    val capabilities = newInstance("org.bouncycastle.mls.codec.Capabilities")
    setShortList(capabilities, "versions", 0x0001.toShort)
    setShortList(capabilities, "cipherSuites", DaveSuiteId)
    setShortList(capabilities, "extensions")
    setShortList(capabilities, "proposals")
    setShortList(capabilities, "credentials", 0x0001.toShort)
    capabilities
  }

  private def setShortList(capabilities: AnyRef, fieldName: String, values: Short*): Unit = {
    val field: Field = classFor("org.bouncycastle.mls.codec.Capabilities").getDeclaredField(fieldName)
    field.setAccessible(true)
    val list = new ArrayList[java.lang.Short]()
    values.foreach(value => list.add(java.lang.Short.valueOf(value)))
    field.set(capabilities, list)
  }

  private def encode(value: AnyRef): Array[Byte] =
    staticInvoke("org.bouncycastle.mls.codec.MLSOutputStream", "encode", value).asInstanceOf[Array[Byte]]

  private def decode(bytes: Array[Byte], className: String): AnyRef =
    staticInvoke("org.bouncycastle.mls.codec.MLSInputStream", "decode", bytes, classFor(className))

  private def classFor(name: String): Class[?] =
    Class.forName(name)

  private def staticInvoke(className: String, methodName: String, args: Any*): AnyRef =
    invoke(classFor(className), null, methodName, args*)

  private def invoke(target: AnyRef, methodName: String, args: Any*): AnyRef =
    invoke(target.getClass, target, methodName, args*)

  private def invoke(targetClass: Class[?], target: AnyRef, methodName: String, args: Any*): AnyRef = {
    val method = targetClass.getMethods
      .find(method => method.getName == methodName && parametersMatch(method.getParameterTypes.toList, args.toList))
      .getOrElse(throw new NoSuchMethodException(s"${targetClass.getName}.$methodName/${args.length}"))
    try method.invoke(target, args.map(_.asInstanceOf[AnyRef])*)
    catch {
      case e: InvocationTargetException => throw e.getTargetException
    }
  }

  private def newInstance(className: String, args: Any*): AnyRef = {
    val targetClass = classFor(className)
    val constructor = targetClass.getConstructors
      .find(constructor => parametersMatch(constructor.getParameterTypes.toList, args.toList))
      .getOrElse(throw new NoSuchMethodException(s"$className.<init>/${args.length}"))
    try constructor.newInstance(args.map(_.asInstanceOf[AnyRef])*)
    catch {
      case e: InvocationTargetException => throw e.getTargetException
    }
  }

  private def parametersMatch(parameterTypes: List[Class[?]], args: List[Any]): Boolean =
    parameterTypes.length == args.length && parameterTypes.zip(args).forall { case (parameterType, arg) => parameterMatches(parameterType, arg) }

  private def parameterMatches(parameterType: Class[?], arg: Any): Boolean =
    Option(arg).fold(!parameterType.isPrimitive) { value =>
      boxedClass(parameterType).isAssignableFrom(value.asInstanceOf[AnyRef].getClass)
    }

  private def boxedClass(parameterType: Class[?]): Class[?] =
    Map[Class[?], Class[?]](
      java.lang.Boolean.TYPE -> classOf[java.lang.Boolean],
      java.lang.Byte.TYPE -> classOf[java.lang.Byte],
      java.lang.Character.TYPE -> classOf[java.lang.Character],
      java.lang.Double.TYPE -> classOf[java.lang.Double],
      java.lang.Float.TYPE -> classOf[java.lang.Float],
      java.lang.Integer.TYPE -> classOf[java.lang.Integer],
      java.lang.Long.TYPE -> classOf[java.lang.Long],
      java.lang.Short.TYPE -> classOf[java.lang.Short]
    ).getOrElse(parameterType, parameterType)

  private def fieldValue[A](target: AnyRef, fieldName: String): A = {
    val field = target.getClass.getDeclaredField(fieldName)
    field.setAccessible(true)
    field.get(target).asInstanceOf[A]
  }

  private def discordUserIdIdentity(userId: String): Array[Byte] =
    ByteBuffer.allocate(8).putLong(java.lang.Long.parseUnsignedLong(userId)).array()

  private def littleEndianUInt64(value: Long): Array[Byte] =
    Array.tabulate(8)(index => ((value >>> (8 * index)) & 0xFF).toByte)
}
