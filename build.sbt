ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0"

ThisBuild / javaHome := {
  val candidates = Seq(
    sys.env.get("JAVA_HOME").map(file),
    Some(file("C:/Program Files/Eclipse Adoptium/jdk-17.0.14.7-hotspot")),
    Some(file("C:/Program Files/Eclipse Adoptium/jdk-8.0.412.8-hotspot"))
  ).flatten

  candidates.find(jdk => (jdk / "bin" / "javac.exe").exists)
}

lazy val root = (project in file("."))
  .settings(
    name := "magicconch",
    scalacOptions ++= Seq("-Xmax-inlines", "64", "-language:implicitConversions"),
    Test / fork := true,
    assembly / assemblyMergeStrategy := {
      case "META-INF/MANIFEST.MF" => MergeStrategy.discard
      case "META-INF/services/org.apache.logging.log4j.spi.Provider" => MergeStrategy.first
      case x if x.endsWith(".class") => MergeStrategy.first
      case x if x.endsWith(".properties") => MergeStrategy.first
      case x if x.contains("module-info") => MergeStrategy.discard
      case _ => MergeStrategy.first
    },
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.6.3",
      "org.http4s" %% "http4s-ember-client" % "0.23.33",
      "org.http4s" %% "http4s-ember-server" % "0.23.33",
      "org.http4s" %% "http4s-dsl" % "0.23.33",
      "org.http4s" %% "http4s-circe" % "0.23.33",
      "com.softwaremill.sttp.client4" %% "core" % "4.0.13",
      "com.softwaremill.sttp.client4" %% "fs2" % "4.0.13",
      "io.circe" %% "circe-core" % "0.14.15",
      "io.circe" %% "circe-generic" % "0.14.15",
      "io.circe" %% "circe-parser" % "0.14.15",
      "ch.qos.logback" % "logback-classic" % "1.5.15",
      "org.typelevel" %% "log4cats-slf4j" % "2.7.1",
      "com.github.pureconfig" %% "pureconfig-core" % "0.17.9",
      "org.java-websocket" % "Java-WebSocket" % "1.6.0",
      "co.fs2" %% "fs2-core" % "3.12.2",
      "io.github.jaredmdobson" % "concentus" % "1.0.2",
      "org.purejava" % "tweetnacl-java" % "1.1.2",  // xsalsa20_poly1305 encryption (legacy)
      "com.goterl" % "lazysodium-java" % "5.1.4",   // xchacha20_poly1305 AEAD encryption for Discord voice
      "net.java.dev.jna" % "jna" % "5.13.0",         // Required by lazysodium
      "org.bouncycastle" % "bcmls-jdk18on" % "1.81", // RFC 9420 MLS state machine for our in-repo DAVE layer
      "org.scodec" %% "scodec-core" % "2.2.2",       // Binary encoding/decoding
      "org.scodec" %% "scodec-cats" % "1.2.0",       // Cats integration for scodec
      "org.scalameta" %% "munit" % "1.0.0" % Test,
      "org.typelevel" %% "munit-cats-effect" % "2.1.0" % Test,
      "net.java.dev.jna" % "jna-platform" % "5.13.0" % Test,
      "com.bdmendes" %% "smockito" % "2.2.1" % Test,
    )
  )
