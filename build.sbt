ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0"

lazy val root = (project in file("."))
  .settings(
    name := "magicconch",
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
      "org.http4s" %% "http4s-circe" % "0.23.33",
      "com.softwaremill.sttp.client4" %% "core" % "4.0.13",
      "com.softwaremill.sttp.client4" %% "fs2" % "4.0.13",
      "io.circe" %% "circe-core" % "0.14.15",
      "io.circe" %% "circe-generic" % "0.14.15",
      "io.circe" %% "circe-parser" % "0.14.15",
      "org.slf4j" % "slf4j-simple" % "2.0.17",
      "org.typelevel" %% "log4cats-slf4j" % "2.7.1",
      "com.github.pureconfig" %% "pureconfig-core" % "0.17.9",
      "org.java-websocket" % "Java-WebSocket" % "1.6.0",
      "co.fs2" %% "fs2-core" % "3.12.2",
      "io.github.jaredmdobson" % "concentus" % "1.0.2"
    )
  )