import Dependencies._
import WartRemoverSettings._
import sbt.Keys._

import scala.language.postfixOps
import scala.sys.process._

ThisBuild / organization := "com.github.mercurievv"
ThisBuild / scalaVersion := "3.2.2"
ThisBuild / version := sys.env.getOrElse("VERSION", "LOCAL")
ThisBuild / autoCompilerPlugins := true
ThisBuild / scalacOptions := Seq(
  "-Ykind-projector:underscores", // turns on the internal kind projector in Scala 3
  "-source:future",
  "-feature",
  "-deprecation",
  "-rewrite",
  "-indent",
  "-Xmax-inlines:55",
  "-unchecked",
  "-deprecation",
  "-language:higherKinds",
  "-language:existentials",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-encoding",
  "utf8",
  "-Xfatal-warnings",
  s"-P:wartremover:excluded:${sourceManaged.value.asFile.getPath}",
)
ThisBuild / pushRemoteCacheTo := Some(MavenCache("local-cache", (ThisBuild / baseDirectory).value / "local-cache-tmp"))
fork := true

val commonSettings = Seq(
  Compile / compile / wartremoverErrors ++= Warts.allBut(wartExclusionsMain: _*),
  Compile / test / wartremoverErrors ++= Warts.allBut(wartExclusionsTest: _*),
  libraryDependencies += "com.disneystreaming" %% "weaver-cats" % "0.8.3" % Test,
  testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
)

lazy val root = (project in file("app"))
  .enablePlugins(WartRemover)
  .settings(commonSettings)
  .settings(
    name := "s4snosj"
  )
  .settings(
    libraryDependencies ++= Seq(
      Cats.Effect,
      Fs2.core,
      Fs2.io,
      Logging.Logback,
      Logging.Slf4jApi,
      Testing.magnolia,
      Testing.scalacheck,
      Testing.catsScalacheck,
      Testing.weaver,
      Testing.weaverCats,
      Testing.weaverScalacheck,
    ) ++ Circe.All
  )
