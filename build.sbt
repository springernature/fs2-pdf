val github = "https://github.com"
val org = "springernature"
val projectName = "fs2-pdf"
val repoPath = s"$org/$projectName"
val repo = s"$github/$repoPath"
val http4sVersion = "0.21.0-RC3"
val circeVersion = "0.12.3"
val specs2Version = "4.6.0"
val logbackVersion = "1.2.3"
val doobieVersion = "0.8.6"
val tapirVersion = "0.12.12"
val log4catsVersion = "1.0.0"

name := "fs2-pdf"
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0")
libraryDependencies ++= List(
  "org.scodec" %% "scodec-stream" % "2.0.0",
  "org.scodec" %% "scodec-cats" % "1.0.0",
  "io.chrisdavenport" %% "log4cats-noop" % log4catsVersion,
  "io.chrisdavenport" %% "log4cats-slf4j" % log4catsVersion,
  "co.fs2" %% "fs2-io" % "2.2.2" % Test,
)

ThisBuild / organization := "com.springernature"
ThisBuild / scalaVersion := "2.13.1"
ThisBuild / libraryDependencies += "org.specs2" %% "specs2-core" % specs2Version % Test
ThisBuild / scalacOptions ++= List(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-language:higherKinds",
  "-Ywarn-value-discard",
  "-Ywarn-unused:imports",
  "-Ywarn-unused:implicits",
  "-Ywarn-unused:params",
  "-Ywarn-unused:patvars",
)
ThisBuild / fork := true
ThisBuild / licenses := List("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage := Some(url(repo))
ThisBuild / scmInfo := Some(ScmInfo(url(repo), s"scm:git@github.com:$repoPath"))
ThisBuild / developers := List(Developer(
  id = "springfield",
  name = "Springfield",
  email = "spring-field@springernature.com",
  url = url(s"$github/$org"),
))
ThisBuild / update / evictionWarningOptions := EvictionWarningOptions.default.withWarnTransitiveEvictions(false)

import ReleaseTransformations._

releaseProcess := Seq[ReleaseStep](
  inquireVersions,
  runClean,
  setReleaseVersion,
  releaseStepCommand("publish"),
  // releaseStepCommandAndRemaining("publish"),
  // releaseStepCommand("sonatypeReleaseAll"),
  tagRelease,
  setNextVersion,
  commitNextVersion,
)
