import sbt.Project.projectToRef

val commonSettings = Seq(
  organization := "opetopic",
  homepage := Some(url("http://ericfinster.github.io")),
  scalaVersion := "2.11.6",
  scalacOptions ++= Seq(
    "-language:higherKinds",
    "-language:implicitConversions",
    "-feature",
    "-deprecation"
  ),
  resolvers += Resolver.sonatypeRepo("releases"),
  resolvers += "bintray/non" at "http://dl.bintray.com/non/maven",
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.5.4"),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full),
  initialCommands in console := 
    """
       import opetopic._
    """
)

lazy val clients = Seq(opetopicJs)

lazy val opetopicPlay = (project in file("opetopic-play")).
  settings(commonSettings: _*).
  settings(
    scalaJSProjects := clients,
    pipelineStages := Seq(scalaJSProd),
    libraryDependencies ++= Seq(
      "com.vmunier" %% "play-scalajs-scripts" % "0.2.1"
    )
  ).enablePlugins(PlayScala).
  aggregate(clients.map(projectToRef): _*).
  dependsOn(opetopicCoreJvm)

lazy val opetopicJs = (project in file("opetopic-js")).
  settings(commonSettings: _*).
  settings(
    persistLauncher := true,
    sourceMapsDirectories += opetopicCoreJs.base / "..",
    unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.8.0"
    )
  ).enablePlugins(ScalaJSPlugin, ScalaJSPlay).
  dependsOn(opetopicCoreJs)

lazy val opetopicFx = (project in file("opetopic-fx")).
  settings(commonSettings: _*).
  settings(
    fork := true,
    mainClass in (Compile, run) := Some("opetopic.fx.FXEditor"),
    libraryDependencies ++= Seq(
      "org.scalafx" %% "scalafx" % "8.0.40-R8"
    )
  ).dependsOn(opetopicCoreJvm)

lazy val opetopicCore = (crossProject.crossType(CrossType.Pure) in file("opetopic-core")).
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "scalatags" % "0.5.1",
      "com.lihaoyi" %%% "upickle" % "0.2.8"
    )
  ).
  jsConfigure(_ enablePlugins ScalaJSPlay).
  jsSettings(
    libraryDependencies ++= Seq(
      "com.github.japgolly.fork.scalaz" %%% "scalaz-core" % "7.1.1-2"
    ),
    sourceMapsBase := baseDirectory.value / ".."
  ).jvmSettings(
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.1.1"
    )
  ).dependsOn(opetopicMacros)

lazy val opetopicCoreJvm = opetopicCore.jvm
lazy val opetopicCoreJs = opetopicCore.js

lazy val opetopicMacros = (crossProject.crossType(CrossType.Pure) in file("opetopic-macros")).
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
  ).
  jsConfigure(_ enablePlugins ScalaJSPlay).
  jsSettings(
    libraryDependencies ++= Seq(
      "com.github.japgolly.fork.scalaz" %%% "scalaz-core" % "7.1.1-2"
    ),
    sourceMapsBase := baseDirectory.value / ".."
  ).jvmSettings(
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.1.1"
    )
  )

lazy val opetopicMacrosJvm = opetopicMacros.jvm
lazy val opetopicMacrosJs = opetopicMacros.js





