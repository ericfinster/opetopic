
val commonSettings = Seq(
  organization := "opetopic",
  homepage := Some(url("http://ericfinster.github.io")),
  scalaVersion := "2.11.7",
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

lazy val opetopicJs = (project in file("opetopic-js")).
  settings(commonSettings: _*).
  settings(
    persistLauncher := true,
    unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.8.1",
      "be.doeraene" %%% "scalajs-jquery" % "0.8.0"
    ),
    jsDependencies ++= Seq(
      "org.webjars" % "jquery" % "2.1.4" / "jquery.js",
      "org.webjars" % "Semantic-UI" % "2.1.6" / "semantic.js"
    ),
    skip in packageJSDependencies := false
  ).enablePlugins(ScalaJSPlugin, SbtWeb).
  dependsOn(opetopicCoreJs)

lazy val opetopicFx = (project in file("opetopic-fx")).
  settings(commonSettings: _*).
  settings(
    fork := true,
    mainClass in (Compile, run) := Some("opetopic.fx.FXEditor"),
    libraryDependencies ++= Seq(
      "org.scalafx" %% "scalafx" % "8.0.40-R8",
      "com.lihaoyi" %% "scalatags" % "0.5.2"
    )
  ).dependsOn(opetopicCoreJvm)

lazy val opetopicCore = (crossProject.crossType(CrossType.Pure) in file("opetopic-core")).
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "upickle" % "0.2.8",
      "com.lihaoyi" %%% "scalatags" % "0.5.2"
    )
  ).
  jsSettings(
    libraryDependencies ++= Seq(
      "com.github.japgolly.fork.scalaz" %%% "scalaz-core" % "7.1.3",
      "org.scala-js" %%% "scala-parser-combinators" % "1.0.2"
    )
  ).jvmSettings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
      "org.scalaz" %% "scalaz-core" % "7.1.3"
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
  jsSettings(
    libraryDependencies ++= Seq(
      "com.github.japgolly.fork.scalaz" %%% "scalaz-core" % "7.1.3"
    )
  ).jvmSettings(
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.1.3"
    )
  )

lazy val opetopicMacrosJvm = opetopicMacros.jvm
lazy val opetopicMacrosJs = opetopicMacros.js


