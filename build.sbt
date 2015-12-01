import sbt.Project.projectToRef

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

lazy val clients = Seq(opetopicSketchpad, opetopicCodebuilder)

lazy val opetopicPlay = (project in file("opetopic-play")).
  settings(commonSettings: _*).
  settings(
    scalaJSProjects := clients,
    pipelineStages := Seq(scalaJSProd),
    JsEngineKeys.engineType := JsEngineKeys.EngineType.Node,
    includeFilter in (Assets, LessKeys.less) := "opetopic.less",
    libraryDependencies ++= Seq(
      "org.webjars" %% "webjars-play" % "2.4.0-1",
      "org.webjars" % "jquery" % "2.1.4",
      "org.webjars" % "Semantic-UI" % "2.1.6",
      "org.webjars" % "codemirror" % "5.8"
    ),
    herokuAppName in Compile := "opetopic",
    herokuSkipSubProjects in Compile := false
  ).enablePlugins(PlayScala).
  aggregate(clients.map(projectToRef): _*).
  dependsOn(opetopicCoreJvm)

lazy val opetopicCodebuilder = (project in file("opetopic-codebuilder")).
  settings(commonSettings: _*).
  settings(
    persistLauncher := true,
    unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.8.1",
      "be.doeraene" %%% "scalajs-jquery" % "0.8.0",
      "com.lihaoyi" %%% "scalatags" % "0.5.3",
      "org.denigma" %%% "codemirror-facade" % "5.4-0.5"
    )
  ).enablePlugins(ScalaJSPlugin).
  dependsOn(opetopicJs)

lazy val opetopicSketchpad = (project in file("opetopic-sketchpad")).
  settings(commonSettings: _*).
  settings(
    persistLauncher := true,
    unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
    resolvers += sbt.Resolver.bintrayRepo("denigma", "denigma-releases"),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.8.1",
      "be.doeraene" %%% "scalajs-jquery" % "0.8.0",
      "com.lihaoyi" %%% "scalatags" % "0.5.3",
      "org.denigma" %%% "codemirror-facade" % "5.4-0.5"
    )
  ).enablePlugins(ScalaJSPlugin).
  dependsOn(opetopicJs)

lazy val opetopicJs = (project in file("opetopic-js")).
  settings(commonSettings: _*).
  settings(
    persistLauncher := true,
    unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.8.1",
      "be.doeraene" %%% "scalajs-jquery" % "0.8.0",
      "com.lihaoyi" %%% "scalatags" % "0.5.3"
    )
  ).enablePlugins(ScalaJSPlugin).
  dependsOn(opetopicCoreJs)

lazy val opetopicCore = (crossProject.crossType(CrossType.Pure) in file("opetopic-core")).
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "scalatags" % "0.5.3"
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


