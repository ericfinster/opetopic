import sbt.Project.projectToRef

val scalaCompilerVersion = "2.11.8"
val scalaJsDomVersion = "0.9.0"
val scalaJsJQueryVersion = "0.9.0"
val scalatagsVersion = "0.6.0"
val upickleVersion = "0.4.1"
val fastparseVersion = "0.4.1"
val codeMirrorVersion = "5.13.2"
val codeMirrorFacadeVersion = "5.11-0.7"

val commonSettings = Seq(
  organization := "opetopic",
  homepage := Some(url("http://opetopic.net")),
  scalaVersion := scalaCompilerVersion,
  scalacOptions ++= Seq(
    "-language:higherKinds",
    "-language:implicitConversions",
    "-feature",
    // "-deprecation",
    "-unchecked"
  ),
  resolvers += Resolver.sonatypeRepo("releases"),
  resolvers += "bintray/non" at "http://dl.bintray.com/non/maven",
  initialCommands in console := 
    """
       import opetopic._
    """
)

lazy val clients = Seq(opetopicJs, opetopicSketchpad, opetopicMultiedit, opetopicColoredit, opetopicAddrExplorer, opetopicMtt, opetopicDocs)

lazy val opetopicPlay = (project in file("opetopic-play")).
  settings(commonSettings: _*).
  settings(
    scalaJSProjects := clients,
    pipelineStages in Assets := Seq(scalaJSPipeline),
    JsEngineKeys.engineType := JsEngineKeys.EngineType.Node,
    includeFilter in (Assets, LessKeys.less) := "opetopic.less",
    resolvers := ("Atlassian Releases" at "https://maven.atlassian.com/public/") +: resolvers.value,
    libraryDependencies ++= Seq(
      "com.mohiva" %% "play-silhouette" % "3.0.2",
      "net.codingwell" %% "scala-guice" % "4.0.0",
      "net.ceedubs" %% "ficus" % "1.1.2",
      "com.lihaoyi" %%% "upickle" % upickleVersion,
      "org.webjars" %% "webjars-play" % "2.4.0-1",
      "org.webjars" % "jquery" % "2.1.4",
      "org.webjars" % "Semantic-UI" % "2.1.6",
      "org.webjars" % "codemirror" % codeMirrorVersion,
      "org.webjars.bower" % "snap.svg" % "0.4.1",
      "org.webjars.bower" % "reveal.js" % "3.3.0",
      "org.postgresql" % "postgresql" % "9.4-1200-jdbc41",
      "com.typesafe.play" %% "play-slick" % "1.1.1",
      cache,
      // evolutions,
      filters
    ),
    routesGenerator := InjectedRoutesGenerator,
    herokuAppName in Compile := "opetopic",
    herokuSkipSubProjects in Compile := false
  ).enablePlugins(PlayScala).
  aggregate(clients.map(projectToRef): _*).
  dependsOn(opetopicCoreJvm)

// lazy val opetopicTt = (crossProject in file("opetopic-tt")).
//   settings(commonSettings: _*).
//   settings(
//     bnfcBasePackage := Some("opetopic"),
//     bnfcSrcDirectory := baseDirectory.value / ".." / "shared" / "src" / "main" / "bnfc",
//     bnfcTgtDirectory := (sourceManaged in Compile).value,
//     scalaBisonJar := baseDirectory.value / ".." / "project" / "lib" / "scala-bison-2.11.jar",
//     jflexScalaJar := baseDirectory.value / ".." / "project" / "lib" / "jflex-scala-1.7.0-SNAPSHOT.jar"
//   ).jsConfigure(_.dependsOn(opetopicCoreJs).enablePlugins(SbtBnfcPlugin)).
//   jvmConfigure(_.dependsOn(opetopicCoreJvm).enablePlugins(SbtBnfcPlugin)).
//   jsSettings(persistLauncher := true)

// lazy val opetopicTtJvm = opetopicTt.jvm
// lazy val opetopicTtJs = opetopicTt.js

lazy val opetopicMtt = (project in file("opetopic-mtt")).
  settings(commonSettings: _*).
  settings(
    persistLauncher := true,
    unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % scalaJsDomVersion,
      "be.doeraene" %%% "scalajs-jquery" % scalaJsJQueryVersion,
      "com.lihaoyi" %%% "scalatags" % scalatagsVersion,
      "com.lihaoyi" %%% "upickle" % upickleVersion
    )
  ).enablePlugins(ScalaJSPlugin).
  dependsOn(opetopicJs)

lazy val opetopicDocs = (project in file("opetopic-docs")).
  settings(commonSettings: _*).
  settings(
    persistLauncher := true,
    unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % scalaJsDomVersion,
      "be.doeraene" %%% "scalajs-jquery" % scalaJsJQueryVersion,
      "com.lihaoyi" %%% "scalatags" % scalatagsVersion
    )
  ).enablePlugins(ScalaJSPlugin).
  dependsOn(opetopicJs)

// lazy val opetopicProver = (project in file("opetopic-prover")).
//   settings(commonSettings: _*).
//   settings(
//     persistLauncher := true,
//     unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
//     resolvers += sbt.Resolver.bintrayRepo("denigma", "denigma-releases"),
//     libraryDependencies ++= Seq(
//       "org.scala-js" %%% "scalajs-dom" % scalaJsDomVersion,
//       "be.doeraene" %%% "scalajs-jquery" % scalaJsJQueryVersion,
//       "com.lihaoyi" %%% "scalatags" % scalatagsVersion,
//       "com.lihaoyi" %%% "upickle" % upickleVersion,
//       "org.denigma" %%% "codemirror-facade" % codeMirrorFacadeVersion
//     )
//   ).enablePlugins(ScalaJSPlugin).
//   dependsOn(opetopicTtJs).
//   dependsOn(opetopicJs)

lazy val opetopicMultiedit = (project in file("opetopic-multiedit")).
  settings(commonSettings: _*).
  settings(
    persistLauncher := true,
    unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % scalaJsDomVersion,
      "be.doeraene" %%% "scalajs-jquery" % scalaJsJQueryVersion,
      "com.lihaoyi" %%% "scalatags" % scalatagsVersion,
      "com.lihaoyi" %%% "upickle" % upickleVersion,
      "com.lihaoyi" %%% "fastparse" % fastparseVersion
    )
  ).enablePlugins(ScalaJSPlugin).
  dependsOn(opetopicJs)

lazy val opetopicColoredit = (project in file("opetopic-coloredit")).
  settings(commonSettings: _*).
  settings(
    persistLauncher := true,
    unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % scalaJsDomVersion,
      "be.doeraene" %%% "scalajs-jquery" % scalaJsJQueryVersion,
      "com.lihaoyi" %%% "scalatags" % scalatagsVersion,
      "com.lihaoyi" %%% "upickle" % upickleVersion,
      "com.lihaoyi" %%% "fastparse" % fastparseVersion
    )
  ).enablePlugins(ScalaJSPlugin).
  dependsOn(opetopicJs)

lazy val opetopicAddrExplorer = (project in file("opetopic-addrexplorer")).
  settings(commonSettings: _*).
  settings(
    persistLauncher := true,
    unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % scalaJsDomVersion,
      "be.doeraene" %%% "scalajs-jquery" % scalaJsJQueryVersion,
      "com.lihaoyi" %%% "scalatags" % scalatagsVersion,
      "com.lihaoyi" %%% "upickle" % upickleVersion
    )
  ).enablePlugins(ScalaJSPlugin).
  dependsOn(opetopicJs)

lazy val opetopicSketchpad = (project in file("opetopic-sketchpad")).
  settings(commonSettings: _*).
  settings(
    persistLauncher := true,
    unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % scalaJsDomVersion,
      "be.doeraene" %%% "scalajs-jquery" % scalaJsJQueryVersion,
      "com.lihaoyi" %%% "scalatags" % scalatagsVersion,
      "com.lihaoyi" %%% "upickle" % upickleVersion,
      "com.lihaoyi" %%% "fastparse" % fastparseVersion
    )
  ).enablePlugins(ScalaJSPlugin).
  dependsOn(opetopicJs)

lazy val opetopicJs = (project in file("opetopic-js")).
  settings(commonSettings: _*).
  settings(
    persistLauncher := true,
    unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % scalaJsDomVersion,
      "be.doeraene" %%% "scalajs-jquery" % scalaJsJQueryVersion,
      "com.lihaoyi" %%% "scalatags" % scalatagsVersion,
      "com.lihaoyi" %%% "upickle" % upickleVersion
    )
  ).enablePlugins(ScalaJSPlugin).
  dependsOn(opetopicCoreJs)

lazy val opetopicCore = (crossProject.crossType(CrossType.Pure) in file("opetopic-core")).
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "scalatags" % scalatagsVersion,
      "com.lihaoyi" %%% "upickle" % upickleVersion,
      "com.lihaoyi" %%% "fastparse" % fastparseVersion
    )
  )

lazy val opetopicCoreJvm = opetopicCore.jvm
lazy val opetopicCoreJs = opetopicCore.js



