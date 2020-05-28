import sbt.Project.projectToRef
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val scalaCompilerVersion = "2.11.12"
val scalaJsDomVersion = "0.9.5"
val scalaJsJQueryVersion = "0.9.0"
val scalatagsVersion = "0.6.0"
val silhouetteVersion = "5.0.0"
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
    "-deprecation",
    "-unchecked"
  ),
  resolvers += Resolver.sonatypeRepo("releases"),
  resolvers += "bintray/non" at "http://dl.bintray.com/non/maven",
  initialCommands in console := 
    """
       import opetopic._
    """
)

lazy val clients = Seq(opetopicJs, opetopicStudio, opetopicDocs, opetopicLf, opetopicMultiEdit)

lazy val opetopicPlay = (project in file("opetopic-play")).
  settings(commonSettings: _*).
  settings(
    scalaJSProjects := clients,
    pipelineStages in Assets := Seq(scalaJSPipeline),
    JsEngineKeys.engineType := JsEngineKeys.EngineType.Node,
    includeFilter in (Assets, LessKeys.less) := "opetopic.less",
    resolvers := ("Atlassian Releases" at "https://maven.atlassian.com/public/") +: resolvers.value,
    libraryDependencies ++= Seq(
      "com.mohiva" %% "play-silhouette" % "5.0.7",
      "com.mohiva" %% "play-silhouette-password-bcrypt" % "5.0.7",
      "com.mohiva" %% "play-silhouette-persistence" % "5.0.7",
      "com.mohiva" %% "play-silhouette-crypto-jca" % "5.0.7",
      "net.codingwell" %% "scala-guice" % "4.1.0",
      "com.iheart" %% "ficus" % "1.4.1",
      "com.lihaoyi" %%% "upickle" % upickleVersion,
      "org.webjars" %% "webjars-play" % "2.6.3",
      "org.webjars" % "jquery" % "2.2.4",
      "org.webjars" % "Semantic-UI" % "2.1.6",
      "org.webjars" % "codemirror" % codeMirrorVersion,
      "org.webjars.bower" % "snap.svg" % "0.4.1",
      "org.webjars.bower" % "reveal.js" % "3.3.0",
      "org.postgresql" % "postgresql" % "9.4-1200-jdbc41",
      "com.typesafe.play" %% "play-slick" % "3.0.0",
      ehcache,
      guice,
      filters
    ),
    routesGenerator := InjectedRoutesGenerator,
    herokuAppName in Compile := "opetopic",
    herokuSkipSubProjects in Compile := false
  ).enablePlugins(PlayScala).
  aggregate(clients.map(projectToRef): _*).
  dependsOn(opetopicCoreJvm)

lazy val opetopicStudio = (project in file("opetopic-studio")).
  settings(commonSettings: _*).
  settings(
    scalaJSUseMainModuleInitializer := true,
    mainClass in Compile := Some("opetopic.studio.Studio"),
    resolvers += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
    unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % scalaJsDomVersion,
      "be.doeraene" %%% "scalajs-jquery" % scalaJsJQueryVersion,
      "com.lihaoyi" %%% "scalatags" % scalatagsVersion,
      "com.lihaoyi" %%% "upickle" % upickleVersion
    )
  ).enablePlugins(ScalaJSPlugin).
  dependsOn(opetopicJs)

lazy val opetopicLf = (project in file("opetopic-lf")).
  settings(commonSettings: _*).
  settings(
    scalaJSUseMainModuleInitializer := true,
    mainClass in Compile := Some("opetopic.lf.Lf"),
    resolvers += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
    unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % scalaJsDomVersion,
      "be.doeraene" %%% "scalajs-jquery" % scalaJsJQueryVersion,
      "com.lihaoyi" %%% "scalatags" % scalatagsVersion,
      "com.lihaoyi" %%% "upickle" % upickleVersion
    )
  ).enablePlugins(ScalaJSPlugin).
  dependsOn(opetopicJs)

lazy val opetopicMultiEdit = (project in file("opetopic-multiedit")).
  settings(commonSettings: _*).
  settings(
    scalaJSUseMainModuleInitializer := true,
    mainClass in Compile := Some("opetopic.multiedit.MultiEdit"),
    resolvers += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
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
    scalaJSUseMainModuleInitializer := true,
    unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % scalaJsDomVersion,
      "be.doeraene" %%% "scalajs-jquery" % scalaJsJQueryVersion,
      "com.lihaoyi" %%% "scalatags" % scalatagsVersion
    )
  ).enablePlugins(ScalaJSPlugin).
  dependsOn(opetopicJs)

lazy val opetopicJs = (project in file("opetopic-js")).
  settings(commonSettings: _*).
  settings(
    scalaJSUseMainModuleInitializer := true,
    unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % scalaJsDomVersion,
      "be.doeraene" %%% "scalajs-jquery" % scalaJsJQueryVersion,
      "com.lihaoyi" %%% "scalatags" % scalatagsVersion,
      "com.lihaoyi" %%% "upickle" % upickleVersion
    )
  ).enablePlugins(ScalaJSPlugin).
  dependsOn(opetopicCoreJs)

lazy val opetopicCore =
  crossProject(JSPlatform, JVMPlatform).
    crossType(CrossType.Pure).
    in(file("opetopic-core")).
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



