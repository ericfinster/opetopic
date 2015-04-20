name := "opetopic root"

lazy val root = project.in(file(".")).
  aggregate(opetopicJS, opetopicJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val opetopic = crossProject.in(file(".")).
  settings(
    name := "opetopic",
    organization := "opetopic",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.11.6",
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation"
    ),
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.5.1"
  ).jvmSettings(
    fork := true,
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0",
    libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.40-R8"
  ).jsSettings(
    libraryDependencies += "com.github.japgolly.fork.scalaz" %%% "scalaz-core" % "7.1.1-2"
  )

lazy val opetopicJVM = opetopic.jvm
lazy val opetopicJS = opetopic.js






