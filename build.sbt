import sbt.Project.projectToRef

lazy val clients = Seq(opetopicJs)
lazy val scalaV = "2.11.6"

lazy val opetopicPlay = (project in file("opetopic-play")).
  settings(
    scalaVersion := scalaV
    // scalaJsProjects := clients
  ).enablePlugins(PlayScala).
  aggregate(clients.map(projectToRef): _*).
  dependsOn(opetopicCoreJvm)

lazy val opetopicJs = (project in file("opetopic-js")).
  settings(
    scalaVersion := scalaV,
    sourceMapsDirectories += opetopicCoreJs.base / "..",
    unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.8.0"
    )
  ).enablePlugins(ScalaJSPlugin, ScalaJSPlay).
  dependsOn(opetopicCoreJs)

lazy val opetopicFx = (project in file("opetopic-fx")).
  settings(
    scalaVersion := scalaV,
    fork := true,
    mainClass := Some("FXEditor"),
    libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.40-R8"
  ).dependsOn(opetopicCoreJvm)

lazy val opetopicCore = (crossProject.crossType(CrossType.Pure) in file("opetopic-core")).
  settings(
    scalaVersion := scalaV,
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.5.1",
    libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.2.8"
  ).
  jsConfigure(_ enablePlugins ScalaJSPlay).
  jsSettings(
    libraryDependencies += "com.github.japgolly.fork.scalaz" %%% "scalaz-core" % "7.1.1-2",
    sourceMapsBase := baseDirectory.value / ".."
  ).jvmSettings(
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.1"
  )

lazy val opetopicCoreJvm = opetopicCore.jvm
lazy val opetopicCoreJs = opetopicCore.js






