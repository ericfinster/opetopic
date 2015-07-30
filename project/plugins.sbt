// The Typesafe repository
resolvers ++= Seq(
  "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"
)

// For simplifying scala js export
addSbtPlugin("com.vmunier" % "sbt-play-scalajs" % "0.2.6")

// Use the Play sbt plugin for Play projects
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.4.2")

// Use the scala-js pluging 
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.3")

