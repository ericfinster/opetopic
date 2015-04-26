// The Typesafe repository
resolvers += "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"

// Use the Play sbt plugin for Play projects
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.3.8")

// Use the scala-js pluging 
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.2")

// For scala-js/play interation
addSbtPlugin("com.vmunier" % "sbt-play-scalajs" % "0.2.4")
