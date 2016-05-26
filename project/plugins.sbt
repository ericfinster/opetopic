// Plugin Setup

resolvers += "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"

resolvers += Resolver.url("heroku-sbt-plugin-releases",
  url("https://dl.bintray.com/heroku/sbt-plugins/"))(Resolver.ivyStylePatterns)

// The Play plugin
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.4.3")

// Use the scala-js pluging 
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.7")

// Play ScalaJS
addSbtPlugin("com.vmunier" % "sbt-play-scalajs" % "0.2.8")

// Heroku
addSbtPlugin("com.heroku" % "sbt-heroku" % "0.5.3.1")

// Sbt-Less for building less files
addSbtPlugin("com.typesafe.sbt" % "sbt-less" % "1.1.0")

