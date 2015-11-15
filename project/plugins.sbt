// The Typesafe repository
resolvers ++= Seq(
  "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"
)

// Use the scala-js pluging 
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.5")

// Sbt-Less for building less files
addSbtPlugin("com.typesafe.sbt" % "sbt-less" % "1.1.0")
