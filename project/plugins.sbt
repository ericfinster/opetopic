// Plugin Setup

resolvers += "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"

resolvers += Resolver.url("heroku-sbt-plugin-releases",
  url("https://dl.bintray.com/heroku/sbt-plugins/"))(Resolver.ivyStylePatterns)

// The Play plugin
addSbtPlugin("com.typesafe.play"  % "sbt-plugin"                    % "2.6.15")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "0.4.0")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"                   % "0.6.23")
addSbtPlugin("com.vmunier"        % "sbt-web-scalajs"               % "1.0.7")
addSbtPlugin("com.heroku"         % "sbt-heroku"                    % "0.5.3.1")
addSbtPlugin("com.typesafe.sbt"   % "sbt-less"                      % "1.1.0")


