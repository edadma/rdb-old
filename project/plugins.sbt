addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.5.6")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.0")

addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.2.7")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.1.1")

resolvers += Resolver.bintrayRepo("oyvindberg", "converter")

addSbtPlugin("org.scalablytyped.converter" % "sbt-converter" % "1.0.0-beta22")

resolvers += Resolver.bintrayRepo("edadma", "sbt-plugins")

addSbtPlugin("xyz.hyperreal" % "npm-plugin" % "0.1.13")
