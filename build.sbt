name := "rdb-sjs"

version := "0.1a"

description := "In-memory relational database system"

scalaVersion := "2.13.2"

scalacOptions ++= Seq( "-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:existentials" )

organization := "xyz.hyperreal"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

enablePlugins(ScalaJSPlugin)

enablePlugins(ScalablyTypedConverterPlugin)

scalaJSUseMainModuleInitializer := true

jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv()

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "3.1.1" % "test",
	"org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
)

libraryDependencies ++= Seq(
	"org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.2"
//	"org.scala-lang.modules" %% "scala-xml" % "1.0.6"
)

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-java-time" % "1.0.0"
)

libraryDependencies ++= Seq(
//	"xyz.hyperreal" %% "json" % "0.8.0",
	"xyz.hyperreal" %%% "table-sjs" % "0.11.2",
//	"xyz.hyperreal" %% "options" % "0.3",
	"xyz.hyperreal" %%% "importer-sjs" % "0.5.1",
  "xyz.hyperreal" %%% "numbers-sjs" % "0.7.1"
)

npmDependencies in Compile ++= Seq(
)

mainClass in (Compile, run) := Some( "xyz.hyperreal." + name.value.replace('-', '_') + ".REPLMain" )

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

licenses := Seq("ISC" -> url("https://opensource.org/licenses/ISC"))

homepage := Some(url("https://github.com/edadma/" + name.value))

pomExtra :=
  <scm>
    <url>git@github.com:edadma/{name.value}.git</url>
    <connection>scm:git:git@github.com:edadma/{name.value}.git</connection>
  </scm>
  <developers>
    <developer>
      <id>edadma</id>
      <name>Edward A. Maxedon, Sr.</name>
      <url>https://github.com/edadma</url>
    </developer>
  </developers>
