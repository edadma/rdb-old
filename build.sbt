name := "rdb-sjs"

version := "0.1.0-snapshot.7"

description := "In-memory relational database system"

scalaVersion := "2.13.6"

scalacOptions ++= Seq( "-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:existentials" )

organization := "xyz.hyperreal"

githubOwner := "edadma"

githubRepository := "rdb-sjs"

enablePlugins(ScalaJSPlugin)

//enablePlugins(ScalablyTypedConverterPlugin)

//scalaJSUseMainModuleInitializer := true

//Test / scalaJSUseMainModuleInitializer := true
//
//Test / scalaJSUseTestModuleInitializer := false

jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv()

libraryDependencies ++= Seq(
	"org.scalatest" %%% "scalatest" % "3.2.5" % "test"
)

libraryDependencies ++= Seq(
	"org.scala-lang.modules" %%% "scala-parser-combinators" % "2.0.0",
//	"org.scala-lang.modules" %% "scala-xml" % "1.0.6"
  "io.github.cquiroz" %%% "scala-java-time" % "2.0.0"
)

libraryDependencies ++= Seq(
//	"xyz.hyperreal" %% "json" % "0.8.0",
	"xyz.hyperreal" %%% "table" % "1.0.0-snapshot.3",
//	"xyz.hyperreal" %% "options" % "0.3",
	"xyz.hyperreal" %%% "importer" % "0.1.1",
  "xyz.hyperreal" %%% "dal" % "0.2.0"
)

//npmDependencies in Compile ++= Seq(
//)

mainClass := Some( "xyz.hyperreal." + name.value.replace('-', '_') + ".REPLMain" )

publishMavenStyle := true

Test / publishArtifact := false

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
