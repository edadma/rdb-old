name := "rdb"

version := "0.1.0-pre.1"

description := "In-memory relational database system"

scalaVersion := "2.13.8"

scalacOptions ++= Seq("-deprecation",
                      "-feature",
                      "-language:postfixOps",
                      "-language:implicitConversions",
                      "-language:existentials")

organization := "io.github.edadma"

githubOwner := "edadma"

githubRepository := "rdb-sjs"

enablePlugins(ScalaJSPlugin)

//enablePlugins(ScalablyTypedConverterPlugin)

scalaJSUseMainModuleInitializer := true

Test / scalaJSUseMainModuleInitializer := false

Test / scalaJSUseTestModuleInitializer := true

//Test / scalaJSUseMainModuleInitializer := true
//
//Test / scalaJSUseTestModuleInitializer := false

jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv()

libraryDependencies ++= Seq(
  "org.scalatest" %%% "scalatest" % "3.2.11" % "test"
)

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.1.0",
//	"org.scala-lang.modules" %% "scala-xml" % "1.0.6"
  "io.github.cquiroz" %%% "scala-java-time" % "2.3.0"
)

libraryDependencies ++= Seq(
  "io.github.edadma" %%% "table" % "1.0.0",
  "io.github.edadma" %%% "importer" % "0.1.5",
  "io.github.edadma" %%% "dal" % "0.1.5"
)

mainClass := Some("io.github.edadma." + name.value.replace('-', '_') + ".REPLMain")

publishMavenStyle := true

Test / publishArtifact := false

licenses := Seq("ISC" -> url("https://opensource.org/licenses/ISC"))
