ThisBuild / licenses += "ISC" -> url("https://opensource.org/licenses/ISC")
ThisBuild / versionScheme := Some("semver-spec")

lazy val rdb = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file(".")).
  settings(
    name := "rdb",
    version := "0.1.0-pre.1",
    description := "In-memory relational database system",
    scalaVersion := "2.13.8",
    scalacOptions ++=
    Seq(
      "-deprecation", "-feature", "-unchecked",
      "-language:postfixOps", "-language:implicitConversions", "-language:existentials", "-language:dynamics",
      "-Xasync"
      ),
    organization := "io.github.edadma",
    githubOwner := "edadma",
    githubRepository := "rdb",
    scalaJSUseMainModuleInitializer := true,
    githubRepository := name.value,
    mainClass := Some(s"${organization.value}.char_reader.Main"),
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.2.11" % "test"
      ),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.1.0",
      ),
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time" % "2.4.0-M1"
      ),
    libraryDependencies ++= Seq(
      "io.github.edadma" %%% "table" % "1.0.0",
      "io.github.edadma" %%% "importer" % "0.1.7",
      "io.github.edadma" %%% "dal" % "0.1.5",
      "io.github.edadma" %%% "cross-platform" % "0.1.2"
      ),
    publishMavenStyle := true,
    Test / publishArtifact := false,
    licenses += "ISC" -> url("https://opensource.org/licenses/ISC")
  ).
  jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
  ).
  nativeSettings(
    nativeLinkStubs := true
  ).
  jsSettings(
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
    //    Test / scalaJSUseMainModuleInitializer := true,
    //    Test / scalaJSUseTestModuleInitializer := false,
    Test / scalaJSUseMainModuleInitializer := false,
    Test / scalaJSUseTestModuleInitializer := true,
    scalaJSUseMainModuleInitializer := true,
  )
