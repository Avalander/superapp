import Dependencies._

ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

enablePlugins(ScalaJSPlugin)
enablePlugins(ScalaJSBundlerPlugin)

scalaJSUseMainModuleInitializer := true

lazy val root = (project in file("."))
  .settings(
    name := "hello-web",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      "org.scala-js" %%% "scalajs-dom" % "1.0.0",
      "com.lihaoyi" %%% "utest" % "0.7.4" % "test"
    ),
    testFrameworks += new TestFramework("utest.runner.Framework"),
    npmDependencies in Compile += "superfine" -> "7.0.0"
  )

jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
