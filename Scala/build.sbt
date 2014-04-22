name := "WSQ"

organization := "dk.itu"
 
version := "0.1"
 
scalaVersion := "2.11.0"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

scalacOptions += "-Xfatal-warnings"

incOptions := incOptions.value.withNameHashing(true)
 
resolvers ++= Seq()
 
libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.0.1",
  "org.scalatest" %% "scalatest" % "2.1.3" % "test",
  "com.typesafe" % "config" % "1.2.0",
  "org.scala-stm" %% "scala-stm" % "0.7"
)

testOptions in Test += Tests.Argument("-oD")

org.scalastyle.sbt.ScalastylePlugin.Settings

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

mainClass in oneJar := Some("dk.itu.wsq.BenchmarkApp")
