name := "WSQ"

organization := "dk.itu"
 
version := "0.1"
 
scalaVersion := "2.10.4"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

scalacOptions += "-Xfatal-warnings"
 
resolvers ++= Seq()
 
libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
  "com.typesafe" % "config" % "1.2.0",
  "org.multiverse" % "multiverse-core" % "0.7.0"
)

testOptions in Test += Tests.Argument("-oD")

org.scalastyle.sbt.ScalastylePlugin.Settings

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

mainClass in oneJar := Some("dk.itu.wsq.BenchmarkApp")
