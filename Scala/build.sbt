name := "WSQ"

organization := "dk.itu"
 
version := "0.1"
 
scalaVersion := "2.10.3"

scalacOptions += "-deprecation"

scalacOptions += "-feature"
 
resolvers ++= Seq()
 
libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
)

testOptions in Test += Tests.Argument("-oD")

org.scalastyle.sbt.ScalastylePlugin.Settings
