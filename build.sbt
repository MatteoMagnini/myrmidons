name := "pps-19-myrmidons"

version := "0.1"

scalaVersion := "2.12.6"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xcheckinit", "-encoding", "utf8")
val AkkaVersion = "2.6.8"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.1.2" % "test",
  "com.typesafe.akka" %% "akka-actor" % AkkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % AkkaVersion % Test,
  "org.scala-lang.modules" %% "scala-swing" % "2.1.1",
  "org.jfree" % "jfreechart" % "1.0.14",
  "it.unibo.alice.tuprolog" % "tuprolog" % "3.3.0",
  "com.google.code.gson" % "gson" % "2.2.4"
)
useCoursier := false
coverageEnabled := false
test in assembly := {}
assemblyJarName in assembly := "pps-19-Myrmidons-1.0.jar"
mainClass in assembly := Some("Myrmidons")
