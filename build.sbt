name := "pps-19-myrmidons"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.2" % "test"

libraryDependencies += "junit" % "junit" % "4.11" % "test"

val AkkaVersion = "2.6.8"
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % AkkaVersion