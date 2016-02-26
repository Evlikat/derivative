name := "derivative"

version := "1.0"

scalaVersion := "2.11.7"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.0-M15" % "test"
libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.4.2"
