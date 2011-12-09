name := "relaxng"

version := "0.1"

organization := "com.inkling"

scalaVersion := "2.9.1"

// Testing
libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "1.6.1",
                            "org.scala-tools.testing" %% "scalacheck" % "1.9")

scalacOptions ++= Seq("-deprecation", "-Xcheckinit", "-unchecked")

