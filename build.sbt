/** Project */
name := "Yacht"

version := "0.1-SNAPSHOT"

organization := "com.samthomson"

scalaVersion := "2.9.1"

mainClass := Some("yacht.YachtApp")

/** Dependencies */
libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "1.7.2",
    "org.scala-lang" % "scala-swing" % "2.9.1"
)
