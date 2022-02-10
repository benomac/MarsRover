ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test

lazy val root = (project in file("."))
  .settings(
    name := "MarsRoverTechChallenge"
  )
