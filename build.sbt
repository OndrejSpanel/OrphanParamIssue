ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"
//ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "Sandbox",
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-Xcheck-macros",
    ),

  )
