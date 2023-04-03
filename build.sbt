ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(name := "ShapelessExamples")
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.10"
libraryDependencies += "io.circe" %% "circe-core" % "0.14.5"
