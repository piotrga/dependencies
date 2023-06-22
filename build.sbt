ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

val circeVersion = "0.14.1"

lazy val root = (project in file("."))
  .settings(
    name := "dependencies",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion) ++ Seq(
      "org.scala-lang.modules" %% "scala-xml" % "2.1.0",
      "org.graphstream" % "gs-core" % "1.3"
    )
  )
