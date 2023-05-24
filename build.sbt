val scala3Version = "3.2.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "multiplicación de matrices en paralelo",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
  )
