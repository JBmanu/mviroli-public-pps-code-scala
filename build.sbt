scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "pps-code-a",
    libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.2" % Test,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test
  )
