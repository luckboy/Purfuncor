import sbt._
import sbt.Keys._

object PurfuncorBuild extends Build {

  lazy val purfuncor = Project(
    id = "purfuncor",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "Purfuncor",
      organization := "pl.luckboy.purfuncor",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.9.2",
      // add other settings here
      libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.0",
      initialCommands in console := "import scalaz._, Scalaz._, pl.luckboy.purfuncor._"
    )
  )
}
