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
      scalaVersion := "2.10.1",
      // add other settings here
      libraryDependencies += "org.scalaz" % "scalaz-core_2.10" % "7.0.0",
      scalacOptions ++= Seq("-feature", "-language:postfixOps", "-language:higherKinds", "-language:implicitConversions"),
      initialCommands in console := "import scalaz._, Scalaz._, pl.luckboy.purfuncor._, pl.luckboy.purfuncor.frontend.parser.Parser.parseString"
    )
  )
}
