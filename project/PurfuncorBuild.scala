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
      libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
      libraryDependencies += "org.scala-lang" % "jline" % "2.10.1",
      scalacOptions ++= Seq("-feature", "-language:postfixOps", "-language:higherKinds", "-language:implicitConversions"),
      initialCommands in console := "import scalaz._, Scalaz._;",
      initialCommands in console += "import pl.luckboy.purfuncor._;",
      initialCommands in console += "import pl.luckboy.purfuncor.frontend.parser.Parser;",
      initialCommands in console += "import pl.luckboy.purfuncor.frontend.resolver.Resolver;",
      initialCommands in console += "import pl.luckboy.purfuncor.frontend.resolver.NameTree;",
      initialCommands in console += "import pl.luckboy.purfuncor.frontend.resolver.Scope;",
      initialCommands in console += "import pl.luckboy.purfuncor.backend.interp.Interpreter;",
      initialCommands in console += "import pl.luckboy.purfuncor.backend.interp.SymbolEnvironment"
    )
  )
}
