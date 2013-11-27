package pl.luckboy.purfuncor.frontend
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._

case class Instance[+T](instanceCombLoc: T, file: Option[java.io.File])