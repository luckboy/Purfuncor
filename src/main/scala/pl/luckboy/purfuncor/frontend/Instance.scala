package pl.luckboy.purfuncor.frontend
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._

case class Instance[+T](instCombLoc: T, file: Option[java.io.File])
{
  def toStringForName(name: String) = "instance " + name + " => " + instCombLoc
}