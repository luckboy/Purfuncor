package pl.luckboy.purfuncor.frontend.instant
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._

sealed trait Instance[+T]
{
  override def toString =
    this match {
      case PolyFunInstance(loc, _, _) => loc.toString
      case ConstructInstance(i, _, _) => "<construct instance: " + i + ">"
      case SelectInstance(n, _, _)    => "<select instance: " + n + ">"
      case LocalInstance(idx)         => "<local instance: " + idx + ">"
    }
}

sealed trait GlobalInstance[+T] extends Instance[T]

case class PolyFunInstance[+T](loc: T, pos: Position, file: Option[java.io.File]) extends GlobalInstance[T]
case class ConstructInstance[+T](i: Int, pos: Position, file: Option[java.io.File]) extends GlobalInstance[T]
case class SelectInstance[+T](n: Int, pos: Position, file: Option[java.io.File]) extends GlobalInstance[T]

case class LocalInstance[+T](idx: Int) extends Instance[T]