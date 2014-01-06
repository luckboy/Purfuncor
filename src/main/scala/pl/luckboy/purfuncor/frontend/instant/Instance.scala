package pl.luckboy.purfuncor.frontend.instant
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._

sealed trait Instance[+T]
{
  override def toString =
    this match {
      case PolyFunInstance(loc, _, _)           => loc.toString
      case ConstructInstance(i, _, _)           => "<construct instance: " + i + ">"
      case SelectInstance(n, _, _)              => "<select instance: " + n + ">"
      case LocalInstance(idx)                   => "<local instance: " + idx + ">"
      case ZeroIntegerConstructInstance(itf)    => "<zero integer construct instance: " + itf + ">"
      case NonZeroIntegerConstructInstance(itf) => "<non zero integer construct instance: " + itf + ">"
      case IntegerSelectInstance(itf)           => "<integer select instance: " + itf + ">"
      case EmptyArrayConstructInstance          => "<empty array construct instance>"
      case NonEmptyArrayConstructInstance       => "<non empty array construct instance>"
      case ArraySelectInstance                  => "<array select instance>"
    }
}

sealed trait GlobalInstance[+T] extends Instance[T]

case class PolyFunInstance[+T](loc: T, pos: Position, file: Option[java.io.File]) extends GlobalInstance[T]
case class ConstructInstance[+T](i: Int, pos: Position, file: Option[java.io.File]) extends GlobalInstance[T]
case class SelectInstance[+T](n: Int, pos: Position, file: Option[java.io.File]) extends GlobalInstance[T]
case class ZeroIntegerConstructInstance[T](itf: IntegerTypeFunction.Value) extends GlobalInstance[T]
case class NonZeroIntegerConstructInstance[T](itf: IntegerTypeFunction.Value) extends GlobalInstance[T]
case class IntegerSelectInstance[T](itf: IntegerTypeFunction.Value) extends GlobalInstance[T]
sealed trait EmptyArrayConstructInstance[+T] extends GlobalInstance[T]
case object EmptyArrayConstructInstance extends EmptyArrayConstructInstance[Nothing]
sealed trait NonEmptyArrayConstructInstance[+T] extends GlobalInstance[T]
case object NonEmptyArrayConstructInstance extends NonEmptyArrayConstructInstance[Nothing]
sealed trait ArraySelectInstance[+T] extends GlobalInstance[T]
case object ArraySelectInstance extends ArraySelectInstance[Nothing]

case class LocalInstance[+T](idx: Int) extends Instance[T]