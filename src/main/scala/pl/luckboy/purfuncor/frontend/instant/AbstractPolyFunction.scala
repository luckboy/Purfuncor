package pl.luckboy.purfuncor.frontend.instant
import scalaz._
import scalaz.Scalaz._

sealed trait AbstractPolyFunction[+T]
{
  override def toString =
    this match {
      case PolyFunction(loc) => loc.toString
      case ConstructFunction => "construct"
      case SelectFunction    => "select"
    }
}

case class PolyFunction[+T](loc: T) extends AbstractPolyFunction[T]
case object ConstructFunction extends AbstractPolyFunction[Nothing]
case object SelectFunction extends AbstractPolyFunction[Nothing]