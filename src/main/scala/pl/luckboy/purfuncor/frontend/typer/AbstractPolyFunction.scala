package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._

sealed trait AbstractPolyFunction[+T]

case class PolyFunction[+T](loc: T) extends AbstractPolyFunction[T]
case object ConstructFunction extends AbstractPolyFunction[Nothing]
case object SelectFunction extends AbstractPolyFunction[Nothing]