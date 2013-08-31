package pl.luckboy.purfuncor.common
import scala.util.parsing.input.Position

sealed trait KindTerm[+T]
{
  def pos: Position
}
case class Arrow[+T](arg: KindTerm[T], ret: KindTerm[T], pos: Position) extends KindTerm[T]
case class Star[+T](starKind: T, pos: Position) extends KindTerm[T]