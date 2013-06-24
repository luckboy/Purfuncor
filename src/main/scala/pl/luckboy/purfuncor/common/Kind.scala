package pl.luckboy.purfuncor.common
import scala.util.parsing.input.Position

sealed trait Kind[+T]
case class Arrow[+T](arg: Kind[T], ret: Kind[T], pos: Position) extends Kind[T]
case class Star[+T](starKind: T, pos: Position) extends Kind[T]