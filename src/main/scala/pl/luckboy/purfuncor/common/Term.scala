package pl.luckboy.purfuncor.common
import scala.util.parsing.input.Position
import scalaz._

sealed trait Term[+T]
case class App[+T](fun: Term[T], args: NonEmptyList[Term[T]], pos: Position) extends Term[T]
case class Simple[+T](simpleTerm: T, pos: Position) extends Term[T]