package pl.luckboy.purfuncor.frontend
import scala.util.parsing.input.Position
import scalaz._
import pl.luckboy.purfuncor.common._

sealed trait SimpleTerm[+T, +U]
// A letInfo field for types.
case class Let[+T, +U](binds: NonEmptyList[LocalBind[T, U]], body: Term[SimpleTerm[T, U]], letInfo: U) extends SimpleTerm[T, U]
case class Lambda[+T, +U](args: NonEmptyList[Arg], body: Term[SimpleTerm[T, U]], letInfo: U) extends SimpleTerm[T, U]
case class Var[+T, +U](loc: T) extends SimpleTerm[T, U]
case class Literal[+T, +U](value: LiteralValue) extends SimpleTerm[T, U]

case class LocalBind[+T, +U](name: String, body: Term[SimpleTerm[T, U]], pos: Position)

case class Arg(name: Option[String], pos: Position)