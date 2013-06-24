package pl.luckboy.purfuncor.frontend
import scala.util.parsing.input.Position
import scalaz._
import pl.luckboy.purfuncor.common._

sealed trait SimpleTerm[+T, +U, +V]
{
  override def toString = simpleTermIndenting.indentedStringFrom(this)(0)
}
// A lambdaInfo field for types.
case class Let[+T, +U, +V](binds: NonEmptyList[Bind[T, U, V]], body: Term[SimpleTerm[T, U, V]], lambdaInfo: U) extends SimpleTerm[T, U, V]
case class Lambda[+T, +U, +V](args: NonEmptyList[Arg], body: Term[SimpleTerm[T, U, V]], lambdaInfo: U) extends SimpleTerm[T, U, V]
case class Var[+T, +U, +V](loc: T) extends SimpleTerm[T, U, V]
case class Literal[+T, +U, +V](value: LiteralValue) extends SimpleTerm[T, U, V]

case class Bind[+T, +U, +V](name: String, body: Term[SimpleTerm[T, U, V]], pos: Position)
{
  override def toString = bindIndenting.indentedStringFrom(Bind.this)(0)
}

case class Arg(name: Option[String], pos: Position)
{
  override def toString = name.map { _.toString }.getOrElse("_")
}