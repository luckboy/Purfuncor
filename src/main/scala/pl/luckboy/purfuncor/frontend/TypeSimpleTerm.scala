package pl.luckboy.purfuncor.frontend
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._

sealed trait TypeSimpleTerm[+T, +U]
case class TypeLambda[+T, +U](args: NonEmptyList[TypeArg], body: Term[TypeSimpleTerm[T, U]], lambdaInfo: U) extends TypeSimpleTerm[T, U]
case class TypeVar[+T, +U](loc: T) extends TypeSimpleTerm[T, U]
case class TypeLiteral[+T, +U](value: TypeLiteralValue) extends TypeSimpleTerm[T, U]

case class TypeArg(name: Option[String], kind: Option[Kind[StarKind[String]]], pos: Position)