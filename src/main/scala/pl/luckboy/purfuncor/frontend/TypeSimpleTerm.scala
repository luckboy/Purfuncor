package pl.luckboy.purfuncor.frontend
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._

sealed trait TypeSimpleTerm[+T, +U]
{
  override def toString =
    this match {
      case TypeLambda(args, body, lambdaInfo) =>
        "\\" + args.map { a => a.kind.map { _ => "(" + a + ")" }.getOrElse(a.toString) + " " }.list.mkString("") +
        (if(lambdaInfo.toString =/= "")  "/*" + lambdaInfo.toString + "*/ " else "") +
        "=> " + typeTermShowing.stringFrom(body)
      case TypeVar(loc)                       =>
        loc.toString
      case TypeLiteral(value)                 =>
        value.toString
      case KindedTypeTerm(term, kind)         =>
        typeTermShowing.stringFrom(term) + ": " + kindShowing.stringFrom(kind)
    }
}

case class TypeLambda[+T, +U](args: NonEmptyList[TypeArg], body: Term[TypeSimpleTerm[T, U]], lambdaInfo: U) extends TypeSimpleTerm[T, U]
case class TypeVar[+T, +U](loc: T) extends TypeSimpleTerm[T, U]
case class TypeLiteral[+T, +U](value: TypeLiteralValue) extends TypeSimpleTerm[T, U]
case class KindedTypeTerm[+T, +U](term: Term[TypeSimpleTerm[T, U]], kind: Kind[StarKind[String]]) extends TypeSimpleTerm[T, U]

case class TypeArg(name: Option[String], kind: Option[Kind[StarKind[String]]], pos: Position)
{
  override def toString = name.map { _.toString }.getOrElse("_") + kind.map { k => ": " + kindShowing.stringFrom(k) }.getOrElse("")
}