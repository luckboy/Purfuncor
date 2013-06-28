package pl.luckboy.purfuncor.frontend
import scalaz._
import scalaz.Scalaz._

sealed trait TypeLiteralValue
{
  override def toString =
    this match {
      case TupleTypeFunValue(n)    => "tuple " + n
      case TypeBuiltinFunValue(bf) =>
        if(bf.toString.headOption.map { c => c.isLetter || c === '_' }.getOrElse(false)) "#" + bf else "##" + bf
    }
}
case class TupleTypeFunValue(n: Int) extends TypeLiteralValue
case class TypeBuiltinFunValue(bf: TypeBuiltinFunction.Value) extends TypeLiteralValue