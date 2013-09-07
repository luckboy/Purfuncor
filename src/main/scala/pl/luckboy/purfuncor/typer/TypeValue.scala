package pl.luckboy.purfuncor.typer
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend
import pl.luckboy.purfuncor.frontend.AbstractTypeCombinator
import pl.luckboy.purfuncor.frontend.TypeSimpleTerm
import pl.luckboy.purfuncor.frontend.TypeLambda
import pl.luckboy.purfuncor.frontend.TypeBuiltinFunction
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.common.Evaluator._

sealed trait TypeValue[T, +U, +V, +W]
{
  def argCount: Int =
    this match {
      case NoTypeValue(_)                           => 1
      case EvaluatedTypeValue(_)                    => 1
      case TupleTypeFunValue(n)                     => n
      case TypeBuiltinFunValue(_, f)                => f.argCount
      case TypeCombinatorValue(comb, _)             => comb.argCount
      case TypeLambdaValue(lambda, _)               => lambda.args.size
      case TypePartialAppValue(funValue, argValues) => funValue.argCount - argValues.size
      case TypeLazyValue(_, _)                      => 1
    }
  
  def isNoTypeValue = this.isInstanceOf[NoTypeValue[T, U, V, W]]
  
  def apply[U2 >: U, V2 >: V, W2 >: W, E](argValues: Seq[TypeValue[T, U2, V2, W2]])(implicit eval: Evaluator[TypeSimpleTerm[U2, V2], E, TypeValue[T, U2, V2, W2]]) =
    app(this, argValues)    
}

object TypeValue
{
  def fromTypeLiteralValue[T, U, V, W](value: frontend.TypeLiteralValue): TypeValue[T, U, V, W] =
    value match {
      case frontend.TupleTypeFunValue(n)    => TupleTypeFunValue[T, U, V, W](n)
      case frontend.TypeBuiltinFunValue(bf) => throw new UnsupportedOperationException
    }
}

case class NoTypeValue[T, +U, +V, +W](err: FatalError) extends TypeValue[T, U, V, W]

object NoTypeValue
{
  def fromError[T, U, V, W](err: FatalError) = NoTypeValue[T, U, V, W](err)
}

case class EvaluatedTypeValue[T, +U, +V, +W](term: TypeValueTerm[T]) extends TypeValue[T, U, V, W]
case class TupleTypeFunValue[T, +U, +V, +W](n: Int) extends TypeValue[T, U, V, W]
case class TypeBuiltinFunValue[T, +U, +V, +W](bf: TypeBuiltinFunction.Value, f: TypeFunction) extends TypeValue[T, U, V, W]
case class TypeCombinatorValue[T, +U, +V, +W](comb: AbstractTypeCombinator[U, V], sym: GlobalSymbol) extends TypeValue[T, U, V, W]
case class TypeLambdaValue[T, +U, +V, +W](lambda: TypeLambda[U, V], closure: W) extends TypeValue[T, U, V, W]
case class TypePartialAppValue[T, +U, +V, +W](funValue: TypeValue[T, U, V, W], args: Seq[TypeValue[T, U, V, W]]) extends TypeValue[T, U, V, W]
case class TypeLazyValue[T, +U, +V, +W](term: Term[TypeSimpleTerm[U, V]], closure: W) extends TypeValue[T, U, V, W]

sealed trait TypeValueTerm[T]

case class TupleType[T](argTerms: Seq[TypeValueTerm[T]]) extends TypeValueTerm[T]
case class BuiltinType[T](bf: TypeBuiltinFunction.Value, argTerms: Seq[TypeValueTerm[T]]) extends TypeValueTerm[T]
case class Unittype[T](loc: T, argsTerms: Seq[TypeValueTerm[T]]) extends TypeValueTerm[T]
case class GlobalTypeApp[T](loc: T, argTerms: Seq[TypeValueLambda[T]]) extends TypeValueTerm[T]
case class TypeParamApp[T](param: Int, argTerms: Seq[TypeValueLambda[T]]) extends TypeValueTerm[T]
case class TypeConjunction[T](terms: Set[TypeValueTerm[T]]) extends TypeValueTerm[T]
case class TypeDisjunction[T](terms: Set[TypeValueTerm[T]]) extends TypeValueTerm[T]

case class TypeValueLambda[T](argParams: Seq[Int], body: TypeValueTerm[T])