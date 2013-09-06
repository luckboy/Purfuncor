package pl.luckboy.purfuncor.typer
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol

sealed trait TypeValue[T, +U, +V, +W]

case class NoTypeValue[T, +U, +V, +W]() extends TypeValue[T, U, V, W]
case class EvaluatedTypeValue[T, +U, +V, +W](term: TypeValueTerm[T]) extends TypeValue[T, U, V, W]
case class TypeBuiltinFunValue[T, +U, +V, +W](bf: TypeBuiltinFunction.Value, f: TypeFunction) extends TypeValue[T, U, V, W]
case class TypeCombinatorValue[T, +U, +V, +W](comb: AbstractTypeCombinator[U, V], sym: GlobalSymbol) extends TypeValue[T, U, V, W]
case class TypeLambdaValue[T, +U, +V, +W](lambda: TypeLambda[U, V], closure: V) extends TypeValue[T, U, V, W]
case class TypePartialAppValue[T, +U, +V, +W](funValue: TypeValue[T, U, V, W], args: Seq[TypeValue[T, U, V, W]]) extends TypeValue[T, U, V, W]
case class TypeLazyValue[T, +U, +V, +W](term: Term[TypeSimpleTerm[U, V]], closure: W) extends TypeValue[T, U, V, W]

sealed trait TypeValueTerm[T]

case class TupleType[T](terms: Seq[TypeValueTerm[T]]) extends TypeValueTerm[T]
case class BuiltinType[T](bf: TypeBuiltinFunction.Value, terms: Seq[TypeValueTerm[T]]) extends TypeValueTerm[T]
case class Unittype[T](loc: T, terms: Seq[TypeValueTerm[T]]) extends TypeValueTerm[T]
case class GlobalTypeVar[T](loc: T) extends TypeValueTerm[T]
case class TypeParam[T](param: Int) extends TypeValueTerm[T]
case class TypeConjunction[T](terms: Set[TypeValueTerm[T]]) extends TypeValueTerm[T]
case class TypeDisjunction[T](terms: Set[TypeValueTerm[T]]) extends TypeValueTerm[T]