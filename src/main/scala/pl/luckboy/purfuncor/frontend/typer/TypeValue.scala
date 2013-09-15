package pl.luckboy.purfuncor.frontend.typer
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend
import pl.luckboy.purfuncor.frontend.AbstractTypeCombinator
import pl.luckboy.purfuncor.frontend.TypeCombinator
import pl.luckboy.purfuncor.frontend.UnittypeCombinator
import pl.luckboy.purfuncor.frontend.TypeSimpleTerm
import pl.luckboy.purfuncor.frontend.TypeLambda
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.common.Evaluator._

sealed trait TypeValue[T, +U, +V, +W]
{
  def argCount: Int =
    this match {
      case NoTypeValue(_, _)                        => 1
      case EvaluatedTypeValue(_)                    => 1
      case TupleTypeFunValue(n)                     => n
      case TypeBuiltinFunValue(_, f)                => f.argCount
      case TypeCombinatorValue(comb, _, _)          => comb.argCount
      case TypeLambdaValue(lambda, _, _)            => lambda.args.size
      case TypePartialAppValue(funValue, argValues) => funValue.argCount - argValues.size
      case TypeLazyValue(_, _, _)                   => 1
    }
  
  def isNoTypeValue = this.isInstanceOf[NoTypeValue[T, U, V, W]]
  
  def apply[U2 >: U, V2 >: V, W2 >: W, E](argValues: Seq[TypeValue[T, U2, V2, W2]])(implicit eval: Evaluator[TypeSimpleTerm[U2, V2], E, TypeValue[T, U2, V2, W2]]) =
    app(this, argValues)
  
  def withPos(pos: Position): TypeValue[T, U, V, W] =
    this match {
      case NoTypeValue(err, false) => NoTypeValue(err.copy(pos = pos), true)
      case _                       => this
    }
    
  def forFile(file: Option[java.io.File]): TypeValue[T, U, V, W] =
    this match {
      case noValue @ NoTypeValue(err, _) => noValue.copy(err = err.copy(file = file))
      case _                             => this
    }
    
  def typeValueTermS[U2 >: U, V2 >: V, W2 >: W, E](env: E)(implicit eval: Evaluator[TypeSimpleTerm[U2, V2], E, TypeValue[T, U2, V2, W2]]) = {
    val (env2, evaluatedValue) = eval.forceS(this)(env)
    val term = evaluatedValue match {
      case EvaluatedTypeValue(term) => term.success
      case _                        => NoTypeValue.fromError[T, U, V, W](FatalError("unevaluated type value", none, NoPosition)).failure
    }
    (env2, term)
  }
  
  def typeValueLambdaS[U2 >: U, V2 >: V, W2 >: W, E](env: E)(implicit eval: Evaluator[TypeSimpleTerm[U2, V2], E, TypeValue[T, U2, V2, W2]], envSt: TypeEnvironmentState[E]): (E, Validation[NoTypeValue[T, U2, V2, W2], TypeValueLambda[T]]) = {
    val (env2, evaluatedValue) = eval.forceS(this)(env)
    evaluatedValue match {
      case EvaluatedTypeValue(term) =>
        (env, TypeValueLambda(Nil, term).success)
      case funValue @ (TypeCombinatorValue(_, _, _) | TypeLambdaValue(_, _, _)) =>
        envSt.withTypeParamsS(funValue.argCount) {
          (param1, paramN, newEnv) =>
            val paramValues = (param1 until paramN).map { i => EvaluatedTypeValue[T, U2, V2, W2](TypeParamApp(i, Nil)) }
            val (newEnv2, retValue) = appS(funValue, paramValues)(newEnv)
            retValue.typeValueLambdaS(newEnv2)
        } (env)
      case _ =>
        (env, NoTypeValue.fromError(FatalError("no applicable", none, NoPosition)).failure)
    }
  }
  
  override def toString =
    this match {
      case NoTypeValue(err, _)                      => "<no type value: " + err + ">"
      case EvaluatedTypeValue(term)                 => term.toString
      case TupleTypeFunValue(n)                     => "tuple " + n
      case TypeBuiltinFunValue(f, _)                => "#" + f 
      case TypeCombinatorValue(_, _, sym)           => sym.toString
      case TypeLambdaValue(_, _, _)                 => "<type lambda value>"
      case TypePartialAppValue(funValue, argValues) =>
        (List(funValue) ++ argValues).map {
          value =>
            value match {
              case _: TypePartialAppValue[T, U, V, W] => "(" + value + ")"
              case _                                  => value.toString
            }
        }.mkString(" ")
      case TypeLazyValue(_, _, _)                   => "<type lazy value>"
    }
}

object TypeValue
{
  def fromTypeLiteralValue[T, U, V, W](value: frontend.TypeLiteralValue): TypeValue[T, U, V, W] =
    value match {
      case frontend.TupleTypeFunValue(n)    => TupleTypeFunValue[T, U, V, W](n)
      case frontend.TypeBuiltinFunValue(bf) => TypeBuiltinFunValue.fromTypeBuiltinFunction[T, U, V, W](bf)
    }
  
  def fullyAppForUnittypeCombinatorS[T, U, V, W, E](comb: UnittypeCombinator[U, V], loc: T, sym: GlobalSymbol, argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) = 
    if(comb.n === argValues.size) {
      val (env2, res) = TypeValueTerm.typeValueTermsFromTypeValuesS(argValues)(env)
      (env2, res.map { ts => EvaluatedTypeValue(Unittype(loc, ts, sym)) }.valueOr(identity))
    } else
      (env, NoTypeValue.fromError[T, U, V, W](FatalError("illegal number of type arguments", none, NoPosition)))
}

case class NoTypeValue[T, +U, +V, +W](err: FatalError, hasPos: Boolean) extends TypeValue[T, U, V, W]

object NoTypeValue
{
  def fromError[T, U, V, W](err: FatalError) = NoTypeValue[T, U, V, W](err, false)
}

case class EvaluatedTypeValue[T, +U, +V, +W](term: TypeValueTerm[T]) extends TypeValue[T, U, V, W]

case class TupleTypeFunValue[T, +U, +V, +W](n: Int) extends TypeValue[T, U, V, W]
{
  def fullyApplyS[U2 >: U, V2 >: V, W2 >: W, E](argValues: Seq[TypeValue[T, U2, V2, W2]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U2, V2], E, TypeValue[T, U2, V2, W2]]): (E, TypeValue[T, U2, V2, W2]) =
    if(n === argValues.size) {
      val (env2, res) = TypeValueTerm.typeValueTermsFromTypeValuesS(argValues)(env)
      (env2, res.map { ts => EvaluatedTypeValue(TupleType(ts)) }.valueOr(identity))
    } else
      (env, NoTypeValue.fromError(FatalError("illegal number of type arguments", none, NoPosition)))
}

case class TypeBuiltinFunValue[T, +U, +V, +W](bf: frontend.TypeBuiltinFunction.Value, f: TypeFunction) extends TypeValue[T, U, V, W]

object TypeBuiltinFunValue
{
  def fromTypeBuiltinFunction[T, U, V, W](bf: frontend.TypeBuiltinFunction.Value) =
    TypeBuiltinFunctions.typeBuiltinFunctions.get(bf).map { TypeBuiltinFunValue[T, U, V, W](bf, _) }.getOrElse(NoTypeValue.fromError[T, U, V, W](FatalError("unsupported built-in type function", none, NoPosition)))
}

case class TypeCombinatorValue[T, +U, +V, +W](comb: AbstractTypeCombinator[U, V], loc: T, sym: GlobalSymbol) extends TypeValue[T, U, V, W]
case class TypeLambdaValue[T, +U, +V, +W](lambda: TypeLambda[U, V], closure: W, file: Option[java.io.File]) extends TypeValue[T, U, V, W]
case class TypePartialAppValue[T, +U, +V, +W](funValue: TypeValue[T, U, V, W], args: Seq[TypeValue[T, U, V, W]]) extends TypeValue[T, U, V, W]
case class TypeLazyValue[T, +U, +V, +W](term: Term[TypeSimpleTerm[U, V]], closure: W, file: Option[java.io.File]) extends TypeValue[T, U, V, W]

sealed trait TypeValueTerm[T]
{
  def & (term: TypeValueTerm[T]) =
    (this, term) match {
      case (TypeConjunction(terms1), TypeConjunction(terms2)) => TypeConjunction(terms1 | terms2)
      case (TypeConjunction(terms), _)                        => TypeConjunction(terms + term)
      case (_, TypeConjunction(terms))                        => TypeConjunction(terms + this)
      case (_, _)                                             => TypeConjunction(Set(this) | Set(term))
    }
  
  def | (term: TypeValueTerm[T]) =
    (this, term) match {
      case (TypeDisjunction(terms1), TypeDisjunction(terms2)) => TypeDisjunction(terms1 | terms2)
      case (TypeDisjunction(terms), _)                        => TypeDisjunction(terms + term)
      case (_, TypeDisjunction(terms))                        => TypeDisjunction(terms + this)
      case (_, _)                                             => TypeDisjunction(Set(this) | Set(term))
    }
  
  def toArgString =
    this match {
      case TupleType(args) if !args.isEmpty           => "(" + this + ")"
      case BuiltinType(_, args) if !args.isEmpty      => "(" + this + ")"
      case Unittype(_, args, _) if !args.isEmpty      => "(" + this + ")"
      case GlobalTypeApp(_, args, _) if !args.isEmpty => "(" + this + ")"
      case TypeParamApp(_, args) if !args.isEmpty     => "(" + this + ")"
      case TypeConjunction(terms) if terms.size >= 2  => "(" + this + ")"
      case TypeDisjunction(terms) if terms.size >= 2  => "(" + this + ")"
      case _                                          => toString
    }
  
  override def toString =
    this match {
      case TupleType(Seq(arg))         => "tuple 1 " + arg.toArgString
      case TupleType(args)             => "(" + args.mkString(", ") + ")"
      case BuiltinType(bf, args)       => "#" + bf + args.map { " " + _.toArgString }.mkString("")
      case Unittype(_, args, sym)      => sym.toString + args.map { " " + _.toArgString }.mkString("")
      case GlobalTypeApp(_, args, sym) => sym.toString + args.map { " " + _.toArgString }.mkString("")
      case TypeParamApp(param, args)   => "t" + param + args.map { " " + _.toArgString }.mkString("")
      case TypeConjunction(terms)      => if(!terms.isEmpty) terms.mkString("#&") else "<type conjunction without terms>"
      case TypeDisjunction(terms)      => if(!terms.isEmpty) terms.mkString("#|") else "<type disjunction without terms>"
    }
}

object TypeValueTerm
{
  def typeValueTermsFromTypeValuesS[T, U, V, W, E](values: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
    values.foldLeft((env, Seq[TypeValueTerm[T]]().success[NoTypeValue[T, U, V, W]])) {
      case ((newEnv, Success(ts)), v)      => v.typeValueTermS(newEnv).mapElements(identity, _.map { ts :+ _ })
      case ((newEnv, Failure(noValue)), _) => (newEnv, noValue.failure)
    }
  
  def typeValueTermFromTypeValues[T, U, V, W, E](values: Seq[TypeValue[T, U, V, W]])(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
    State(typeValueTermsFromTypeValuesS[T, U, V, W, E](values))
}

case class TupleType[T](args: Seq[TypeValueTerm[T]]) extends TypeValueTerm[T]
case class BuiltinType[T](bf: TypeBuiltinFunction.Value, args: Seq[TypeValueTerm[T]]) extends TypeValueTerm[T]
case class Unittype[T](loc: T, args: Seq[TypeValueTerm[T]], sym: GlobalSymbol) extends TypeValueTerm[T]
case class GlobalTypeApp[T](loc: T, args: Seq[TypeValueLambda[T]], sym : GlobalSymbol) extends TypeValueTerm[T]
case class TypeParamApp[T](param: Int, args: Seq[TypeValueLambda[T]]) extends TypeValueTerm[T]
case class TypeConjunction[T](terms: Set[TypeValueTerm[T]]) extends TypeValueTerm[T]
case class TypeDisjunction[T](terms: Set[TypeValueTerm[T]]) extends TypeValueTerm[T]

case class TypeValueLambda[T](argParams: Seq[Int], body: TypeValueTerm[T])
{
  def toArgString = if(!argParams.isEmpty) "(" + this + ")" else body.toArgString
  
  override def toString = if(!argParams.isEmpty) "\\" + argParams.map { "t" + _ }.mkString(" ") + " => " + body else body.toString
}

object TypeValueLambda
{
  def typeValueLambdasFromTypeValuesS[T, U, V, W, E](values: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]], envSt: TypeEnvironmentState[E]) =
    values.foldLeft((env, Seq[TypeValueLambda[T]]().success[NoTypeValue[T, U, V, W]])) {
      case ((newEnv, Success(ls)), v)      => v.typeValueLambdaS(newEnv).mapElements(identity, _.map { ls :+ _ })
      case ((newEnv, Failure(noValue)), _) => (newEnv, noValue.failure)
    }

  def typeValueLambdasFromTypeValues[T, U, V, W, E](values: Seq[TypeValue[T, U, V, W]])(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]], envSt: TypeEnvironmentState[E]) =
    State(typeValueLambdasFromTypeValuesS[T, U, V, W, E](values))
}