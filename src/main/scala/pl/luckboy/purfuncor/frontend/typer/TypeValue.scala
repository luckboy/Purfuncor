/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
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
      case NoTypeValue(_, _)                           => 1
      case EvaluatedTypeValue(_)                       => 1
      case EvaluatedTypeLambdaValue(lambda)            => lambda.argParams.size
      case TupleTypeFunValue(n)                        => n
      case FieldTypeFunValue(_)                        => 1
      case FieldsetTypeFunValue(_)                     => 1
      case TypeBuiltinFunValue(_, f)                   => f.argCount
      case TypeCombinatorValue(comb, _, _)             => comb.argCount
      case TypeLambdaValue(lambda, _, _, _)            => lambda.args.size
      case TypePartialAppValue(funValue, argValues, _) => funValue.argCount - argValues.size
      case TypeLazyValue(_, _, _)                      => 1
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
      case EvaluatedTypeValue(term)         =>
        term.success
      case EvaluatedTypeLambdaValue(lambda) =>
        lambda match {
          case TypeValueLambda(Seq(), body) => body.success
          case _                            => NoTypeValue.fromError[T, U, V, W](FatalError("type lambda value has arguments", none, NoPosition)).failure
        }
      case _                                =>
        NoTypeValue.fromError[T, U, V, W](FatalError("unevaluated type value", none, NoPosition)).failure
    }
    (env2, term)
  }
  
  def typeValueLambdaWithParamsS[U2 >: U, V2 >: V, W2 >: W, E](param1: Int, paramN: Int)(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U2, V2], E, TypeValue[T, U2, V2, W2]], envSt: TypeEnvironmentState[E, T, TypeValue[T, U2, V2, W2]]): (E, Validation[NoTypeValue[T, U2, V2, W2], TypeValueLambda[T]]) = {
    val (env2, evaluatedValue) = eval.forceS(this)(env)
    evaluatedValue match {
      case EvaluatedTypeValue(term) =>
        (env2, TypeValueLambda(param1 until paramN, term).success)
      case EvaluatedTypeLambdaValue(TypeValueLambda(argParams, body)) =>
        if((argParams.toSet & (param1 until paramN).toSet).isEmpty)
          (env2, TypeValueLambda(argParams ++ (param1 until paramN), body)success)
        else
          (env2, NoTypeValue.fromError(FatalError("conflict of type arguments", none, NoPosition)).failure)
      case TupleTypeFunValue(0) =>
        (env2, NoTypeValue.fromError(FatalError("no applicable", none, NoPosition)).failure)
      case funValue /*@ (TypeCombinatorValue(_, _, _) | TypeLambdaValue(_, _, _, _) | TypePartialAppValue(_, _, _) | TypeBuiltinFunValue(_, _) | TupleTypeFunValue(_) | FieldTypeFunValue(_) | FieldsetTypeFunValue(_))*/ =>
        envSt.withTypeParamsS(funValue.argCount) {
          (newParam1, newParamN, newEnv) =>
            val (newEnv2, paramAppIdx) = envSt.currentTypeParamAppIdxFromEnvironmentS(newEnv)
            val paramValues = (newParam1 until newParamN).map { i => EvaluatedTypeValue[T, U2, V2, W2](TypeParamApp(i, Nil, paramAppIdx)) }
            val (newEnv3, retValue) = appS(funValue, paramValues)(newEnv2)
            retValue.typeValueLambdaWithParamsS(param1, newParamN)(newEnv3)
        } (env2)
      //case _ =>
      //  (env2, NoTypeValue.fromError(FatalError("no applicable", none, NoPosition)).failure)
    }
  }

  def typeValueLambdaS[U2 >: U, V2 >: V, W2 >: W, E](env: E)(implicit eval: Evaluator[TypeSimpleTerm[U2, V2], E, TypeValue[T, U2, V2, W2]], envSt: TypeEnvironmentState[E, T, TypeValue[T, U2, V2, W2]]): (E, Validation[NoTypeValue[T, U2, V2, W2], TypeValueLambda[T]]) = {
    val (env2, paramCount) = envSt.typeParamCountFromEnvironmentS(env)
    typeValueLambdaWithParamsS[U2, V2, W2, E](paramCount, paramCount)(env2)
  }
  
  def forCombLoc(loc: Option[T]): TypeValue[T, U, V, W] = 
    this match {
      case lambdaValue: TypeLambdaValue[T, U, V, W]         => lambdaValue.copy(combLoc = loc)
      case partialAppValue: TypePartialAppValue[T, U, V, W] => partialAppValue.copy(combLoc = loc)
      case _                                                => this
    }
  
  override def toString =
    this match {
      case NoTypeValue(err, _)                         => "<no type value: " + err + ">"
      case EvaluatedTypeValue(term)                    => term.toString
      case EvaluatedTypeLambdaValue(lambda)            => lambda.toString
      case TupleTypeFunValue(n)                        => "tuple " + n
      case FieldTypeFunValue(i)                        => "## " + (i + 1)
      case FieldsetTypeFunValue(n)                     => "fieldset " + n
      case TypeBuiltinFunValue(f, _)                   => "#" + f 
      case TypeCombinatorValue(_, _, sym)              => sym.toString
      case TypeLambdaValue(_, _, _, _)                 => "<type lambda value>"
      case TypePartialAppValue(funValue, argValues, _) =>
        (List(funValue) ++ argValues).map {
          value =>
            value match {
              case _: TypePartialAppValue[T, U, V, W] => "(" + value + ")"
              case _                                  => value.toString
            }
        }.mkString(" ")
      case TypeLazyValue(_, _, _)                      => "<type lazy value>"
    }
}

object TypeValue
{
  def fromTypeLiteralValue[T, U, V, W](value: frontend.TypeLiteralValue): TypeValue[T, U, V, W] =
    value match {
      case frontend.TupleTypeFunValue(n)    => TupleTypeFunValue[T, U, V, W](n)
      case frontend.FieldTypeFunValue(i)    => FieldTypeFunValue[T, U, V, W](i)
      case frontend.FieldsetTypeFunValue(n) => FieldsetTypeFunValue[T, U, V, W](n)
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
case class EvaluatedTypeLambdaValue[T, +U, +V, +W](lambda: TypeValueLambda[T]) extends TypeValue[T, U, V, W]

case class TupleTypeFunValue[T, +U, +V, +W](n: Int) extends TypeValue[T, U, V, W]
{
  def fullyApplyS[U2 >: U, V2 >: V, W2 >: W, E](argValues: Seq[TypeValue[T, U2, V2, W2]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U2, V2], E, TypeValue[T, U2, V2, W2]]): (E, TypeValue[T, U2, V2, W2]) =
    if(n === argValues.size) {
      val (env2, res) = TypeValueTerm.typeValueTermsFromTypeValuesS(argValues)(env)
      (env2, res.map { ts => EvaluatedTypeValue(TupleType(ts)) }.valueOr(identity))
    } else
      (env, NoTypeValue.fromError(FatalError("illegal number of type arguments", none, NoPosition)))
}

case class FieldTypeFunValue[T, +U, +V, +W](i: Int) extends TypeValue[T, U, V, W]
{
  def fullyApplyS[U2 >: U, V2 >: V, W2 >: W, E](argValues: Seq[TypeValue[T, U2, V2, W2]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U2, V2], E, TypeValue[T, U2, V2, W2]]): (E, TypeValue[T, U2, V2, W2]) =
    argValues match {
      case Seq(argValue) => 
        val (env2, res) = argValue.typeValueTermS(env)
        (env2, res.map { t => EvaluatedTypeValue(FieldType(i, t)) }.valueOr(identity))
      case _             =>
        (env, NoTypeValue.fromError(FatalError("illegal number of type arguments", none, NoPosition)))
    }
}

case class FieldsetTypeFunValue[T, +U, +V, +W](n: Int) extends TypeValue[T, U, V, W]
{
  def fullyApplyS[U2 >: U, V2 >: V, W2 >: W, E](argValues: Seq[TypeValue[T, U2, V2, W2]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U2, V2], E, TypeValue[T, U2, V2, W2]]): (E, TypeValue[T, U2, V2, W2]) =
    argValues match {
      case Seq(argValue) =>
        val (env2, res) = argValue.typeValueTermS(env)
        (env2, res.map { t => EvaluatedTypeValue(FieldSetType(n, t)) }.valueOr(identity))
      case _             =>
        (env, NoTypeValue.fromError(FatalError("illegal number of type arguments", none, NoPosition)))
    }
}

case class TypeBuiltinFunValue[T, +U, +V, +W](bf: frontend.TypeBuiltinFunction.Value, f: TypeFunction) extends TypeValue[T, U, V, W]

object TypeBuiltinFunValue
{
  def fromTypeBuiltinFunction[T, U, V, W](bf: frontend.TypeBuiltinFunction.Value) =
    TypeBuiltinFunctions.typeBuiltinFunctions.get(bf).map { TypeBuiltinFunValue[T, U, V, W](bf, _) }.getOrElse(NoTypeValue.fromError[T, U, V, W](FatalError("unsupported built-in type function", none, NoPosition)))
}

case class TypeCombinatorValue[T, +U, +V, +W](comb: AbstractTypeCombinator[U, V], loc: T, sym: GlobalSymbol) extends TypeValue[T, U, V, W]
case class TypeLambdaValue[T, +U, +V, +W](lambda: TypeLambda[U, V], closure: W, combLoc: Option[T], file: Option[java.io.File]) extends TypeValue[T, U, V, W]
case class TypePartialAppValue[T, +U, +V, +W](funValue: TypeValue[T, U, V, W], args: Seq[TypeValue[T, U, V, W]], combLoc: Option[T]) extends TypeValue[T, U, V, W]
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
  
  def distributedTypeValueTerm =
    this match {
      case TypeConjunction(terms) =>
        val (typeDisjs, tmpOtherTerms) = terms.partition { 
          case TypeDisjunction(ts) => ts.size > 1
          case _                   => false
        }
        typeDisjs.headOption.flatMap {
          case TypeDisjunction(terms2) =>
            terms2.headOption.flatMap {
              term2 =>
                val typeConj2 = TypeConjunction(typeDisjs.tail | tmpOtherTerms)
                terms2.tail.headOption.map {
                  secondTerm2 =>
                    val typeDisj2 = if(terms2.tail.size > 1) TypeDisjunction(terms2.tail) else secondTerm2
                    (term2 & typeConj2) | (typeDisj2 & typeConj2)
                }
            }
          case _ =>
            none
        }
      case TypeDisjunction(terms) =>
        val (typeConjs, tmpOtherTerms) = terms.partition { 
          case TypeConjunction(ts) => ts.size > 1
          case _                   => false
        }
        typeConjs.headOption.flatMap {
          case TypeConjunction(terms2) =>
            terms2.headOption.flatMap {
              term2 =>
                val typeDisj2 = TypeDisjunction(typeConjs.tail | tmpOtherTerms)
                terms2.tail.headOption.map {
                  secondTerm2 =>
                    val typeConj2 = if(terms2.tail.size > 1) TypeConjunction(terms2.tail) else secondTerm2
                    (term2 | typeDisj2) & (typeConj2 | typeDisj2)
                } 
            }
          case _ =>
            none
        }
      case _ =>
        none
    }
  
  def isTypeParamApp = isInstanceOf[TypeParamApp[T]]
  
  def toArgString =
    this match {
      case TupleType(args) if args.size === 1         => "(" + this + ")"
      case FieldType(_, _)                            => "(" + this + ")"
      case FieldSetType(_, _)                         => "(" + this + ")"
      case BuiltinType(_, args) if !args.isEmpty      => "(" + this + ")"
      case Unittype(_, args, _) if !args.isEmpty      => "(" + this + ")"
      case GlobalTypeApp(_, args, _) if !args.isEmpty => "(" + this + ")"
      case TypeParamApp(_, args, _) if !args.isEmpty  => "(" + this + ")"
      case TypeConjunction(terms) if terms.size >= 2  => "(" + this + ")"
      case TypeDisjunction(terms) if terms.size >= 2  => "(" + this + ")"
      case _                                          => toString
    }
  
  override def toString =
    this match {
      case TupleType(Seq(arg))          => "tuple 1 " + arg.toArgString
      case TupleType(args)              => "(" + args.mkString(", ") + ")"
      case FieldType(i, term)           => "##" + (i + 1) + " " + term.toArgString 
      case FieldSetType(n, term)        => "fieldset " + n + " " + term.toArgString
      case BuiltinType(bf, args)        => 
        bf match {
          case TypeBuiltinFunction.Fun => 
            args.headOption.map { 
              arg =>
                val s = arg match {
                  case TypeConjunction(_) | TypeDisjunction(_)                       => "(" + arg + ")"
                  case BuiltinType(TypeBuiltinFunction.Fun, args2) if args.size >= 1 => "(" + arg + ")"
                  case _                                                             => arg.toString 
                }
                s + " #" + bf + args.tail.map { 
                  arg2 =>
                    val s2 = arg2 match {
                      case TypeConjunction(_) | TypeDisjunction(_) => "(" + arg2 + ")"
                      case _                                       => arg2.toString
                    }
                    " " + s2 
                }.mkString("")
            }.getOrElse { "##" + bf }
          case _                       =>
            (if(bf.toString.headOption.map { c => c.isLetter || c === '_' }.getOrElse(false)) "#" + bf else "##" + bf) +
            args.map { " " + _.toArgString }.mkString("")
        }
      case Unittype(_, args, sym)       => sym.toString + args.map { " " + _.toArgString }.mkString("")
      case GlobalTypeApp(_, args, sym)  => sym.toString + args.map { " " + _.toArgString }.mkString("")
      case TypeParamApp(param, args, _) => "t" + (param + 1) + args.map { " " + _.toArgString }.mkString("")
      case TypeConjunction(terms)       => if(!terms.isEmpty) terms.map { _.toArgString }.mkString(" #& ") else "<type conjunction without terms>"
      case TypeDisjunction(terms)       =>
        if(!terms.isEmpty)
          terms.map { 
            term => 
              term match {
                case TypeConjunction(_) => term.toString
                case _                  => term.toArgString
              }
          }.mkString(" #| ")
        else
          "<type disjunction without terms>"
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
    
  def appForGlobalTypeS[T, U, V, W, E](funLoc: T, argLambdas: Seq[TypeValueLambda[T]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]], envSt: TypeEnvironmentState[E, T, TypeValue[T, U, V, W]]) = {
    val (env2, funValue) = envSt.globalTypeVarValueFromEnvironmentS(funLoc)(env)
    val (env3, retValue) = if(!argLambdas.isEmpty) 
      appS(funValue, argLambdas.map { EvaluatedTypeLambdaValue(_) })(env2)
    else
      (env2, funValue)
    eval.forceS(retValue)(env3) match {
      case (env4, noType: NoTypeValue[T, U, V, W]) => (env4, noType.failure)
      case (env4, EvaluatedTypeValue(term))        => (env4, term.success)
      case (env4, _)                               => (env4, NoTypeValue.fromError[T, U, V, W](FatalError("unevaluated type value", none, NoPosition)).failure)
    }
  }
  
  def appForGlobalType[T, U, V, W, E](funLoc: T, argLambdas: Seq[TypeValueLambda[T]])(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]], envSt: TypeEnvironmentState[E, T, TypeValue[T, U, V, W]]) =
    State(appForGlobalTypeS[T, U, V, W, E](funLoc, argLambdas))
}

case class TupleType[T](args: Seq[TypeValueTerm[T]]) extends TypeValueTerm[T]
case class FieldType[T](i: Int, term: TypeValueTerm[T]) extends TypeValueTerm[T]
case class FieldSetType[T](n: Int, term: TypeValueTerm[T]) extends TypeValueTerm[T]
case class BuiltinType[T](bf: TypeBuiltinFunction.Value, args: Seq[TypeValueTerm[T]]) extends TypeValueTerm[T]
case class Unittype[T](loc: T, args: Seq[TypeValueTerm[T]], sym: GlobalSymbol) extends TypeValueTerm[T]
sealed trait TypeApp[T] extends TypeValueTerm[T]
{
  def args: Seq[TypeValueLambda[T]]
  
  def withArgs(args: Seq[TypeValueLambda[T]]): TypeApp[T]
}
case class GlobalTypeApp[T](loc: T, args: Seq[TypeValueLambda[T]], sym : GlobalSymbol) extends TypeApp[T]
{
  override def withArgs(args: Seq[TypeValueLambda[T]]) = copy(args = args)
}
case class TypeParamApp[T](param: Int, args: Seq[TypeValueLambda[T]], paramAppIdx: Int) extends TypeApp[T]
{
  override def withArgs(args: Seq[TypeValueLambda[T]]) = copy(args = args)
}
case class TypeConjunction[T](terms: Set[TypeValueTerm[T]]) extends TypeValueTerm[T]
case class TypeDisjunction[T](terms: Set[TypeValueTerm[T]]) extends TypeValueTerm[T]

case class TypeValueLambda[T](argParams: Seq[Int], body: TypeValueTerm[T])
{
  def toArgString = if(!argParams.isEmpty) "(" + this + ")" else body.toArgString
  
  override def toString = if(!argParams.isEmpty) "\\" + argParams.map { p => "t" + (p + 1) }.mkString(" ") + " => " + body else body.toString
}

object TypeValueLambda
{
  def typeValueLambdasFromTypeValuesS[T, U, V, W, E](values: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]], envSt: TypeEnvironmentState[E, T, TypeValue[T, U, V, W]]) =
    values.foldLeft((env, Seq[TypeValueLambda[T]]().success[NoTypeValue[T, U, V, W]])) {
      case ((newEnv, Success(ls)), v)      => v.typeValueLambdaS(newEnv).mapElements(identity, _.map { ls :+ _ })
      case ((newEnv, Failure(noValue)), _) => (newEnv, noValue.failure)
    }

  def typeValueLambdasFromTypeValues[T, U, V, W, E](values: Seq[TypeValue[T, U, V, W]])(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]], envSt: TypeEnvironmentState[E, T, TypeValue[T, U, V, W]]) =
    State(typeValueLambdasFromTypeValuesS[T, U, V, W, E](values))
}
