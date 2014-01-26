/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

object Terms
{
  def boolean[T, U, V](x: Boolean, pos: Position) = Simple(Literal[T, U, V](BooleanValue(x)), pos)
  def char[T, U, V](x: Char, pos: Position) = Simple(Literal[T, U, V](CharValue(x)), pos)
  def byte[T, U, V](x: Byte, pos: Position) = Simple(Literal[T, U, V](ByteValue(x)), pos)
  def short[T, U, V](x: Short, pos: Position) = Simple(Literal[T, U, V](ShortValue(x)), pos)
  def int[T, U, V](x: Int, pos: Position) = Simple(Literal[T, U, V](IntValue(x)), pos)
  def long[T, U, V](x: Long, pos: Position) = Simple(Literal[T, U, V](LongValue(x)), pos)
  def float[T, U, V](x: Float, pos: Position) = Simple(Literal[T, U, V](FloatValue(x)), pos)
  def double[T, U, V](x: Double, pos: Position) = Simple(Literal[T, U, V](DoubleValue(x)), pos)
  def tupleFun[T, U, V](n: Int, pos: Position) = Simple(Literal[T, U, V](TupleFunValue(n)), pos)
  def tupleFieldFun[T, U, V](n: Int, i: Int, pos: Position) = Simple(Literal[T, U, V](TupleFieldFunValue(n, i)), pos)
  def makearrayFun[T, U, V](n: Int, pos: Position) = Simple(Literal[T, U, V](MakearrayFunValue(n)), pos)
  def makelistFun[T, U, V](n: Int, pos: Position) = Simple(Literal[T, U, V](MakelistFunValue(n)), pos)
  def fieldFun[T, U, V](i: Int, pos: Position) = Simple(Literal[T, U, V](FieldFunValue(i)), pos)
  def fieldsetFun[T, U, V](n: Int, pos: Position) = Simple(Literal[T, U, V](FieldsetFunValue(n)), pos)
  def fieldSetAppFun[T, U, V](n: Int, pos: Position) = Simple(Literal[T, U, V](FieldSetAppFunValue(n)), pos)
  def fieldswithFun[T, U, V](n: Int, is: List[Int], pos: Position) = Simple(Literal[T, U, V](FieldswithFunValue(n, is)), pos)
  def builtinFun[T, U, V](bf: BuiltinFunction.Value, pos: Position) = Simple(Literal[T, U, V](BuiltinFunValue(bf)), pos)
  
  def let[T, U, V](binds: NonEmptyList[Bind[T, U, V]], body: Term[SimpleTerm[T, U, V]], lambdaInfo: U, pos: Position) = Simple(Let(binds, body, lambdaInfo), pos)
  def lambda[T, U, V](args: NonEmptyList[Arg[V]], body: Term[SimpleTerm[T, U, V]], lambdaInfo: U, pos: Position) = Simple(Lambda(args, body, lambdaInfo), pos)
  def variable[T, U, V](loc: T, lambdaInfo: U, pos: Position) = Simple(Var[T, U, V](loc, lambdaInfo), pos)
  def typedTerm[T, U, V](term: Term[SimpleTerm[T, U, V]], typ: Term[V], pos: Position) = Simple(TypedTerm(term, typ), pos)
  def construct[T, U, V](n: Int, lambdaInfo: U, pos: Position) = Simple(Construct[T, U, V](n, lambdaInfo), pos)
  def select[T, U, V](term: Term[SimpleTerm[T, U, V]], cases: NonEmptyList[Case[T, U, V]], lambdaInfo: U, pos: Position) = Simple(Select(term, cases, lambdaInfo), pos)
  def extract[T, U, V](term: Term[SimpleTerm[T, U, V]], args: NonEmptyList[Arg[V]], body: Term[SimpleTerm[T, U, V]], lambdaInfo: U, pos: Position) = Simple(Extract(term, args, body, lambdaInfo), pos)
    
  def anyType[T, U](pos: Position) = Simple(TypeLiteral[T, U](TypeBuiltinFunValue(TypeBuiltinFunction.Any)), pos)
  def nothingType[T, U](pos: Position) = Simple(TypeLiteral[T, U](TypeBuiltinFunValue(TypeBuiltinFunction.Nothing)), pos)
  def zeroType[T, U](pos: Position) = Simple(TypeLiteral[T, U](TypeBuiltinFunValue(TypeBuiltinFunction.Zero)), pos)
  def nonZeroType[T, U](pos: Position) = Simple(TypeLiteral[T, U](TypeBuiltinFunValue(TypeBuiltinFunction.NonZero)), pos)
  def booleanType[T, U](pos: Position) = Simple(TypeLiteral[T, U](TypeBuiltinFunValue(TypeBuiltinFunction.Boolean)), pos)
  def charType[T, U](pos: Position) = Simple(TypeLiteral[T, U](TypeBuiltinFunValue(TypeBuiltinFunction.Char)), pos)
  def byteType[T, U](pos: Position) = Simple(TypeLiteral[T, U](TypeBuiltinFunValue(TypeBuiltinFunction.Byte)), pos)
  def shortType[T, U](pos: Position) = Simple(TypeLiteral[T, U](TypeBuiltinFunValue(TypeBuiltinFunction.Short)), pos)
  def intType[T, U](pos: Position) = Simple(TypeLiteral[T, U](TypeBuiltinFunValue(TypeBuiltinFunction.Int)), pos)
  def longType[T, U](pos: Position) = Simple(TypeLiteral[T, U](TypeBuiltinFunValue(TypeBuiltinFunction.Long)), pos)
  def floatType[T, U](pos: Position) = Simple(TypeLiteral[T, U](TypeBuiltinFunValue(TypeBuiltinFunction.Float)), pos)
  def doubleType[T, U](pos: Position) = Simple(TypeLiteral[T, U](TypeBuiltinFunValue(TypeBuiltinFunction.Double)), pos)
  def emptyType[T, U](pos: Position) = Simple(TypeLiteral[T, U](TypeBuiltinFunValue(TypeBuiltinFunction.Empty)), pos)
  def nonEmptyType[T, U](pos: Position) = Simple(TypeLiteral[T, U](TypeBuiltinFunValue(TypeBuiltinFunction.NonEmpty)), pos)
  def arrayTypeFun[T, U](pos: Position) = Simple(TypeLiteral[T, U](TypeBuiltinFunValue(TypeBuiltinFunction.Array)), pos)
  def funTypeFun[T, U](pos: Position) = Simple(TypeLiteral[T, U](TypeBuiltinFunValue(TypeBuiltinFunction.Fun)), pos)
  def typeConjFun[T, U](pos: Position) = Simple(TypeLiteral[T, U](TypeBuiltinFunValue(TypeBuiltinFunction.Conj)), pos)
  def typeDisjFun[T, U](pos: Position) = Simple(TypeLiteral[T, U](TypeBuiltinFunValue(TypeBuiltinFunction.Disj)), pos)
  def fieldSet1TypeFun[T, U](pos: Position) = Simple(TypeLiteral[T, U](TypeBuiltinFunValue(TypeBuiltinFunction.FieldSet1)), pos)
  def fieldSet2TypeFun[T, U](pos: Position) = Simple(TypeLiteral[T, U](TypeBuiltinFunValue(TypeBuiltinFunction.FieldSet2)), pos)
  def tupleTypeFun[T, U](n: Int, pos: Position) = Simple(TypeLiteral(TupleTypeFunValue(n)), pos)
  def fieldTypeFun[T, U](i: Int, pos: Position) = Simple(TypeLiteral(FieldTypeFunValue(i)), pos)
  
  def typeLambda[T, U](args: NonEmptyList[TypeArg], body: Term[TypeSimpleTerm[T, U]], lambdaInfo: U, pos: Position) = Simple(TypeLambda(args, body, lambdaInfo), pos)
  def typeVar[T, U](loc: T, pos: Position) = Simple(TypeVar[T, U](loc), pos)
  def kindedTypeTerm[T, U](term: Term[TypeSimpleTerm[T, U]], kind: KindTerm[StarKindTerm[String]], pos: Position) = Simple(KindedTypeTerm(term, kind), pos)

  def typeLambdaForArgList[T, U](args: List[TypeArg], body: Term[TypeSimpleTerm[T, U]], lambdaInfo: U, pos: Position) =
    args.toNel.map { as => Simple(TypeLambda(as, body, lambdaInfo), pos) }.getOrElse(body)

}
