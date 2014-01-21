/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.parser
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

object SyntaxSugar
{
  def makeString(s: String) = {
    (pos: Position) => makeArray(s.map { c => Simple(Literal(CharValue(c)), pos) }.toList)(pos)
  }
  
  def makeTuple(terms: List[Term[SimpleTerm[Symbol, LambdaInfo, TypeSimpleTerm[Symbol, TypeLambdaInfo]]]]) = {
    (pos: Position) =>
      terms.toNel.map { App(Simple(Literal(TupleFunValue(terms.size)), pos), _, pos) }.getOrElse(Simple(Literal(TupleFunValue(0)), pos))
  }
  
  def makeArray(terms: List[Term[SimpleTerm[Symbol, LambdaInfo, TypeSimpleTerm[Symbol, TypeLambdaInfo]]]]) = {
    (pos: Position) =>
      terms.toNel.map { App(Simple(Literal(MakearrayFunValue(terms.size)), pos), _, pos) }.getOrElse(Simple(Literal(MakearrayFunValue(0)), pos))
  }
  
  def makeList(terms: List[Term[SimpleTerm[Symbol, LambdaInfo, TypeSimpleTerm[Symbol, TypeLambdaInfo]]]]) = {
    (pos: Position) =>
      val consSym = GlobalSymbol(NonEmptyList("predef", "::"), pos)
      val nilSym = GlobalSymbol(NonEmptyList("predef", "Nil"), pos)
      val consTerm = Simple(Var(consSym, LambdaInfo), pos)
      val nilTerm = Simple(Var(nilSym, LambdaInfo), pos)
      App(Simple(Literal(MakelistFunValue(terms.size)), pos), consTerm :: terms <::: NonEmptyList(nilTerm), pos)
  }
    
  def makeIfElse(condTerm: Term[SimpleTerm[Symbol, LambdaInfo, TypeSimpleTerm[Symbol, TypeLambdaInfo]]], firstTerm: Term[SimpleTerm[Symbol, LambdaInfo, TypeSimpleTerm[Symbol, TypeLambdaInfo]]], secondTerm: Term[SimpleTerm[Symbol, LambdaInfo, TypeSimpleTerm[Symbol, TypeLambdaInfo]]]) = {
    (pos: Position) =>
      App(Simple(Literal(BuiltinFunValue(BuiltinFunction.Cond)), pos), NonEmptyList(
          Simple(Lambda(NonEmptyList(Arg(none, none, firstTerm.pos)), firstTerm, LambdaInfo), firstTerm.pos),
          Simple(Lambda(NonEmptyList(Arg(none, none, secondTerm.pos)), secondTerm, LambdaInfo), secondTerm.pos),
          condTerm), pos)
  }
  
  def makeTypeTuple(terms: List[Term[TypeSimpleTerm[Symbol, TypeLambdaInfo]]]) = {
    (pos: Position) =>
      terms.toNel.map { App(Simple(TypeLiteral(TupleTypeFunValue(terms.size)), pos), _, pos) }.getOrElse(Simple(TypeLiteral(TupleTypeFunValue(0)), pos))
  }
}
