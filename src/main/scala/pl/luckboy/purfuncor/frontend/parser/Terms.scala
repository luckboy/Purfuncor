/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.parser
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

object Terms {
  def namedArgsFromTypeArgs(args: List[Arg[TypeSimpleTerm[Symbol, TypeLambdaInfo]]]) =
    args.zip(1 to args.size).map { case (a, i) => a.copy(name = some(a.name.getOrElse("$x" + i))) }
  
  def namedArgsFromArgs(args: List[Arg[TypeSimpleTerm[Symbol, TypeLambdaInfo]]]) =
    args.zip(1 to args.size).map { case (a, i) => a.copy(name = some(a.name.getOrElse("$x" + i))) }

  def appForArgs(fun: Term[TypeSimpleTerm[Symbol, TypeLambdaInfo]], args: List[Arg[TypeSimpleTerm[Symbol, TypeLambdaInfo]]], pos: Position) =
    args.toNel.map { 
      as =>
        App(fun, as.zip(NonEmptyList.nel(1, (2 to as.size).toList)).map { 
          case (a, i) => Simple(Var(NormalSymbol(NonEmptyList(a.name.getOrElse("$x" + i)), a.pos), LambdaInfo), a.pos)
        }, pos)
    }.getOrElse(fun)

  def appForSymbolAndArgs(funSym: Symbol, args: List[Arg[TypeSimpleTerm[Symbol, TypeLambdaInfo]]], pos: Position) =
    appForArgs(Simple(TypeVar(funSym), pos), args, pos)
  
  def typeAppForSymbol(funSym: Symbol, args: NonEmptyList[Term[TypeSimpleTerm[Symbol, TypeLambdaInfo]]], pos: Position) =
    App(Simple(TypeVar(funSym), pos), args, pos)
  
  def namedTypeArgsFromTypeArgs(args: List[TypeArg]) =
    args.zip(1 to args.size).map { case (a, i) => a.copy(name = some(a.name.getOrElse("$t" + i))) }
    
  def typeAppForArgs(fun: Term[TypeSimpleTerm[Symbol, TypeLambdaInfo]], args: List[TypeArg], pos: Position) =
    args.toNel.map { 
      as =>
        App(fun, as.zip(NonEmptyList.nel(1, (2 to as.size).toList)).map { 
          case (a, i) => Simple(TypeVar(NormalSymbol(NonEmptyList(a.name.getOrElse("$t" + i)), a.pos)), a.pos)
        }, pos)
    }.getOrElse(fun)
    
  def typeAppForSymbolAndArgs(funSym: Symbol, args: List[TypeArg], pos: Position) =
    typeAppForArgs(Simple(TypeVar(funSym), pos), args, pos)

}
