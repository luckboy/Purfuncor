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

object Terms
{
  def namedArgsFromArgs(args: List[Arg[TypeSimpleTerm[Symbol, TypeLambdaInfo]]]) =
    args.zipWithIndex.map { case (a, i) => a.copy(name = some(a.name.getOrElse("$x" + (i + 1)))) }
  
  def varsFromArgs(args: List[Arg[TypeSimpleTerm[Symbol, TypeLambdaInfo]]]) =
    args.zipWithIndex.map { case (a, i) => Simple(Var(NormalSymbol(NonEmptyList(a.name.getOrElse("$x" + (i + 1))), a.pos), LambdaInfo), a.pos) }
    
  def namedTypeArgsFromTypeArgs(args: List[TypeArg]) =
    args.zipWithIndex.map { case (a, i) => a.copy(name = some(a.name.getOrElse("$t" + (i + 1)))) }
  
  def typeVarsFromTypeArgs(args: List[TypeArg]) =
    args.zipWithIndex.map { case (a, i) => Simple(TypeVar(NormalSymbol(NonEmptyList(a.name.getOrElse("$t" + (i + 1))), a.pos)), a.pos) }
  
  def renamedTypeArgsFromTypeArgs(args: List[TypeArg], prefix: String) =
    args.zipWithIndex.map { case (a, i) => a.copy(name = some(prefix + (i + 1))) }
  
  def renamedTypeVarsFromTypeArgs(args: List[TypeArg], prefix: String) =
    typeVarsFromTypeArgs(renamedTypeArgsFromTypeArgs(args, prefix))
}
