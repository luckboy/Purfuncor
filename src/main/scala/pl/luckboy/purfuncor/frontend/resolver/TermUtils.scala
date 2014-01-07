/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.resolver
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

object TermUtils
{
  def usedGlobalVarsFromTerm[T, U](term: Term[SimpleTerm[Symbol, T, U]]): Set[GlobalSymbol] =
    term match {
      case App(fun, args, _)                    => usedGlobalVarsFromTerm(fun) | args.list.flatMap(usedGlobalVarsFromTerm).toSet
      case Simple(Let(binds, body, _), _)       => binds.list.flatMap { b => usedGlobalVarsFromTerm(b.body) }.toSet | usedGlobalVarsFromTerm(body)
      case Simple(Lambda(_, body, _), _)        => usedGlobalVarsFromTerm(body)
      case Simple(Var(sym: GlobalSymbol, _), _) => Set(sym)
      case Simple(TypedTerm(term2, _), _)       => usedGlobalVarsFromTerm(term2)
      case Simple(_, _)                         => Set()
    }
  
  def usedGlobalTypeVarsFromTypeTerm[T](term: Term[TypeSimpleTerm[Symbol, T]]): Set[GlobalSymbol] =
    term match {
      case App(fun, args, _)                     => usedGlobalTypeVarsFromTypeTerm(fun) | args.list.flatMap(usedGlobalTypeVarsFromTypeTerm).toSet
      case Simple(TypeLambda(_, body, _), _)     => usedGlobalTypeVarsFromTypeTerm(body)
      case Simple(TypeVar(sym: GlobalSymbol), _) => Set(sym)
      case Simple(KindedTypeTerm(term2, _), _)   => usedGlobalTypeVarsFromTypeTerm(term2)
      case Simple(_, _)                          => Set()
    }
}
