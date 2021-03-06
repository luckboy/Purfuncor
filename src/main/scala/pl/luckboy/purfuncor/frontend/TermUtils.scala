/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

object TermUtils
{
  def lambdaInfosFromTerm[T, U, V](term: Term[SimpleTerm[T, U, V]]): Set[U] =
    term match {
      case App(fun, args, _)                              =>
        lambdaInfosFromTerm(fun) | args.list.flatMap { lambdaInfosFromTerm(_) }.toSet
      case Simple(Let(_, _, lambdaInfo), _)               =>
        Set(lambdaInfo)
      case Simple(Lambda(_, _, lambdaInfo), _)            =>
        Set(lambdaInfo)
      case Simple(Var(_, lambdaInfo), _)                  =>
        Set(lambdaInfo)
      case Simple(Construct(_, lambdaInfo), _)            =>
        Set(lambdaInfo)
      case Simple(Select(term2, cases, lambdaInfo), _)    =>
        (lambdaInfosFromTerm(term2) | cases.list.flatMap { c => lambdaInfosFromTerm(c.body) + c.lambdaInfo }.toSet) + lambdaInfo 
      case Simple(Extract(term2, _, body, lambdaInfo), _) =>
        (lambdaInfosFromTerm(term2) | lambdaInfosFromTerm(body)) + lambdaInfo
      case _                                              =>
        Set()
    }
}
