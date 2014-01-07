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
import pl.luckboy.purfuncor.common.Arrow

object KindTermUtils
{
  def kindParamsFromKindTerm[T](term: KindTerm[StarKindTerm[T]]): Set[T] =
    term match {
      case Arrow(arg, ret, _)        => kindParamsFromKindTerm(arg) | kindParamsFromKindTerm(ret)
      case Star(KindParam(param), _) => Set(param)
      case _                         => Set()
    }
  
  private def intKindTermFromKindTermForParamsS[T](kindTerm: KindTerm[StarKindTerm[T]])(params: Map[T, Int]): (Map[T, Int], KindTerm[StarKindTerm[Int]]) =
    kindTerm match {
      case Arrow(arg, ret, pos) =>
        val (params2, arg2) = intKindTermFromKindTermForParamsS(arg)(params)
        val (params3, ret2) = intKindTermFromKindTermForParamsS(ret)(params2)
        (params3, Arrow(arg2, ret2, pos))
      case Star(KindType, pos) =>
        (params, Star(KindType, pos))
      case Star(KindParam(param), pos) =>
        params.get(param).map {
          param2 => (params, Star(KindParam(param2), pos))
        }.getOrElse((params + (param -> params.size), Star(KindParam(params.size), pos)))
    }
  
  def intKindTermFromKindTerm[T](kindTerm: KindTerm[StarKindTerm[T]]) =
    intKindTermFromKindTermForParamsS(kindTerm)(Map())._2
}
