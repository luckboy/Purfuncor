/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.instant
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.LambdaInfoExtractor

object LambdaInfoUtils
{
  def usedGlobalVarsFromLambdaInfo[T, U](lambdaInfo: T)(implicit lambdaInfoExtractor: LambdaInfoExtractor[T, Instance[U]]) =
    lambdaInfoExtractor.instancesFromLambdaInfo(lambdaInfo).flatMap {
      case PolyFunInstance(loc, _, _) => Seq(loc)
      case _                          => Seq[U]()
    }.toSet
}
