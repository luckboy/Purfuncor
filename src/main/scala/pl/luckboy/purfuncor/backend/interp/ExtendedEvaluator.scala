/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.backend.interp
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

trait ExtendedEvaluator[I, E, V]
{
  def variableS(value: V, lambdaInfo: I)(env: E): (E, V)
  
  def constructS(n: Int, lambdaInfo: I)(env: E): (E, V)
  
  def selectS[T, U](value: V, cases: Seq[Case[T, I, U]], lambdaInfo: I)(env: E): (E, Validation[V, Case[T, I, U]])
}
