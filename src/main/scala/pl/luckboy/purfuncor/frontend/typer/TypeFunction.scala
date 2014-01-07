/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

abstract class TypeFunction(val argCount: Int)
{
  def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]): (E, TypeValue[T, U, V, W])

  def apply[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
    State(applyS[T, U, V, W, E](argValues))
}
