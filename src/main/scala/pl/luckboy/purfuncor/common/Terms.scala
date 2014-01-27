/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.common
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._

object Terms
{
  def app[T](fun: Term[T], args: List[Term[T]], pos: Position) =
    args.toNel.map { App(fun, _, pos) }.getOrElse(fun)
}
