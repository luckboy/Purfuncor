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

sealed trait Term[+T]
{
  def pos: Position
}
case class App[+T](fun: Term[T], args: NonEmptyList[Term[T]], pos: Position) extends Term[T]
case class Simple[+T](simpleTerm: T, pos: Position) extends Term[T]
