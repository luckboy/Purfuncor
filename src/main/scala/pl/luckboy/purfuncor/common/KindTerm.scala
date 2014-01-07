/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.common
import scala.util.parsing.input.Position

sealed trait KindTerm[+T]
{
  def pos: Position
}
case class Arrow[+T](arg: KindTerm[T], ret: KindTerm[T], pos: Position) extends KindTerm[T]
case class Star[+T](starKind: T, pos: Position) extends KindTerm[T]
