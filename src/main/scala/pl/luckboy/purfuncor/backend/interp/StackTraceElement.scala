/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.backend.interp
import scala.util.parsing.input.Position
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol

case class StackTraceElement(file: Option[java.io.File], combSym: Option[GlobalSymbol], pos: Position)
{
  override def toString = file.map { _.getPath() }.getOrElse("<no file>") + ": " + combSym.map { _.toString }.getOrElse("<no combinator>") + ": " + pos
}
