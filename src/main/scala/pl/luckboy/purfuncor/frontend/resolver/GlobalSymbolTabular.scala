/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.resolver
import scalaz._
import scalaz.Scalaz._

trait GlobalSymbolTabular[T, U]
{
  def getGlobalLocationFromTable(table: T)(sym: GlobalSymbol): Option[U]
  
  def getGlobalSymbolFromTable(table: T)(loc: U): Option[GlobalSymbol]
}
