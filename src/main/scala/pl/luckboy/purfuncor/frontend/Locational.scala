/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend

trait Locational[T, U, V]
{
  def getGlobalLocationFromLocation(loc: T): Option[U]
  
  def getLocalLocationFromLocation(loc: T): Option[V]
}
