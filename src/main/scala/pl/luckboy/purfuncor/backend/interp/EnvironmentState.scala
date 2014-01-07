/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.backend.interp
import pl.luckboy.purfuncor.frontend.resolver.NameTree

trait EnvironmentState[E, L, V, W]
{
  def nameTreeFromEnvironmentS(env: E): (E, NameTree)
  
  def globalVarValueFromEnvironmentS(loc: L)(env: E): (E, V)
  
  def currentLocalInstanceValuesFromEnvironmentS(env: E): (E, Seq[W])
}
