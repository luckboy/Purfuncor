/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol

trait TypeEnvironmental[T, U]
{
  def globalTypeVarValueFromEnvironment(env: T)(sym: GlobalSymbol): U
  
  def withPartialEvaluation[U](env: T)(isPartial: Boolean)(f: T => (T, U)): (T, U)
}
