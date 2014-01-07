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
import pl.luckboy.purfuncor.frontend.kinder.Kind

trait KindInferrenceEnvironmentState[E, L]
{
  def globalTypeVarKindFromEnvironmentS(loc: L)(env: E): (E, Kind)
  
  def typeParamKindFromEnvironmentS(param: Int)(env: E): (E, Kind)
  
  def addTypeParamKindS(param: Int, kind: Kind)(env: E): (E, Unit)
  
  def unifyStarKindWithKindS(kind: Kind)(env: E): (E, Kind)
}
