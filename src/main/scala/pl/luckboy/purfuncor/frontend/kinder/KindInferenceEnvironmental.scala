/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.kinder
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol

trait KindInferenceEnvironmental[E, L, M]
{
  def copyEnvironment(env: E): E
  
  def globalTypeVarKindFromEnvironment(env: E)(sym: GlobalSymbol): Kind
  
  def localKindTablesFromEnvironment(env: E)(sym: Option[GlobalSymbol]): Map[Int, KindTable[LocalSymbol]]
  
  def globalKindTableFromEnvironment(env: E): KindTable[L]
  
  def withCurrentTypeCombinatorLocation(env: E)(loc: Option[L]): E
  
  def getLocalKindTableFromEnvironment(env: E)(lambdaIdx: Int): Option[KindTable[M]]
}
