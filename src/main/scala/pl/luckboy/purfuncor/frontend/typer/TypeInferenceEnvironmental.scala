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
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol

trait TypeInferenceEnvironmental[E, L, M, N]
{
  def copyEnvironment(env: E): E
  
  def globalVarTypeFromEnvironment(env: E)(sym: GlobalSymbol): Type[N]
  
  def lambdaInfosFromEnvironment(env: E)(sym: Option[GlobalSymbol]): Map[Int, InferenceLambdaInfo[LocalSymbol, GlobalSymbol]]
  
  def getLambdaInfoFromEnvironment(env: E)(lambdaIdx: Int): Option[InferenceLambdaInfo[M, N]]
  
  def globalTypeTableFromEnvironment(env: E): TypeTable[L, N]
  
  def withCurrentCombinatorLocation(env: E)(loc: Option[L]): E
}
