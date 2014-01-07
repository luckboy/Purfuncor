/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.instant
import scalaz._
import scalaz.Scalaz._

trait InstantiationEnvironmental[E, L, M]
{
  def copyEnvironment(env: E): E
  
  def getLambdaInfoFromEnvironment(env: E)(lambdaIdx: Int): Option[InstantiationLambdaInfo[L]]
  
  def withCurrentCombinatorLocation(env: E)(loc: Option[L]): E
  
  def treeGlobalInstanceTreeFromEnvironment(env: E): InstanceTree[AbstractPolyFunction[L], M, GlobalInstance[L]]
  
  def instanceArgTableFromFromEnvironment(env: E): InstanceArgTable[L, M]
}
