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
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

trait TreeInfoTransformer[T[_, _], U, V, W]
{
  def transformTreeInfo[X, Y, E](treeInfo: T[lmbdindexer.TypeLambdaInfo[X], Y])(env: E)(implicit inferrer: Inferrer[TypeSimpleTerm[U, lmbdindexer.TypeLambdaInfo[X]], E, Kind], envSt: KindInferenceEnvironmentState[E, V], enval: KindInferenceEnvironmental[E, V, W]): ValidationNel[AbstractError, T[TypeLambdaInfo[X, W], TypeTreeInfo[Y, V]]]
}
