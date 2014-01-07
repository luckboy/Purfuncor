/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._

package object lmbdindexer
{
  implicit val resolverTreeInfoTransformer: TreeInfoTransformer[resolver.TreeInfo] = new TreeInfoTransformer[resolver.TreeInfo] {
    override def transformTreeInfo[U, V](treeInfo: resolver.TreeInfo[U, V]): ValidationNel[AbstractError, resolver.TreeInfo[TypeLambdaInfo[U], V]] =
      (LambdaIndexer.transformTypeTree(treeInfo.typeTree) |@| LambdaIndexer.transformSelectConstructInstances(treeInfo.selectConstructInsts)) {
        (tt, scis) => resolver.TreeInfo(tt, treeInfo.insts, scis)
      }
  }
}
