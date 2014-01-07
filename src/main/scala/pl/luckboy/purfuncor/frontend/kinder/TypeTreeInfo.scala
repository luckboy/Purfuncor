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

case class TypeTreeInfo[T, U](treeInfo: T, kindTable: InferredKindTable[U])
{
  override def toString = 
    (if(!treeInfo.toString.isEmpty) treeInfo + "\n" else "") +
    "//// kindTable\n" +
    kindTable.kinds.map { case (l, k) => "// " + l + ": " + k + "\n" }.mkString("\n")
}
