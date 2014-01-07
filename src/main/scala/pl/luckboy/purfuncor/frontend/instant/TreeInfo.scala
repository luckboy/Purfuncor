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
import pl.luckboy.purfuncor.frontend.typer.InferredTypeTable

case class TreeInfo[+T, U, V](
    treeInfo: T,
    typeTable: InferredTypeTable[U, V],
    instTree: InstanceTree[AbstractPolyFunction[U], V, GlobalInstance[U]],
    instArgTable: InstanceArgTable[U, V])
{
  override def toString =
    (if(!treeInfo.toString.isEmpty) treeInfo + "\n" else "") +
    "//// typeTables\n" +
    typeTable.types.map { case (l, t) => "// " + l + ": " + t + "\n" }.mkString("\n") + "\n" +
    "//// instTree\n" +
    instTree.instTables.flatMap {
      case (pf, it) => it.pairs.map { case (t, i) => "// instance " + pf + " => " + i + "// " + t + "\n" }
    }.mkString("\n") + "\n" +
    "//// instArgTable\n" +
    instArgTable.instArgs.map { case (l, ias) => "// " + l + " // instArgs=" + ias.mkString(",") + "\n" }.mkString("\n")
}
