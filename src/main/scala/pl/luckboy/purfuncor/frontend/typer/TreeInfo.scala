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

case class TreeInfo[+T, U, V](treeInfo: T, typeTable: InferredTypeTable[U, V])
{
  override def toString =
    (if(!treeInfo.toString.isEmpty) treeInfo + "\n" else "") +
    "//// typeTables\n" +
    typeTable.types.map { case (l, t) => "// " + l + ": " + t + "\n" }.mkString("\n")
}
