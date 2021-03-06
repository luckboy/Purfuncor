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

case class LambdaInfo[+T, U, V](
    lambdaInfo: T,
    idx: Int,
    typeTable: InferredTypeTable[U, V],
    polyFunType: Option[InferredType[V]],
    combTypeParams: Map[Int, Int])
{
  override def toString = {
    (if(!lambdaInfo.toString.isEmpty) List(lambdaInfo.toString) else Nil) ++
    List("idx=" + idx) ++
    (if(!typeTable.types.isEmpty) List("typeTable=" + typeTable.types.map { case (l, t) => l + ": " + t }.mkString(",")) else Nil) ++
    polyFunType.map { "polyFunType=" + _ } ++
    (if(!combTypeParams.isEmpty) List("combTypeParams=" + combTypeParams.map { p => p._1 + "->" + p._2 }.mkString(",")) else Nil)
  }.mkString(";")
}
