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

case class TypeLambdaInfo[T, U](lambdaInfo: T, idx: Int, kindTable: InferredKindTable[U])
{
  override def toString = {
    (if(!lambdaInfo.toString.isEmpty) List(lambdaInfo.toString) else Nil) ++
    List("idx=" + idx) ++
    (if(!kindTable.kinds.isEmpty) List("kindTable=" + kindTable.kinds.map { case (l, k) => l + ": " + k }.mkString(",")) else Nil)
  }.mkString(";")
}
