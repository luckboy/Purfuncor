/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.instant
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.typer.InferredType

case class PreinstantiationLambdaInfo[+T, U](
    polyFun: Option[AbstractPolyFunction[T]],
    polyFunType: Option[InferredType[U]],
    combTypeParams: Map[Int, Int],
    isCase: Boolean,
    pos: Position,
    file: Option[java.io.File])
   
object PreinstantiationLambdaInfo
{
  def fromLambdaInfo[T, U, V, W](lambdaInfo: typer.LambdaInfo[T, U, V]) = PreinstantiationLambdaInfo[W, V](
      polyFun = none,
      polyFunType = lambdaInfo.polyFunType,
      combTypeParams = lambdaInfo.combTypeParams,
      isCase = false,
      pos = NoPosition,
      file = none)
}
