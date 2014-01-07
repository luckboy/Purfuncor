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

case class InferenceLambdaInfo[T, U](
    typeTable: TypeTable[T, U],
    polyFunType: Option[Type[U]],
    combTypeParams: Map[Int, Int])

case class TypeTable[T, U](types: Map[T, Type[U]])

object TypeTable
{
  def empty[T, U] = TypeTable[T, U](Map())
}
