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
import pl.luckboy.purfuncor.frontend.typer.InferredType

case class InstanceArgTable[T, U](instArgs: Map[T, Seq[InstanceArg[T, U]]])

object InstanceArgTable
{
  def empty[T, U] = InstanceArgTable[T, U](Map())
}

case class InstanceArg[+T, U](polyFun: AbstractPolyFunction[T], typ: InferredType[U])
{
  override def toString = polyFun + ": " + typ
}
