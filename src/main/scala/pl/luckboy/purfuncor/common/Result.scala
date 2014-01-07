/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.common
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._

object Result
{
  def resultForFile[T](res: ValidationNel[AbstractError, T], file: Option[java.io.File]) =
    res.swapped { _.map { _.map { _.withFile(file) } } }
 
  def resultWithPos[T](res: ValidationNel[AbstractError, T], pos: Position) =
    res.swapped { _.map { _.map { _.withPos(pos) } } }
}
