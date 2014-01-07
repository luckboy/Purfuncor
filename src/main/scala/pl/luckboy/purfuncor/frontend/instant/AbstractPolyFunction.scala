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

sealed trait AbstractPolyFunction[+T]
{
  override def toString =
    this match {
      case PolyFunction(loc) => loc.toString
      case ConstructFunction => "construct"
      case SelectFunction    => "select"
    }
}

case class PolyFunction[+T](loc: T) extends AbstractPolyFunction[T]
case object ConstructFunction extends AbstractPolyFunction[Nothing]
case object SelectFunction extends AbstractPolyFunction[Nothing]
