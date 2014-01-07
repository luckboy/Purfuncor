/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.resolver
import scalaz._
import scalaz.Scalaz._

sealed trait Symbol
{
  override def toString =
    this match {
      case GlobalSymbol(names) =>"#." + names.list.mkString(".")
      case LocalSymbol(name)   => name
    }
}

case class GlobalSymbol(names: NonEmptyList[String]) extends Symbol
{
  def ++ (names: List[String]) = GlobalSymbol(this.names :::> names)
  
  def moduleSymbol = ModuleSymbol(names.list.init)
}
case class LocalSymbol(name: String) extends Symbol
