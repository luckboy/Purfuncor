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

case class ModuleSymbol(names: List[String])
{
  def + (name: String) = ModuleSymbol(names :+ name)
  
  def ++ (names: List[String]) = ModuleSymbol(this.names ++ names)
  
  def globalSymbolFromName(name: String) = GlobalSymbol(toNel(names).map { _ :::> List(name) }.getOrElse(NonEmptyList(name)) )

  def globalSymbolFromNames(names: NonEmptyList[String]) = GlobalSymbol(this.names <::: names)
  
  override def toString = "#." + names.mkString(".")
}

object ModuleSymbol
{
  val root = ModuleSymbol(Nil)
}
