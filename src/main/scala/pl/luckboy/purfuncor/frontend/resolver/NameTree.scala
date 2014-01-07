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
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.common.Tree

case class NameTree(nameTables: Map[ModuleSymbol, NameTable])
{
  def getNameTable(sym: ModuleSymbol) =
    nameTables.get(sym)
  
  def containsComb(sym: GlobalSymbol) =
    getNameTable(ModuleSymbol(sym.names.list.init)).map { _.combNames.contains(sym.names.list.last) }.getOrElse(false)
    
  def containsTypeComb(sym: GlobalSymbol) =
    getNameTable(ModuleSymbol(sym.names.list.init)).map { _.typeCombNames.contains(sym.names.list.last) }.getOrElse(false)

  def containsModule(sym: ModuleSymbol) =
    nameTables.contains(sym)
}

object NameTree
{
  val empty = NameTree(Map())
  
  def fromTree[T, U](tree: Tree[GlobalSymbol, T, U]) = fromGlobalSymbols(tree.combs.keys)
  
  def fromGlobalSymbols(syms: Iterable[GlobalSymbol]) = syms.foldLeft(NameTree.empty) { _ |+| fromGlobalSymbol(_) }
  
  def fromGlobalSymbol(sym: GlobalSymbol) =
    fromModuleSymbol(sym.moduleSymbol) |+| NameTree(Map(ModuleSymbol(sym.names.list.init) -> NameTable(Set(sym.names.reverse.head), Set(), Set())))
    
  def fromTypeGlobalSymbols(syms: Iterable[GlobalSymbol]) = syms.foldLeft(NameTree.empty) { _ |+| fromTypeGlobalSymbol(_) }

  def fromTypeGlobalSymbol(sym: GlobalSymbol) =
    fromModuleSymbol(sym.moduleSymbol) |+| NameTree(Map(ModuleSymbol(sym.names.list.init) -> NameTable(Set(), Set(sym.names.reverse.head), Set())))

  def fromModuleSymbol(sym: ModuleSymbol) = {
    val nameTables = sym.names.reverse match {
      case Nil           =>
        Map[ModuleSymbol, NameTable]()
      case name :: names =>
        (name :: names).reverse.zip(names.reverse.inits.toList.reverse).map {
      	  case (name, parentNames) => ModuleSymbol(parentNames) -> NameTable(Set(), Set(), Set(name))
        }.toMap
    }
    NameTree(nameTables)
  }
}

case class NameTable(
    combNames: Set[String],
    typeCombNames: Set[String],
    moduleNames: Set[String])
    
object NameTable
{
  val empty = NameTable(Set(), Set(), Set())
}
