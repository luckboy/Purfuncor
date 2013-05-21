package pl.luckboy.purfuncor.frontend.resolver
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.common.Tree

case class NameTree(nameTabs: Map[ModuleSymbol, NameTable])
{
  def getNameTable(sym: ModuleSymbol) =
    nameTabs.get(sym)
  
  def containsCombinator(sym: GlobalSymbol) =
    getNameTable(ModuleSymbol(sym.names.list.init)).map { _.combNames.contains(sym.names.list.last) }.getOrElse(false)

  def containsModule(sym: ModuleSymbol) =
    nameTabs.contains(sym)
}

object NameTree
{
  val empty = NameTree(Map())
  
  def fromTree[T, U](tree: Tree[GlobalSymbol, Combinator[GlobalSymbol, Symbol, T], U]) =
    tree.combs.keys.foldLeft(NameTree.empty) { _ |+| fromGlobalSymbol(_) }
  
  def fromGlobalSymbol(sym: GlobalSymbol) =
    fromModuleSymbol(sym.moduleSymbol) |+| NameTree(Map(ModuleSymbol(sym.names.list.init) -> NameTable(Set(sym.names.reverse.head), Set())))

  def fromModuleSymbol(sym: ModuleSymbol) = {
    val nameTabs = sym.names.init.zip(sym.names.init.inits.toList.tail).map {
      case (name, parentNames) => ModuleSymbol(parentNames) -> NameTable(Set(), Set(name))
    }.toMap
    NameTree(nameTabs)
  }
  
}

case class NameTable(
    combNames: Set[String],
    moduleNames: Set[String])
    
object NameTable
{
  val empty = NameTable(Set(), Set())
}