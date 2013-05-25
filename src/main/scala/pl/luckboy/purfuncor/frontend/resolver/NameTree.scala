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

  def containsModule(sym: ModuleSymbol) =
    nameTables.contains(sym)
}

object NameTree
{
  val empty = NameTree(Map())
  
  def fromTree[T, U](tree: Tree[GlobalSymbol, T, U]) =
    tree.combs.keys.foldLeft(NameTree.empty) { _ |+| fromGlobalSymbol(_) }
  
  def fromGlobalSymbol(sym: GlobalSymbol) =
    fromModuleSymbol(sym.moduleSymbol) |+| NameTree(Map(ModuleSymbol(sym.names.list.init) -> NameTable(Set(sym.names.reverse.head), Set())))

  def fromModuleSymbol(sym: ModuleSymbol) = {
    val nameTables = sym.names.reverse match {
      case Nil           => Map[ModuleSymbol, NameTable]()
      case name :: names =>
        names.reverse.zip(names.reverse.inits.toList.tail).map {
      	  case (name, parentNames) => ModuleSymbol(parentNames) -> NameTable(Set(), Set(name))
        }.toMap
    }
    NameTree(nameTables)
  }
  
}

case class NameTable(
    combNames: Set[String],
    moduleNames: Set[String])
    
object NameTable
{
  val empty = NameTable(Set(), Set())
}