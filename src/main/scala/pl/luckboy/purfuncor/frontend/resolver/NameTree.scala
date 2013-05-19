package pl.luckboy.purfuncor.frontend.resolver

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
}

case class NameTable(
    combNames: Set[String],
    moduleNames: Set[String])
    
object NameTable
{
  val empty = NameTable(Set(), Set())
}