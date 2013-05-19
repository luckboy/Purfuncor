package pl.luckboy.purfuncor.frontend.resolver

case class NameTree(nameTables: Map[ModuleSymbol, NameTable])
{
  def getNameTable(sym: ModuleSymbol) =
    nameTables.get(sym)
  
  def containsCombinator(sym: Symbol) =
    getNameTable(ModuleSymbol(sym.names.list.init)).map { _.combNames.contains(sym.names.list.last) }.getOrElse(false)

  def containsModile(sym: ModuleSymbol) =
    nameTables.contains(sym)
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