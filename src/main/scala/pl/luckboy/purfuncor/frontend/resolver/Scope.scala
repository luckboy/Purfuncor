package pl.luckboy.purfuncor.frontend.resolver
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend._

case class Scope(
    currentModuleSyms: NonEmptyList[ModuleSymbol],
    importedCombSyms: Map[String, NonEmptyList[GlobalSymbol]],
    importedModuleSyms: Map[String, NonEmptyList[ModuleSymbol]],
    localVarNames: Set[String],
    nameTree: NameTree)
{
  def withCurrentModule(sym: ModuleSymbol) = copy(currentModuleSyms = sym <:: currentModuleSyms)

  def withImportedCombinators(syms: Map[String, GlobalSymbol]) = copy(importedCombSyms = importedCombSyms |+| syms.mapValues { NonEmptyList(_) })
  
  def withImportedModules(syms: Map[String, ModuleSymbol]) = copy(importedModuleSyms = importedModuleSyms |+| syms.mapValues { NonEmptyList(_) })
  
  def withLocalVariables(names: Set[String]) = copy(localVarNames = localVarNames ++ names)
}

object Scope
{
  def fromNameTree(nameTree: NameTree) = Scope(
      currentModuleSyms = NonEmptyList(ModuleSymbol.root),
      importedCombSyms = Map(),
      importedModuleSyms = Map(),
      localVarNames = Set(),
      nameTree = nameTree)
}