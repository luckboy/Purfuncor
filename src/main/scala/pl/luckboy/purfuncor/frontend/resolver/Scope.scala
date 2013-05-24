package pl.luckboy.purfuncor.frontend.resolver
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend._

case class Scope(
    nameTree: NameTree,
    currentModuleSyms: NonEmptyList[ModuleSymbol],
    importedCombSyms: Map[String, Set[GlobalSymbol]],
    importedModuleSyms: Map[String, Set[ModuleSymbol]],
    localVarNames: Set[String])
{
  def withCurrentModule(sym: ModuleSymbol) = copy(currentModuleSyms = sym <:: currentModuleSyms)

  def withImportedCombs(syms: Map[String, GlobalSymbol]) = copy(importedCombSyms = importedCombSyms |+| syms.mapValues { Set(_) })
  
  def withImportedModules(syms: Map[String, ModuleSymbol]) = copy(importedModuleSyms = importedModuleSyms |+| syms.mapValues { Set(_) })
  
  def withLocalVars(names: Set[String]) = copy(localVarNames = localVarNames ++ names)  
}

object Scope
{
  def fromNameTree(nameTree: NameTree) = Scope(
      nameTree = nameTree,
      currentModuleSyms = NonEmptyList(ModuleSymbol.root),
      importedCombSyms = Map(),
      importedModuleSyms = Map(),
      localVarNames = Set())
}