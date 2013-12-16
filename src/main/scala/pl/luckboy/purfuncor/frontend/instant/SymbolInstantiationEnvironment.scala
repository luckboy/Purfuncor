package pl.luckboy.purfuncor.frontend.instant
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol
import pl.luckboy.purfuncor.frontend.kinder.TypeLambdaInfo
import pl.luckboy.purfuncor.frontend.typer.SymbolTypeInferenceEnvironment
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.common.RecursiveInitializer._

case class SymbolInstantiationEnvironment[T, U](
    typeInferenceEnv: SymbolTypeInferenceEnvironment[T, U],
    globalInstTree: InstanceTree[AbstractPolyFunction[GlobalSymbol], GlobalSymbol, GlobalInstance[GlobalSymbol]],
    lambdaInfos: Map[Option[GlobalSymbol], Map[Int, InstantiationLambdaInfo[GlobalSymbol, GlobalSymbol]]],
    combNodes: Map[GlobalSymbol, CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]],
    recursiveCombSyms: Set[GlobalSymbol],
    errs: List[AbstractError],
    isRecursive: Boolean)
{
  def withTypeInferenceEnv(env: SymbolTypeInferenceEnvironment[T, U]) = copy(typeInferenceEnv = env)
  
  def withLambdaInfos(lambdaInfos: Map[Option[GlobalSymbol], Map[Int, InstantiationLambdaInfo[GlobalSymbol, GlobalSymbol]]]) = copy(lambdaInfos = lambdaInfos)
  
  def withCombNodes(nodes: Map[GlobalSymbol, CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]]) = copy(combNodes = nodes)
  
  def withComb(sym: GlobalSymbol, node: CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]) = copy(combNodes = combNodes + (sym -> node))
  
  def withoutCombs(syms: Set[GlobalSymbol]) = copy(combNodes = combNodes -- syms)
  
  def withRecursiveCombSyms(syms: Set[GlobalSymbol]) = copy(recursiveCombSyms = syms) 
  
  def withRecursive(isRecursive: Boolean) = copy(isRecursive = isRecursive)
}