package pl.luckboy.purfuncor.frontend
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol
import pl.luckboy.purfuncor.frontend.kinder.TypeLambdaInfo
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.common.RecursiveInitializer._

package object instant
{
  implicit def symbolCombinatorInstanceRecursiveInitialzer[T, U]: RecursiveInitializer[NonEmptyList[AbstractError], GlobalSymbol, AbstractCombinator[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]], CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol], SymbolInstantiationEnvironment[T, U]] = new RecursiveInitializer[NonEmptyList[AbstractError], GlobalSymbol, AbstractCombinator[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]], CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol], SymbolInstantiationEnvironment[T, U]] {
    override def combinatorFromNode(node: CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]): AbstractCombinator[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]] =
      throw new UnsupportedOperationException
    
    override def recursiveCombinatorsFromNode(node: CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]): Set[GlobalSymbol] =
      throw new UnsupportedOperationException
    
    override def markedRecursiveCombinatorsFromNode(node: CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]): Set[GlobalSymbol] =
      throw new UnsupportedOperationException
    
    override def createNode(comb: AbstractCombinator[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]], recursiveCombLocs: Set[GlobalSymbol], markedRecCombLocs: Set[GlobalSymbol]): CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol] =
      throw new UnsupportedOperationException
    
    override def addNodeS(loc: GlobalSymbol, node: CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol])(env: SymbolInstantiationEnvironment[T, U]): (SymbolInstantiationEnvironment[T, U], Unit) =
      throw new UnsupportedOperationException
    
    override def isRecursiveFromEnvironmentS(env: SymbolInstantiationEnvironment[T, U]): (SymbolInstantiationEnvironment[T, U], Boolean) =
      throw new UnsupportedOperationException
    
    override def isUninitializedGlobalVarS(loc: GlobalSymbol)(env: SymbolInstantiationEnvironment[T, U]): (SymbolInstantiationEnvironment[T, U], Boolean) =
      throw new UnsupportedOperationException
  
    override def nonRecursivelyInitializeGlobalVarS(loc: GlobalSymbol, comb: AbstractCombinator[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]])(env: SymbolInstantiationEnvironment[T, U]): (SymbolInstantiationEnvironment[T, U], ValidationNel[AbstractError, Unit]) =
      throw new UnsupportedOperationException
    
    override def checkInitializationS(res: ValidationNel[AbstractError, Unit], combLocs: Set[GlobalSymbol], oldNodes: Map[GlobalSymbol, CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]])(env: SymbolInstantiationEnvironment[T, U]): (SymbolInstantiationEnvironment[T, U], ValidationNel[AbstractError, Unit]) =
      throw new UnsupportedOperationException
    
    override def nodesFromEnvironmentS(env: SymbolInstantiationEnvironment[T, U]): (SymbolInstantiationEnvironment[T, U], Map[GlobalSymbol, CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]]) =
      throw new UnsupportedOperationException
    
    override def withRecursiveS[V](combLocs: Set[GlobalSymbol], newNodes: Map[GlobalSymbol, CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]])(f: SymbolInstantiationEnvironment[T, U] => (SymbolInstantiationEnvironment[T, U], V))(env: SymbolInstantiationEnvironment[T, U]): (SymbolInstantiationEnvironment[T, U], V) =
      throw new UnsupportedOperationException
    
    override def withClearS[V](f: SymbolInstantiationEnvironment[T, U] => (SymbolInstantiationEnvironment[T, U], V))(env: SymbolInstantiationEnvironment[T, U]): (SymbolInstantiationEnvironment[T, U], V) =
      throw new UnsupportedOperationException
  }
  
  implicit def symbolCombinatorInstanceInitializer[T, U]: Initializer[NonEmptyList[AbstractError], GlobalSymbol, AbstractCombinator[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]], SymbolInstantiationEnvironment[T, U]] = new Initializer[NonEmptyList[AbstractError], GlobalSymbol, AbstractCombinator[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]], SymbolInstantiationEnvironment[T, U]] {
    override def globalVarsFromEnvironmentS(env: SymbolInstantiationEnvironment[T, U]): (SymbolInstantiationEnvironment[T, U], Set[GlobalSymbol]) =
      throw new UnsupportedOperationException
    
    override def usedGlobalVarsFromCombinator(comb: AbstractCombinator[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]]): Set[GlobalSymbol] =
      throw new UnsupportedOperationException
    
    override def prepareGlobalVarS(loc: GlobalSymbol)(env: SymbolInstantiationEnvironment[T, U]): (SymbolInstantiationEnvironment[T, U], Unit) =
      throw new UnsupportedOperationException
    
    override def initializeGlobalVarS(loc: GlobalSymbol, comb: AbstractCombinator[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]])(env: SymbolInstantiationEnvironment[T, U]): (SymbolInstantiationEnvironment[T, U], ValidationNel[AbstractError, Unit]) =
      throw new UnsupportedOperationException
    
    override def checkEnvironmentS(env: SymbolInstantiationEnvironment[T, U]): (SymbolInstantiationEnvironment[T, U], ValidationNel[AbstractError, Unit]) =
      throw new UnsupportedOperationException
    
    override def undefinedGlobalVarError: NonEmptyList[AbstractError] =
      throw new UnsupportedOperationException
    
    override def withSaveS[V, W](f: SymbolInstantiationEnvironment[T, U] => (SymbolInstantiationEnvironment[T, U], Validation[V, W]))(env: SymbolInstantiationEnvironment[T, U]): (SymbolInstantiationEnvironment[T, U], Validation[V, W]) =
      throw new UnsupportedOperationException
  }
}