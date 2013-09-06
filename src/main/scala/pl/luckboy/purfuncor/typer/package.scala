package pl.luckboy.purfuncor
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol

package object typer
{
  implicit def symbolTypeSimpleTermEvaluator[T]: Evaluator[TypeSimpleTerm[Symbol, T], SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]] = new Evaluator[TypeSimpleTerm[Symbol, T], SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]] {
    override def evaluateSimpleTermS(simpleTerm: TypeSimpleTerm[Symbol, T])(env: SymbolTypeEnvironment[T]): (SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]) =
      throw new UnsupportedOperationException
    
    override def valueFromTermS(term: Term[TypeSimpleTerm[Symbol, T]])(env: SymbolTypeEnvironment[T]): (SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]) =
      throw new UnsupportedOperationException
    
    override def valueArgCount(value: TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]): Int =
      throw new UnsupportedOperationException
      
    override def fullyAppS(funValue: TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]], argValues: Seq[TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]])(env: SymbolTypeEnvironment[T]): (SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]) =
      throw new UnsupportedOperationException
      
    override def partiallyAppS(funValue: TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]], argValues: Seq[TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]])(env: SymbolTypeEnvironment[T]): (SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]) =
      throw new UnsupportedOperationException
      
    override def isNoValue(value: TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]): Boolean =
      throw new UnsupportedOperationException
      
    override def withPos(res: (SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]))(pos: Position): (SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]) =
      throw new UnsupportedOperationException
  }
  
  implicit def symbolTypeCombinatorInitializer[T] = new Initializer[NoTypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]], GlobalSymbol, AbstractTypeCombinator[Symbol, T], SymbolTypeEnvironment[T]] {
    override def globalVarsFromEnvironmentS(env: SymbolTypeEnvironment[T]): (SymbolTypeEnvironment[T], Set[GlobalSymbol]) =
      throw new UnsupportedOperationException
      
    override def usedGlobalVarsFromCombinator(comb: AbstractTypeCombinator[Symbol, T]): Set[GlobalSymbol] =
      throw new UnsupportedOperationException
      
    override def prepareGlobalVarS(loc: GlobalSymbol)(env: SymbolTypeEnvironment[T]): (SymbolTypeEnvironment[T], Unit) =
      throw new UnsupportedOperationException
      
    override def initializeGlobalVarS(loc: GlobalSymbol, comb: AbstractTypeCombinator[Symbol, T])(env: SymbolTypeEnvironment[T]): (SymbolTypeEnvironment[T], Validation[NoTypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]], Unit]) =
      throw new UnsupportedOperationException
      
    override def checkEnvironmentS(env: SymbolTypeEnvironment[T]): (SymbolTypeEnvironment[T], Validation[NoTypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]], Unit]) =
      throw new UnsupportedOperationException
      
    override def undefinedGlobalVarError: NoTypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]] =
      throw new UnsupportedOperationException
      
    override def withSaveS[U, V](f: SymbolTypeEnvironment[T] => (SymbolTypeEnvironment[T], Validation[U, V]))(env: SymbolTypeEnvironment[T]): (SymbolTypeEnvironment[T], Validation[U, V]) =
      throw new UnsupportedOperationException
  }
}