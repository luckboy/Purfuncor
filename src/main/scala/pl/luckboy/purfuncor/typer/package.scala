package pl.luckboy.purfuncor
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol
import pl.luckboy.purfuncor.common.Evaluator._

package object typer
{
  implicit def symbolTypeSimpleTermEvaluator[T]: Evaluator[TypeSimpleTerm[Symbol, T], SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]] = new Evaluator[TypeSimpleTerm[Symbol, T], SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]] {
    override def evaluateSimpleTermS(simpleTerm: TypeSimpleTerm[Symbol, T])(env: SymbolTypeEnvironment[T]) =
      simpleTerm match {
        case lambda: TypeLambda[Symbol, T] =>
          (env, TypeLambdaValue(lambda, env.currentTypeClosure))
        case TypeVar(loc)                  =>
          (env, env.typeVarValue(loc))
        case TypeLiteral(value)            =>
          TypeValue.fromTypeLiteralValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]](value) match {
            case TypeBuiltinFunValue(_, f) if f.argCount === 0 => f.applyS(Nil)(env)
            case value2                                        => (env, value2)
          }
        case KindedTypeTerm(term, _)       =>
          evaluateS(term)(env)
      }
    
    override def valueFromTermS(term: Term[TypeSimpleTerm[Symbol, T]])(env: SymbolTypeEnvironment[T]) =
      (env, TypeLazyValue(term, env.currentTypeClosure))
    
    override def valueArgCount(value: TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]) = value.argCount
    
    override def fullyAppS(funValue: TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]], argValues: Seq[TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]])(env: SymbolTypeEnvironment[T]): (SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]) =
      throw new UnsupportedOperationException
      
    override def partiallyAppS(funValue: TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]], argValues: Seq[TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]])(env: SymbolTypeEnvironment[T]): (SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]) =
      throw new UnsupportedOperationException
      
    override def isNoValue(value: TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]) = value.isNoTypeValue
      
    override def forceS(value: TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]])(env: SymbolTypeEnvironment[T]): (SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]) =
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