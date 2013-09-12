package pl.luckboy.purfuncor.frontend
import scala.annotation.tailrec
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol
import pl.luckboy.purfuncor.common.Evaluator._
import pl.luckboy.purfuncor.frontend.resolver.TermUtils._

package object typer
{
  implicit def symbolTypeEnvironmentState[T] = new TypeEnvironmentState[SymbolTypeEnvironment[T]] {
    override def withTypeParamsS[U](paramCount: Int)(f: (Int, Int, SymbolTypeEnvironment[T]) => (SymbolTypeEnvironment[T], U))(env: SymbolTypeEnvironment[T]) =
      env.withTypeParams(paramCount)(f)
  }
  
  implicit def symbolTypeSimpleTermEvaluator[T]: Evaluator[TypeSimpleTerm[Symbol, T], SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]] = new Evaluator[TypeSimpleTerm[Symbol, T], SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]] {
    override def evaluateSimpleTermS(simpleTerm: TypeSimpleTerm[Symbol, T])(env: SymbolTypeEnvironment[T]) =
      simpleTerm match {
        case lambda: TypeLambda[Symbol, T] =>
          (env, TypeLambdaValue(lambda, env.currentTypeClosure, env.currentFile))
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
      (env, TypeLazyValue(term, env.currentTypeClosure, env.currentFile))
    
    override def valueArgCount(value: TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]) = value.argCount
    
    override def fullyAppS(funValue: TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]], argValues: Seq[TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]])(env: SymbolTypeEnvironment[T]): (SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]) =
      funValue match {
        case TypeCombinatorValue(comb: TypeCombinator[Symbol, T], _, _) =>
          if(comb.args.size === argValues.size) {
            val localTypeVarValues = comb.args.zip(argValues).flatMap {
              case (TypeArg(Some(name), _, _), v) => List((LocalSymbol(name), v))
              case (_, _)                         => Nil
            }.toMap
            val (env2, retValue) = env.withTypeClosure(SymbolTypeClosure(Map())) {
              _.withLocalTypeVars(localTypeVarValues) { newEnv => evaluateS(comb.body)(newEnv) }
            }
            (env2, retValue.forFile(comb.file))
          } else
            (env, NoTypeValue.fromError(FatalError("illegal number of type arguments", none, NoPosition)))
        case TypeCombinatorValue(comb: UnittypeCombinator[Symbol, T], loc, sym) =>
          TypeValue.fullyAppForUnittypeCombinatorS(comb, loc, sym, argValues)(env)
        case TypeLambdaValue(lambda, closure, file) =>
          if(lambda.args.size === argValues.size) {
            val localTypeVarValues = lambda.args.list.zip(argValues).flatMap {
              case (TypeArg(Some(name), _, _), v) => List((LocalSymbol(name), v))
              case (_, _)                         => Nil
            }.toMap
            val (env2, retValue) = env.withTypeClosure(closure) {
              _.withLocalTypeVars(localTypeVarValues) { newEnv => evaluateS(lambda.body)(newEnv) }
            }
            (env2, retValue.forFile(file))
          } else
            (env, NoTypeValue.fromError(FatalError("illegal number of type arguments", none, NoPosition)))
        case TypePartialAppValue(funValue2, argValues2) =>
          if(funValue2.argCount - argValues2.size === argValues.size)
            fullyAppS(funValue2, argValues2 ++ argValues)(env)
          else
            (env, NoTypeValue.fromError(FatalError("illegal number of type arguments", none, NoPosition)))
        case tupleTypeFunValue @ TupleTypeFunValue(_) =>
          tupleTypeFunValue.fullyApplyS(argValues)(env)
        case TypeBuiltinFunValue(_, f) =>
          if(f.argCount === argValues.size)
            f.applyS(argValues)(env)
          else
            (env, NoTypeValue.fromError(FatalError("illegal number of type arguments", none, NoPosition)))
        case EvaluatedTypeValue(globalTypeApp: GlobalTypeApp[GlobalSymbol]) =>
          if(argValues.size === 1) {
            val (env2, res) = TypeValueLambda.typeValueLambdasFromTypeValuesS(argValues)(env)
            (env2, res.map { tvls => EvaluatedTypeValue(globalTypeApp.copy(args = globalTypeApp.args ++ tvls)) }.valueOr(identity))
          } else
            (env, NoTypeValue.fromError(FatalError("illegal number of type arguments", none, NoPosition)))
        case EvaluatedTypeValue(typeParamApp: TypeParamApp[GlobalSymbol]) =>
          if(argValues.size === 1) {
            val (env2, res) = TypeValueLambda.typeValueLambdasFromTypeValuesS(argValues)(env)
            (env2, res.map { tvls => EvaluatedTypeValue(typeParamApp.copy(args = typeParamApp.args ++ tvls)) }.valueOr(identity))
          } else
            (env, NoTypeValue.fromError(FatalError("illegal number of type arguments", none, NoPosition)))
        case _ =>
          (env, NoTypeValue.fromError(FatalError("no applicable", none, NoPosition)))
      }
      
    override def partiallyAppS(funValue: TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]], argValues: Seq[TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]])(env: SymbolTypeEnvironment[T]) =
      (env, TypePartialAppValue(funValue, argValues))
      
    override def isNoValue(value: TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]) = value.isNoTypeValue
    
    @tailrec
    override final def forceS(value: TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]])(env: SymbolTypeEnvironment[T]): (SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]) =
      value match {
        case TypeLazyValue(term, closure, file) =>
          val (env2, evaluatedValue) = env.withTypeClosure(closure) { evaluateS(term)(_) }
          forceS(evaluatedValue.forFile(file))(env2)
        case value =>
          (env, value)
      }
    
    override def withPos(res: (SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]))(pos: Position) =
      (res._1, res._2.withPos(pos))
  }
  
  implicit def symbolTypeCombinatorInitializer[T] = new Initializer[NoTypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]], GlobalSymbol, AbstractTypeCombinator[Symbol, T], SymbolTypeEnvironment[T]] {
    override def globalVarsFromEnvironmentS(env: SymbolTypeEnvironment[T]) = (env, env.globalTypeVarValues.keySet)
      
    override def usedGlobalVarsFromCombinator(comb: AbstractTypeCombinator[Symbol, T]) =
      comb match {
        case TypeCombinator(_, _, body, _, _) => usedGlobalTypeVarsFromTypeTerm(body)
        case UnittypeCombinator(_, _, _)      => Set()
      }
      
    override def prepareGlobalVarS(loc: GlobalSymbol)(env: SymbolTypeEnvironment[T]) =
      (env.withGlobalTypeVar(loc, EvaluatedTypeValue(GlobalTypeApp(loc, Nil, loc))), ())
      
    override def initializeGlobalVarS(loc: GlobalSymbol, comb: AbstractTypeCombinator[Symbol, T])(env: SymbolTypeEnvironment[T]) = {
      val (env2, value) = if(comb.argCount === 0) {
        comb match {
          case TypeCombinator(_, _, body, _, file) =>
            val (newEnv, value) = env.withFile(file) { evaluateS(body)(_) }
            (newEnv, value.forFile(file))
          case UnittypeCombinator(_, _, _)         =>
            (env, EvaluatedTypeValue(Unittype(loc, Nil, loc)))
        }
      } else
        (env, TypeCombinatorValue(comb, loc, loc))
      value match {
        case noValue: NoTypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]] => (env, noValue.failure)
        case _                                                                   => (env.withGlobalTypeVar(loc, value), ().success)
      }
    }
      
    override def checkEnvironmentS(env: SymbolTypeEnvironment[T]) =
      (env, ().success[NoTypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]])
      
    override def undefinedGlobalVarError: NoTypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]] =
      NoTypeValue.fromError(FatalError("undefined global type variable", none, NoPosition))
      
    override def withSaveS[U, V](f: SymbolTypeEnvironment[T] => (SymbolTypeEnvironment[T], Validation[U, V]))(env: SymbolTypeEnvironment[T]) = {
      val (env2, res) = f(env)
      res.map { x => (env2, x.success) }.valueOr { e => (env, e.failure ) }      
    }
  }
  
  def symbolTypeEnvironmental[T] = new TypeEnvironmental[SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]] {
    override def globalTypeVarValueFromEnvironment(env: SymbolTypeEnvironment[T])(sym: GlobalSymbol) =
      env.typeVarValue(sym)
  }
}