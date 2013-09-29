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
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbolTabular
import pl.luckboy.purfuncor.frontend.kinder.SymbolKindInferenceEnvironment
import pl.luckboy.purfuncor.common.Evaluator._
import pl.luckboy.purfuncor.frontend.resolver.TermUtils._

package object typer
{
  //
  // A type interpreter.
  //
  
  implicit def symbolTypeEnvironmentState[T] = new TypeEnvironmentState[SymbolTypeEnvironment[T]] {
    override def typeParamCountFromEnvironmentS(env: SymbolTypeEnvironment[T]) = (env, env.typeParamCount)
    
    override def withTypeParamsS[U](paramCount: Int)(f: (Int, Int, SymbolTypeEnvironment[T]) => (SymbolTypeEnvironment[T], U))(env: SymbolTypeEnvironment[T]) =
      env.withTypeParams(paramCount)(f)
  }
  
  implicit def symbolTypeSimpleTermEvaluator[T]: Evaluator[TypeSimpleTerm[Symbol, T], SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]] = new Evaluator[TypeSimpleTerm[Symbol, T], SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]] {
    override def evaluateSimpleTermS(simpleTerm: TypeSimpleTerm[Symbol, T])(env: SymbolTypeEnvironment[T]) =
      simpleTerm match {
        case lambda: TypeLambda[Symbol, T] =>
          (env, TypeLambdaValue(lambda, env.currentTypeClosure, none, env.currentFile))
        case TypeVar(loc)                  =>
          loc match {
            case globalSym: GlobalSymbol if env.applyingCombSyms.contains(globalSym) =>
              (env, EvaluatedTypeValue(GlobalTypeApp(globalSym, Nil, globalSym)))
            case _                                                                   =>
              (env, env.typeVarValue(loc))
          }
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
        case TypeCombinatorValue(comb: TypeCombinator[Symbol, T], loc, _) =>
          if(comb.args.size === argValues.size) {
            val localTypeVarValues = comb.args.zip(argValues).flatMap {
              case (TypeArg(Some(name), _, _), v) => List((LocalSymbol(name), v))
              case (_, _)                         => Nil
            }.toMap
            val (env2, retValue) = env.withCombSym(loc) {
              _.withTypeClosure(SymbolTypeClosure(Map())) {
                _.withLocalTypeVars(localTypeVarValues) { newEnv => evaluateS(comb.body)(newEnv) }
              }
            }
            (env2, retValue.forFile(comb.file))
          } else
            (env, NoTypeValue.fromError(FatalError("illegal number of type arguments", none, NoPosition)))
        case TypeCombinatorValue(comb: UnittypeCombinator[Symbol, T], loc, sym) =>
          TypeValue.fullyAppForUnittypeCombinatorS(comb, loc, sym, argValues)(env)
        case TypeLambdaValue(lambda, closure, combLoc, file) =>
          if(lambda.args.size === argValues.size) {
            val localTypeVarValues = lambda.args.list.zip(argValues).flatMap {
              case (TypeArg(Some(name), _, _), v) => List((LocalSymbol(name), v))
              case (_, _)                         => Nil
            }.toMap
            val (env2, retValue) = env.withCombSyms(combLoc.toSet) {
              _.withTypeClosure(closure) {
                _.withLocalTypeVars(localTypeVarValues) { newEnv => evaluateS(lambda.body)(newEnv) }
              }
            }
            (env2, retValue.forFile(file))
          } else
            (env, NoTypeValue.fromError(FatalError("illegal number of type arguments", none, NoPosition)))
        case TypePartialAppValue(funValue2, argValues2, combLoc) =>
          if(funValue2.argCount - argValues2.size === argValues.size)
            env.withCombSyms(combLoc.toSet)(fullyAppS(funValue2, argValues2 ++ argValues))
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
      (env, TypePartialAppValue(funValue, argValues, none))
      
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
            (newEnv, value.forFile(file).forCombLoc(some(loc)))
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
  
  implicit def symbolTypeEnvironmental[T] = new TypeEnvironmental[SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]] {
    override def globalTypeVarValueFromEnvironment(env: SymbolTypeEnvironment[T])(sym: GlobalSymbol) =
      env.typeVarValue(sym)
  }
  
  implicit def symbolTypeEnvironmentGlobalSymbolTabular[T] = new GlobalSymbolTabular[SymbolTypeEnvironment[T], GlobalSymbol] {
    override def getGlobalLocationFromTable(table: SymbolTypeEnvironment[T])(sym: GlobalSymbol) = some(sym)

    override def getGlobalSymbolFromTable(table: SymbolTypeEnvironment[T])(sym: GlobalSymbol) = some(sym)
  }
  
  //
  // A type inferrer.
  //
  
  implicit def noTypeSemigroup[T]: Semigroup[NoType[T]] = new Semigroup[NoType[T]] {
    override def append(f1: NoType[T], f2: => NoType[T]): NoType[T] =
      throw new UnsupportedOperationException
  }
  
  implicit def symbolTypeValueTermUnifier[T, U]: Unifier[NoType[GlobalSymbol], TypeValueTerm[GlobalSymbol], SymbolTypeInferenceEnvironment[T, U], Int] = new Unifier[NoType[GlobalSymbol], TypeValueTerm[GlobalSymbol], SymbolTypeInferenceEnvironment[T, U], Int] {
    override def matchesTermsS[V](term1: TypeValueTerm[GlobalSymbol], term2: TypeValueTerm[GlobalSymbol])(z: V)(f: (Int, Either[Int, TypeValueTerm[GlobalSymbol]], V, SymbolTypeInferenceEnvironment[T, U]) => (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], V]))(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], V]) =
      throw new UnsupportedOperationException
    
    override def getParamTermS(param: Int)(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Option[TypeValueTerm[GlobalSymbol]]) =
      throw new UnsupportedOperationException
    
    override def findRootParamS(param: Int)(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], Int]) =
      throw new UnsupportedOperationException
    
    override def replaceParamS(param: Int, term: TypeValueTerm[GlobalSymbol])(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], Unit]) =
      throw new UnsupportedOperationException
    
    override def unionParamsS(param1: Int, param2: Int)(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], Boolean]) =
      throw new UnsupportedOperationException
    
    override def allocateParamS(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], Int]) =
      throw new UnsupportedOperationException
    
    override def replaceTermParamsS(term: TypeValueTerm[GlobalSymbol])(f: (Int, SymbolTypeInferenceEnvironment[T, U]) => (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], Either[Int, TypeValueTerm[GlobalSymbol]]]))(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], TypeValueTerm[GlobalSymbol]]) =
      throw new UnsupportedOperationException
    
    override def mismatchedTermErrorS(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], NoType[GlobalSymbol]) =
      throw new UnsupportedOperationException
    
    override def checkUnificationS(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], Unit]) =
      throw new UnsupportedOperationException
    
    override def withSaveS[V, W](f: SymbolTypeInferenceEnvironment[T, U] => (SymbolTypeInferenceEnvironment[T, U], Validation[V, W]))(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Validation[V, W]) =
      throw new UnsupportedOperationException
  }
  
  implicit def symbolSimpleTermTypeInferrer[T, U]: Inferrer[SimpleTerm[Symbol, lmbdindexer.LambdaInfo[T], TypeSimpleTerm[Symbol, lmbdindexer.TypeLambdaInfo[U]]], SymbolTypeInferenceEnvironment[T, U], Type[GlobalSymbol]] = new Inferrer[SimpleTerm[Symbol, lmbdindexer.LambdaInfo[T], TypeSimpleTerm[Symbol, lmbdindexer.TypeLambdaInfo[U]]], SymbolTypeInferenceEnvironment[T, U], Type[GlobalSymbol]] {
    override def inferSimpleTermInfoS(simpleTerm: SimpleTerm[Symbol, lmbdindexer.LambdaInfo[T], TypeSimpleTerm[Symbol, lmbdindexer.TypeLambdaInfo[U]]])(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Type[GlobalSymbol]) =
      throw new UnsupportedOperationException
    
    override def unifyInfosS(info1: Type[GlobalSymbol], info2: Type[GlobalSymbol])(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Type[GlobalSymbol]) =
      throw new UnsupportedOperationException

    override def unifyArgInfosS(funArgInfo: Type[GlobalSymbol], argInfo: Type[GlobalSymbol])(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Type[GlobalSymbol]) =
      throw new UnsupportedOperationException
    
    override def argInfosFromInfoS(info: Type[GlobalSymbol], argCount: Int)(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Validation[Type[GlobalSymbol], Seq[Type[GlobalSymbol]]]) =
      throw new UnsupportedOperationException
    
    override def returnInfoFromInfoS(info: Type[GlobalSymbol], argCount: Int)(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Type[GlobalSymbol]) =
      throw new UnsupportedOperationException
    
    override def isNoInfo(info: Type[GlobalSymbol]): Boolean =
      throw new UnsupportedOperationException
    
    override def functionInfo(argCount: Int): Type[GlobalSymbol] =
      throw new UnsupportedOperationException
    
    override def concatErrors(info1: Type[GlobalSymbol], info2: Type[GlobalSymbol]): Type[GlobalSymbol] =
      throw new UnsupportedOperationException
    
    override def unequalListLengthNoInfo: Type[GlobalSymbol] =
      throw new UnsupportedOperationException
    
    override def withPos(res: (SymbolTypeInferenceEnvironment[T, U], Type[GlobalSymbol]))(pos: Position): (SymbolTypeInferenceEnvironment[T, U], Type[GlobalSymbol]) =
      throw new UnsupportedOperationException
  }
  
  implicit def symbolCombinatorTypeInitializer[T, U]: Initializer[NoType[GlobalSymbol], GlobalSymbol, AbstractCombinator[Symbol, lmbdindexer.LambdaInfo[T], TypeSimpleTerm[Symbol, lmbdindexer.TypeLambdaInfo[U]]], SymbolTypeInferenceEnvironment[T, U]] = new Initializer[NoType[GlobalSymbol], GlobalSymbol, AbstractCombinator[Symbol, lmbdindexer.LambdaInfo[T], TypeSimpleTerm[Symbol, lmbdindexer.TypeLambdaInfo[U]]], SymbolTypeInferenceEnvironment[T, U]] {
    override def globalVarsFromEnvironmentS(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Set[GlobalSymbol]) =
      throw new UnsupportedOperationException
    
    override def usedGlobalVarsFromCombinator(comb: AbstractCombinator[Symbol, lmbdindexer.LambdaInfo[T], TypeSimpleTerm[Symbol, lmbdindexer.TypeLambdaInfo[U]]]): Set[GlobalSymbol] =
      throw new UnsupportedOperationException
    
    override def prepareGlobalVarS(loc: GlobalSymbol)(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Unit) =
      throw new UnsupportedOperationException
      
    override def initializeGlobalVarS(loc: GlobalSymbol, comb: AbstractCombinator[Symbol, lmbdindexer.LambdaInfo[T], TypeSimpleTerm[Symbol, lmbdindexer.TypeLambdaInfo[U]]])(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], Unit]) =
      throw new UnsupportedOperationException
    
    override def checkEnvironmentS(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], Unit]) =
      throw new UnsupportedOperationException
    
    override def undefinedGlobalVarError: NoType[GlobalSymbol] =
      throw new UnsupportedOperationException
    
    override def withSaveS[V, W](f: SymbolTypeInferenceEnvironment[T, U] => (SymbolTypeInferenceEnvironment[T, U], Validation[V, W]))(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Validation[V, W]) =
      throw new UnsupportedOperationException
  }
  
  //
  // A miscellaneous.
  //
  
  implicit val typeBuiltinFunctionEqual: Equal[TypeBuiltinFunction.Value] = new Equal[TypeBuiltinFunction.Value] {
    override def equal(a1: TypeBuiltinFunction.Value, a2: TypeBuiltinFunction.Value) = a1 == a2
  }
}