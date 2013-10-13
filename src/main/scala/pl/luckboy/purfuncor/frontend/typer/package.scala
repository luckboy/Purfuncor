package pl.luckboy.purfuncor.frontend
import scala.annotation.tailrec
import scala.collection.immutable.IntMap
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
import pl.luckboy.purfuncor.frontend.kinder.Kind
import pl.luckboy.purfuncor.frontend.kinder.NoKind
import pl.luckboy.purfuncor.frontend.kinder.SymbolKindInferenceEnvironment
import pl.luckboy.purfuncor.frontend.kinder.symbolTypeSimpleTermKindInferrer
import pl.luckboy.purfuncor.common.Evaluator._
import pl.luckboy.purfuncor.common.Inferrer._
import pl.luckboy.purfuncor.frontend.typer.TypeResult._
import pl.luckboy.purfuncor.frontend.typer.TypeValueTermKindInferrer._
import pl.luckboy.purfuncor.frontend.typer.TypeValueTermUnifier._
import pl.luckboy.purfuncor.frontend.typer.TypeValueTermUtils._
import pl.luckboy.purfuncor.frontend.resolver.TermUtils._

package object typer
{
  //
  // A type interpreter.
  //
  
  implicit def symbolTypeEnvironmentState[T] = new TypeEnvironmentState[SymbolTypeEnvironment[T], GlobalSymbol, TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]] {
    override def typeParamCountFromEnvironmentS(env: SymbolTypeEnvironment[T]) = (env, env.typeParamCount)
    
    override def withTypeParamsS[U](paramCount: Int)(f: (Int, Int, SymbolTypeEnvironment[T]) => (SymbolTypeEnvironment[T], U))(env: SymbolTypeEnvironment[T]) =
      env.withTypeParams(paramCount)(f)
      
    override def currentTypeParamAppIdxFromEnvironmentS(env: SymbolTypeEnvironment[T]) = (env, env.currentTypeParamAppIdx)
    
    override def globalTypeVarValueFromEnvironmentS(loc: GlobalSymbol)(env: SymbolTypeEnvironment[T]) = (env, env.typeVarValue(loc))
  }
  
  implicit def symbolTypeSimpleTermEvaluator[T]: Evaluator[TypeSimpleTerm[Symbol, T], SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]] = new Evaluator[TypeSimpleTerm[Symbol, T], SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]] {
    override def evaluateSimpleTermS(simpleTerm: TypeSimpleTerm[Symbol, T])(env: SymbolTypeEnvironment[T]) =
      simpleTerm match {
        case lambda: TypeLambda[Symbol, T] =>
          (env, TypeLambdaValue(lambda, env.currentTypeClosure, none, env.currentFile))
        case TypeVar(loc)                  =>
          loc match {
            case globalSym: GlobalSymbol if env.applyingTypeCombSyms.contains(globalSym) =>
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
            val (env2, retValue) = env.withTypeCombSym(loc) {
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
            val (env2, retValue) = env.withTypeCombSyms(combLoc.toSet) {
              _.withTypeClosure(closure) {
                _.withLocalTypeVars(localTypeVarValues) { newEnv => evaluateS(lambda.body)(newEnv) }
              }
            }
            (env2, retValue.forFile(file))
          } else
            (env, NoTypeValue.fromError(FatalError("illegal number of type arguments", none, NoPosition)))
        case TypePartialAppValue(funValue2, argValues2, combLoc) =>
          if(funValue2.argCount - argValues2.size === argValues.size)
            env.withTypeCombSyms(combLoc.toSet)(fullyAppS(funValue2, argValues2 ++ argValues))
          else
            (env, NoTypeValue.fromError(FatalError("illegal number of type arguments", none, NoPosition)))
        case tupleTypeFunValue @ TupleTypeFunValue(_) =>
          tupleTypeFunValue.fullyApplyS(argValues)(env)
        case TypeBuiltinFunValue(_, f) =>
          if(f.argCount === argValues.size)
            f.applyS(argValues)(env)
          else
            (env, NoTypeValue.fromError(FatalError("illegal number of type arguments", none, NoPosition)))
        case EvaluatedTypeValue(typeApp: TypeApp[GlobalSymbol]) =>
          if(argValues.size === 1) {
            val (env2, res) = TypeValueLambda.typeValueLambdasFromTypeValuesS(argValues)(env)
            (env2, res.map { ls => EvaluatedTypeValue(typeApp.withArgs(typeApp.args ++ ls)) }.valueOr(identity))
          } else
            (env, NoTypeValue.fromError(FatalError("illegal number of type arguments", none, NoPosition)))
        case EvaluatedTypeLambdaValue(lambda) =>
          if(lambda.argParams.size === argValues.size) {
            val (env2, res) = TypeValueLambda.typeValueLambdasFromTypeValuesS(argValues)(env)
            val retValue = res.map {
              ls =>
                substituteTypeValueLambdas(lambda.body, lambda.argParams.zip(ls).toMap).map {
                  EvaluatedTypeValue(_)
                }.getOrElse(NoTypeValue.fromError[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]](FatalError("can't substitute type value lambdas", none, NoPosition)))
            }.valueOr(identity)
            (env2, retValue)
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
  
  implicit def symbolKindInferrenceEnvironmentState[T]: KindInferrenceEnvironmentState[SymbolKindInferenceEnvironment[T], GlobalSymbol] = new KindInferrenceEnvironmentState[SymbolKindInferenceEnvironment[T], GlobalSymbol] {
    override def globalTypeVarKindFromEnvironmentS(loc: GlobalSymbol)(env: SymbolKindInferenceEnvironment[T]) =
      (env, env.typeVarKind(loc))
    
    override def typeParamKindFromEnvironmentS(param: Int)(env: SymbolKindInferenceEnvironment[T]) =
      (env, env.typeParamKind(param))
    
    override def unifyStarKindWithKindS(kind: Kind)(env: SymbolKindInferenceEnvironment[T]): (SymbolKindInferenceEnvironment[T], Kind) =
      throw new UnsupportedOperationException
  }
  
  implicit def symbolTypeInferenceEnvironmentState[T, U]: TypeInferenceEnvironmentState[SymbolTypeInferenceEnvironment[T, U], GlobalSymbol] = new TypeInferenceEnvironmentState[SymbolTypeInferenceEnvironment[T, U], GlobalSymbol] {
    override def appForGlobalTypeS(funLoc: GlobalSymbol, argLambdas: Seq[TypeValueLambda[GlobalSymbol]])(env: SymbolTypeInferenceEnvironment[T, U]) = {
      val (typeEnv, res) = env.typeEnv.withTypeParamAppIdx(env.nextTypeParamAppIdx) { TypeValueTerm.appForGlobalTypeS(funLoc, argLambdas)(_) }
      (env.withTypeEnv(typeEnv), typeResultFromTypeValueResult(res))
    }
    
    override def inferTypeValueTermKindS(term: TypeValueTerm[GlobalSymbol])(env: SymbolTypeInferenceEnvironment[T, U]) =
      TypeValueTermKindInferrer.inferTypeValueTermKindS(term)(env.kindInferenceEnv).mapElements(env.withKindInferenceEnv, typeResultFromKind)
    
    override def appKindS(funKind: Kind, argKinds: Seq[Kind])(env: SymbolTypeInferenceEnvironment[T, U]) =
      appInfoS(funKind, argKinds)(env.kindInferenceEnv).mapElements(env.withKindInferenceEnv, typeResultFromKind)
    
    override def appStarKindS(argKinds: Seq[Kind])(env: SymbolTypeInferenceEnvironment[T, U]) =
      TypeValueTermKindInferrer.appStarKindS(argKinds)(env.kindInferenceEnv).mapElements(env.withKindInferenceEnv, typeResultFromKind)
    
    override def lambdaKindS(argKinds: Seq[Kind], retKind: Kind)(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], Kind]) =
      TypeValueTermKindInferrer.lambdaKindS(argKinds, retKind)(env.kindInferenceEnv).mapElements(env.withKindInferenceEnv, typeResultFromKind)
    
    override def unifyKindsS(kind1: Kind, kind2: Kind)(env: SymbolTypeInferenceEnvironment[T, U]) =
      symbolTypeSimpleTermKindInferrer.unifyInfosS(kind1, kind2)(env.kindInferenceEnv).mapElements(env.withKindInferenceEnv, typeResultFromKind)
    
    override def returnKindFromEnvironmentS(env: SymbolTypeInferenceEnvironment[T, U]) = (env, env.typeRetKind)
    
    override def setReturnKindS(kind: Kind)(env: SymbolTypeInferenceEnvironment[T, U]) = (env.withTypeRetKind(kind), ())
    
    override def withRecursionCheckingS[V](locs: Set[GlobalSymbol])(f: SymbolTypeInferenceEnvironment[T, U] => (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], V]))(env: SymbolTypeInferenceEnvironment[T, U]) =
      if((env.matchingGlobalTypeSyms & locs).isEmpty)
        env.withGlobalTypes(locs)(f)
      else
        symbolTypeValueTermUnifier.mismatchedTermErrorS(env).mapElements(identity, _.failure)
    
    override def addDelayedErrorsS(errs: Map[Int, NoType[GlobalSymbol]])(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env.withDelayedErrs(errs), ())
    
    override def delayedErrorsFromEnvironmentS(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env, env.delayedErrNoTypes)
    
    override def withDelayedErrorRestoringOrSavingS[V](errs: Map[Int, NoType[GlobalSymbol]])(f: SymbolTypeInferenceEnvironment[T, U] => (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], V]))(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], (Validation[NoType[GlobalSymbol], V], Boolean)) = {
      val savedErrCount = env.delayedErrNoTypes.size
      val (env2, res) = f(env)
      res match {
        case Success(_) =>
          if(savedErrCount === env.delayedErrNoTypes.size) (env2, (res, true)) else (env, (res, false))
        case Failure(_) =>
          (env, (res, false))
      }
    }
    
    override def allocateTypeParamAppIdxS(env: SymbolTypeInferenceEnvironment[T, U]) =
      env.allocateTypeParamAppIdx.map { _.mapElements(identity, _.success) }.valueOr { nt => (env, nt.failure) }
    
    override def nextTypeParamAppIdxFromEnvironmentS(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env, env.nextTypeParamAppIdx)
    
    override def allocatedTypeParamsFromEnvironmentS(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env, env.typeParamForest.allocatedParams)
    
    override def withTypeLambdaArgsS[V](argParams: Seq[Set[Int]])(f: SymbolTypeInferenceEnvironment[T, U] => (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], V]))(env: SymbolTypeInferenceEnvironment[T, U]) = 
      env.withTypeLambdaArgs(argParams)(f)
    
    override def currentTypeMatchingFromEnvironmentS(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env, env.currentTypeMatching)
    
    override def setCurrentTypeMatchingS(typeMatching: TypeMatching.Value)(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env.withCurrentTypeMatching(typeMatching), ())
  }
  
  implicit def symbolTypeValueTermUnifier[T, U]: Unifier[NoType[GlobalSymbol], TypeValueTerm[GlobalSymbol], SymbolTypeInferenceEnvironment[T, U], Int] = new Unifier[NoType[GlobalSymbol], TypeValueTerm[GlobalSymbol], SymbolTypeInferenceEnvironment[T, U], Int] {
    override def matchesTermsS[V](term1: TypeValueTerm[GlobalSymbol], term2: TypeValueTerm[GlobalSymbol])(z: V)(f: (Int, Either[Int, TypeValueTerm[GlobalSymbol]], V, SymbolTypeInferenceEnvironment[T, U]) => (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], V]))(env: SymbolTypeInferenceEnvironment[T, U]) =
      matchesTypeValueTermsS(term1, term2)(z)(f)(env)
    
    override def getParamTermS(param: Int)(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env, env.typeParamForest.getTerm(param))
    
    override def findRootParamS(param: Int)(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env, env.typeParamForest.findRootParam(param).toSuccess(NoType.fromError(FatalError("not found type parameter", none, NoPosition))))
    
    override def replaceParamS(param: Int, term: TypeValueTerm[GlobalSymbol])(env: SymbolTypeInferenceEnvironment[T, U]) = {
      if(!env.typeParamForest.containsTerm(param))
        env.typeParamForest.findRootParam(param).map {
          rootParam =>
            env.irreplaceableTypeParams.get(rootParam).map {
              dts => (env, NoType.fromErrors[GlobalSymbol](dts.map { dt =>  FatalError("couldn't instantiate parameter at defined type " + dt, none, NoPosition) }).failure)
            }.getOrElse {
              val paramKind = env.kindInferenceEnv.typeParamKind(param)
              val (kindInferenceEnv, termKind) = inferTypeValueTermKindS(term)(env.kindInferenceEnv)
              val (kindInferenceEnv2, retKind) = symbolTypeSimpleTermKindInferrer.unifyInfosS(paramKind, termKind)(kindInferenceEnv)
              val env2 = env.withKindInferenceEnv(kindInferenceEnv2)
              retKind match {
                case noKind: NoKind =>
                  (env2, NoType.fromNoKind[GlobalSymbol](noKind).failure)
                case _              =>
                  env2.typeParamForest.replaceParam(rootParam, term).map {
                    tpf => (env2.withTypeParamForest(tpf).withTypeRetKind(retKind), ().success)
                  }.getOrElse((env2, NoType.fromError[GlobalSymbol](FatalError("not found type parameter", none, NoPosition)).failure))
              }
            }
        }.getOrElse((env, NoType.fromError[GlobalSymbol](FatalError("not found type parameter", none, NoPosition)).failure))
      else
        (env, NoType.fromError[GlobalSymbol](FatalError("type parameter is already replaced", none, NoPosition)).failure)
    }        
    
    override def unionParamsS(param1: Int, param2: Int)(env: SymbolTypeInferenceEnvironment[T, U]) =
      if(!env.typeParamForest.containsTerm(param1) && !env.typeParamForest.containsTerm(param2)) {
        val paramKind1 = env.kindInferenceEnv.typeParamKind(param1)
        val paramKind2 = env.kindInferenceEnv.typeParamKind(param2)
        val (kindInferenceEnv, retKind) = symbolTypeSimpleTermKindInferrer.unifyInfosS(paramKind1, paramKind2)(env.kindInferenceEnv)
        val env2 = env.withKindInferenceEnv(kindInferenceEnv)
        retKind match {
          case noKind: NoKind =>
            (env2, NoType.fromNoKind[GlobalSymbol](noKind).failure)
          case _              =>
            env2.typeParamForest.unionParams(param1, param2).map {
              case (tpf, isChanged) =>
                tpf.findRootParam(param1).map {
                  rp =>
                    val newIrreplaceableTypeParams = if(param1 =/= param2) {
                      val definedTypes = env2.irreplaceableTypeParams.get(param1).map { _.list }.getOrElse(Nil) ++ env2.irreplaceableTypeParams.get(param2).map { _.list }.getOrElse(Nil)
                      IntMap() ++ (env2.irreplaceableTypeParams ++ definedTypes.toNel.map { rp -> _})
                    } else
                      env2.irreplaceableTypeParams
                    (env2.withTypeParamForest(tpf).copy(irreplaceableTypeParams = newIrreplaceableTypeParams).withTypeRetKind(retKind), isChanged.success)
                }.getOrElse((env2, NoType.fromError[GlobalSymbol](FatalError("not found type parameter", none, NoPosition)).failure))
            }.getOrElse((env2, NoType.fromError[GlobalSymbol](FatalError("not found one type parameter or two type parameters", none, NoPosition)).failure))
        }
      } else
        (env, NoType.fromError[GlobalSymbol](FatalError("one type parameter or two type parameters are already replaced ", none, NoPosition)).failure)
    
    override def allocateParamS(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], Int]) =
      throw new UnsupportedOperationException
    
    override def replaceTermParamsS(term: TypeValueTerm[GlobalSymbol])(f: (Int, SymbolTypeInferenceEnvironment[T, U]) => (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], Either[Int, TypeValueTerm[GlobalSymbol]]]))(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], TypeValueTerm[GlobalSymbol]]) =
      throw new UnsupportedOperationException
    
    override def mismatchedTermErrorS(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], NoType[GlobalSymbol]) =
      throw new UnsupportedOperationException
    
    override def checkUnificationS(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], Unit]) =
      throw new UnsupportedOperationException
    
    override def prepareToMatchingS(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Unit) =
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
  
  implicit val typeMatchingEqual: Equal[TypeMatching.Value] = new Equal[TypeMatching.Value] {
    override def equal(a1: TypeMatching.Value, a2: TypeMatching.Value) = a1 == a2
  }
}