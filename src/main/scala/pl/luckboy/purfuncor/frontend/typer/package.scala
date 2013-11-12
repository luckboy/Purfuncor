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
import pl.luckboy.purfuncor.frontend.kinder.InferredKind
import pl.luckboy.purfuncor.frontend.kinder.InferringKind
import pl.luckboy.purfuncor.frontend.kinder.KindInferrer
import pl.luckboy.purfuncor.frontend.kinder.SymbolKindInferenceEnvironment
import pl.luckboy.purfuncor.frontend.kinder.TypeLambdaInfo
import pl.luckboy.purfuncor.frontend.kinder.symbolTypeSimpleTermKindInferrer
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.common.Evaluator._
import pl.luckboy.purfuncor.common.Inferrer._
import pl.luckboy.purfuncor.common.RecursiveInitializer._
import pl.luckboy.purfuncor.frontend.kinder.KindTermUnifier._
import pl.luckboy.purfuncor.frontend.typer.TypeResult._
import pl.luckboy.purfuncor.frontend.typer.TypeInferrer._
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
            case globalSym: GlobalSymbol if env.applyingTypeCombSyms.contains(globalSym) || env.isPartial =>
              (env, EvaluatedTypeValue(GlobalTypeApp(globalSym, Nil, globalSym)))
            case _                                                                                        =>
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
                substituteTypeValueLambdas(lambda.body, lambda.argParams.zip(ls).toMap, env2.typeParamCount).map {
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
            val (newEnv, value) = env.withFile(file) { _.withPartialEvaluation(false) { evaluateS(body)(_) } }
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
    
    override def withPartialEvaluation[U](env: SymbolTypeEnvironment[T])(isPartial: Boolean)(f: SymbolTypeEnvironment[T] => (SymbolTypeEnvironment[T], U)) =
      env.withPartialEvaluation(isPartial)(f)
  }
  
  implicit def symbolTypeEnvironmentGlobalSymbolTabular[T] = new GlobalSymbolTabular[SymbolTypeEnvironment[T], GlobalSymbol] {
    override def getGlobalLocationFromTable(table: SymbolTypeEnvironment[T])(sym: GlobalSymbol) = some(sym)

    override def getGlobalSymbolFromTable(table: SymbolTypeEnvironment[T])(sym: GlobalSymbol) = some(sym)
  }
  
  //
  // A type inferrer.
  //
  
  implicit def noTypeSemigroup[T]: Semigroup[NoType[T]] = new Semigroup[NoType[T]] {
    override def append(f1: NoType[T], f2: => NoType[T]) =
      (f1, f2) match {
        case (NoType(prevErrs1, currentErrs1), NoType(prevErrs2, currentErrs2)) =>
          NoType(prevErrs = prevErrs1 ++ prevErrs2, currentErrs = currentErrs1 ++ currentErrs2)
      }
  }
  
  implicit def symbolTypeLambdaInfoArgTabular[T]: ArgTabular[TypeLambda[Symbol, TypeLambdaInfo[T, LocalSymbol]], LocalSymbol] = new ArgTabular[TypeLambda[Symbol, TypeLambdaInfo[T, LocalSymbol]], LocalSymbol] {
    override def getArgLocationsFromTable(table: TypeLambda[Symbol, TypeLambdaInfo[T, LocalSymbol]]): Seq[Option[LocalSymbol]] =
      table.args.list.map { _.name.map { LocalSymbol(_) } }
  }
  
  implicit val symbolBuiltinFunTypes = new BuiltinFunTypes[GlobalSymbol]
  
  implicit def symbolKindInferrenceEnvironmentState[T]: KindInferrenceEnvironmentState[SymbolKindInferenceEnvironment[T], GlobalSymbol] = new KindInferrenceEnvironmentState[SymbolKindInferenceEnvironment[T], GlobalSymbol] {
    override def globalTypeVarKindFromEnvironmentS(loc: GlobalSymbol)(env: SymbolKindInferenceEnvironment[T]) =
      (env, env.typeVarKind(loc))
    
    override def typeParamKindFromEnvironmentS(param: Int)(env: SymbolKindInferenceEnvironment[T]) =
      (env, env.typeParamKind(param))
    
    override def addTypeParamKindS(param: Int, kind: Kind)(env: SymbolKindInferenceEnvironment[T]) =
      (env.withTypeParamKind(param, kind), ())
    
    override def unifyStarKindWithKindS(kind: Kind)(env: SymbolKindInferenceEnvironment[T]): (SymbolKindInferenceEnvironment[T], Kind) = {
      val (env2, res) = kind.instantiatedKindTermS(env)
      res.map { kt => env2.withKindTermPair(some((Star(KindType, NoPosition), kt)))(TypeValueTermKindInferrer.unifyStarKindWithKindS(kind)) }.valueOr { (env2, _) }
    }
  }
  
  implicit def symbolTypeInferenceEnvironmentState[T, U]: TypeInferenceEnvironmentState[SymbolTypeInferenceEnvironment[T, U], GlobalSymbol, GlobalSymbol] = new TypeInferenceEnvironmentState[SymbolTypeInferenceEnvironment[T, U], GlobalSymbol, GlobalSymbol] {
    override def appForGlobalTypeS(funLoc: GlobalSymbol, argLambdas: Seq[TypeValueLambda[GlobalSymbol]], paramCount: Int, paramAppIdx: Int)(env: SymbolTypeInferenceEnvironment[T, U]) = {
      val (typeEnv, res) = env.typeEnv.withTypeParamAppIdx(paramAppIdx) { 
        _.withTypeParams(paramCount) { 
          (_, _, typeEnv2) => 
            typeEnv2.withPartialEvaluation(false) { TypeValueTerm.appForGlobalTypeS(funLoc, argLambdas)(_) }
        }
      }
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
      val (env3, res2) = checkDefinedTypesS(env2.definedTypes)(env2)
      res.orElse(res2) match {
        case Success(_)      =>
          if(savedErrCount === env.delayedErrNoTypes.size) (env3, (res, true)) else (env, (res, false))
        case Failure(noType) =>
          (env, (noType.failure, false))
      }
    }
    
    override def allocateTypeParamAppIdxS(env: SymbolTypeInferenceEnvironment[T, U]) =
      env.allocateTypeParamAppIdx.map { _.mapElements(identity, _.success) }.valueOr { nt => (env, nt.failure) }
    
    override def nextTypeParamAppIdxFromEnvironmentS(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env, env.nextTypeParamAppIdx)
    
    override def allocatedTypeParamsFromEnvironmentS(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env, env.typeParamForest.allocatedParams)
    
    override def nextTypeParamFromEnvironmentS(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env, env.typeParamForest.next)
        
    override def withTypeLambdaArgsS[V](argParams: Seq[Set[Int]])(f: SymbolTypeInferenceEnvironment[T, U] => (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], V]))(env: SymbolTypeInferenceEnvironment[T, U]) = 
      env.withTypeLambdaArgs(argParams)(f)
    
    override def currentTypeMatchingFromEnvironmentS(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env, env.currentTypeMatching)
    
    override def setCurrentTypeMatchingS(typeMatching: TypeMatching.Value)(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env.withCurrentTypeMatching(typeMatching), ())
      
    override def inferringKindFromKindS(kind: Kind)(env: SymbolTypeInferenceEnvironment[T, U]) = {
      val (kindInferenceEnv, uninstantiatedKind) = kind.uninstantiatedKindS(env.kindInferenceEnv)
      (env.withKindInferenceEnv(kindInferenceEnv), uninstantiatedKind match {
        case noKind: NoKind               => NoType.fromNoKind[GlobalSymbol](noKind).failure
        case inferringKind: InferringKind => inferringKind.success
        case _                            => NoType.fromError[GlobalSymbol](FatalError("no inferring kind", none, NoPosition)).failure
      })
    }
  
    override def setTypeParamKindsS(kinds: Map[Int, Kind])(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env.withKindInferenceEnv(env.kindInferenceEnv.withTypeParamKinds(kinds)), ())
      
    override def argCountFromKindS(kind: Kind)(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], Int]) =
      KindInferrer.argCountFromKindS(kind)(env.kindInferenceEnv).mapElements(env.withKindInferenceEnv, typeResultFromKindResult)
  
    override def inferredKindFromKindS(kind: Kind)(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], InferredKind]) = {
      val (kindInferenceEnv, instantiatedKind) = kind.instantiatedKindS(env.kindInferenceEnv)
      (env.withKindInferenceEnv(kindInferenceEnv), instantiatedKind match {
        case noKind: NoKind             => NoType.fromNoKind[GlobalSymbol](noKind).failure
        case inferredKind: InferredKind => inferredKind.success
        case _                          => NoType.fromError[GlobalSymbol](FatalError("no inferred kind", none, NoPosition)).failure
      })
    }
    
    override def withClearS[V](f: SymbolTypeInferenceEnvironment[T, U] => (SymbolTypeInferenceEnvironment[T, U], V))(env: SymbolTypeInferenceEnvironment[T, U]) =
      env.withClear(f)
      
    override def withCombinatorLocationS[V](loc: Option[GlobalSymbol])(f: SymbolTypeInferenceEnvironment[T, U] => (SymbolTypeInferenceEnvironment[T, U], V))(env: SymbolTypeInferenceEnvironment[T, U]) = {
      val oldLoc = env.currentCombSym
      val (env2, res) = f(env.withCurrentCombSym(loc))
      (env2.withCurrentCombSym(loc), res)
    }
      
    override def instantiateTypesFromLambdaInfosS(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], Unit]) = {
      val (env2, res) = env.lambdaInfos.getOrElse(env.currentCombSym, Map()).foldLeft((env, Map[Int, InferenceLambdaInfo[LocalSymbol, GlobalSymbol]]().success[NoType[GlobalSymbol]])) {
        case ((newEnv, Success(lis)), (i, li)) =>
          val (newEnv2, newRes) = instantiateTypeMapS(li.typeTable.types)(newEnv)
          newRes.map {
            ts => instantiateTypesS(li.instanceTypes)(newEnv).mapElements(identity, _.map { its => lis + (i -> li.copy(typeTable = TypeTable(ts), instanceTypes = its)) })
          }.valueOr { nt => (newEnv, nt.failure) }
        case ((newEnv, Failure(nt)), _)        =>
          (newEnv, nt.failure)
      }
      res.map {
        lis => (env2.copy(lambdaInfos = env2.lambdaInfos + (env.currentCombSym -> lis)), ().success)
      }.valueOr { nt => (env, nt.failure) }
    }
    
    override def instantiateTypeS(typ: Type[GlobalSymbol])(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Type[GlobalSymbol]) =
      typ.instantiatedTypeS(env)
  }
  
  implicit def symbolTypeValueTermUnifier[T, U]: Unifier[NoType[GlobalSymbol], TypeValueTerm[GlobalSymbol], SymbolTypeInferenceEnvironment[T, U], Int] = new Unifier[NoType[GlobalSymbol], TypeValueTerm[GlobalSymbol], SymbolTypeInferenceEnvironment[T, U], Int] {
    override def matchesTermsS[V](term1: TypeValueTerm[GlobalSymbol], term2: TypeValueTerm[GlobalSymbol])(z: V)(f: (Int, Either[Int, TypeValueTerm[GlobalSymbol]], V, SymbolTypeInferenceEnvironment[T, U]) => (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], V]))(env: SymbolTypeInferenceEnvironment[T, U]) =
      matchesTypeValueTermsS(term1, term2)(z)(f)(env)
    
    override def getParamTermS(param: Int)(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env, env.typeParamForest.getTerm(param))
    
    override def findRootParamS(param: Int)(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env, env.typeParamForest.findRootParam(param).toSuccess(NoType.fromError(FatalError("not found type parameter", none, NoPosition))))
    
    override def replaceParamS(param: Int, term: TypeValueTerm[GlobalSymbol])(env: SymbolTypeInferenceEnvironment[T, U]) =
      if(!env.typeParamForest.containsTerm(param))
        if(!env.typeLambdaArgParams.contains(param))
          env.typeParamForest.findRootParam(param).map {
            rp =>
              env.irreplaceableTypeParams.get(rp).map {
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
                    env2.typeParamForest.replaceParam(rp, term).map {
                      tpf => (env2.withTypeParamForest(tpf).withTypeRetKind(retKind), ().success)
                    }.getOrElse((env2, NoType.fromError[GlobalSymbol](FatalError("not found type parameter", none, NoPosition)).failure))
                }
              }
          }.getOrElse((env, NoType.fromError[GlobalSymbol](FatalError("not found type parameter", none, NoPosition)).failure))
        else
          mismatchedTermErrorS(env).mapElements(identity, _.failure)
      else
        (env, NoType.fromError[GlobalSymbol](FatalError("type parameter is already replaced", none, NoPosition)).failure)
    
    override def unionParamsS(param1: Int, param2: Int)(env: SymbolTypeInferenceEnvironment[T, U]) =
      if(!env.typeParamForest.containsTerm(param1) && !env.typeParamForest.containsTerm(param2)) 
        if((env.typeLambdaArgParams.get(param1) |@| env.typeLambdaArgParams.get(param2)) { _ === _ }.getOrElse(true)){
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
          mismatchedTermErrorS(env).mapElements(identity, _.failure)
      else
        (env, NoType.fromError[GlobalSymbol](FatalError("one type parameter or two type parameters are already replaced ", none, NoPosition)).failure)
    
    override def allocateParamS(env: SymbolTypeInferenceEnvironment[T, U]) =
      env.typeParamForest.allocateParam.map {
        case (tpf, p) =>
          val (kindInferenceEnv, res) = allocateKindTermParamsS(Star(KindParam(0), NoPosition))(Map())(env.kindInferenceEnv)
          val env2 = env.withKindInferenceEnv(kindInferenceEnv)
          res.map {
            case (_, kt) =>
              val kindInferenceEnv2 = env2.kindInferenceEnv.withTypeParamKind(p, InferringKind(kt))
              (env2.withTypeParamForest(tpf).withKindInferenceEnv(kindInferenceEnv2), p.success)
          }.valueOr { nk => (env2, NoType.fromNoKind[GlobalSymbol](nk).failure) }
      }.getOrElse((env, NoType.fromError[GlobalSymbol](FatalError("can't allocate type parameter", none, NoPosition)).failure))
    
    override def replaceTermParamsS(term: TypeValueTerm[GlobalSymbol])(f: (Int, SymbolTypeInferenceEnvironment[T, U]) => (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], Either[Int, TypeValueTerm[GlobalSymbol]]]))(env: SymbolTypeInferenceEnvironment[T, U]) =
      replaceTypeValueTermParamsS(term)(f)(env)
    
    override def mismatchedTermErrorS(env: SymbolTypeInferenceEnvironment[T, U]) = {
      val (s1, s2) = env.currentTypePair.map {
        case (t1, t2) => (t1.toString, t2.toString)
      }.getOrElse(("<unknown type>", "<unknown type"))
      (env, NoType.fromError[GlobalSymbol](Error("couldn't match type " + s1 + " with type " + s2, none, NoPosition)))
    }
    
    override def prepareToUnificationS(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env.withDelayedErrNoTypes(Map()).withPrevDelayedErrTypeParamAppIdxs(Set()), ())
    
    override def checkMatchingS(env: SymbolTypeInferenceEnvironment[T, U]) =
      if(env.delayedErrNoTypes.keySet === env.prevDelayedErrTypeParamAppIdxs)
        env.delayedErrNoTypes.headOption.map {
          case (_, nt) => (env.withDelayedErrNoTypes(Map()).withPrevDelayedErrTypeParamAppIdxs(Set()), nt.failure)
        }.getOrElse((env.withDelayedErrNoTypes(Map()).withPrevDelayedErrTypeParamAppIdxs(Set()), false.success))
      else
        (env.withPrevDelayedErrTypeParamAppIdxs(env.delayedErrNoTypes.keySet), true.success)
    
    override def withSaveS[V, W](f: SymbolTypeInferenceEnvironment[T, U] => (SymbolTypeInferenceEnvironment[T, U], Validation[V, W]))(env: SymbolTypeInferenceEnvironment[T, U]) = {
      val (env2, res) = f(env)
      res.map { x => (env2, x.success) }.valueOr { e => (env, e.failure ) }        
    }
  }
  
  implicit def symbolSimpleTermTypeInferrer[T, U]: Inferrer[SimpleTerm[Symbol, lmbdindexer.LambdaInfo[T], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]], SymbolTypeInferenceEnvironment[T, U], Type[GlobalSymbol]] = new Inferrer[SimpleTerm[Symbol, lmbdindexer.LambdaInfo[T], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]], SymbolTypeInferenceEnvironment[T, U], Type[GlobalSymbol]] {
    private def inferCaseTypeS(cas: Case[Symbol, lmbdindexer.LambdaInfo[T], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]])(env: SymbolTypeInferenceEnvironment[T, U]) =
      cas match {
        case Case(name, typ, body, lmbdindexer.LambdaInfo(_, lambdaIdx)) =>
          env.withLambdaIdx(lambdaIdx) {
            _.withLocalVarTypes(name.map { s => Map(LocalSymbol(s) -> some(typ)) }.getOrElse(Map())) {
              newEnv =>
                val (newEnv2, bodyInfo) = inferS(body)(newEnv)
                val (newEnv3, argInfo) = name.map { s => (newEnv2, newEnv.varType(LocalSymbol(s))) }.getOrElse {
                  newEnv.definedTypeFromTypeTerm(typ).mapElements(identity, _.map { dt => InferringType(dt.term) }.valueOr(identity))
                }
                if(!bodyInfo.isNoType && !argInfo.isNoType)
                  (newEnv3.withCurrentInstanceTypes(Seq(argInfo)), bodyInfo)
                else
                  (newEnv3, concatErrors(argInfo, bodyInfo))
            }
          }
      }
      
    private def inferConstructTypeS(construct: Construct[Symbol, lmbdindexer.LambdaInfo[T], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]])(env: SymbolTypeInferenceEnvironment[T, U]) =
      construct match {
        case Construct(n, lmbdindexer.LambdaInfo(_, lambdaIdx)) =>
          val (env2, funType) = functionInfo(n).uninstantiatedTypeS(env)
          val (env3, argTypeRes) = argInfosFromInfoS(funType, n)(env2)
          val (env4, retType) = returnInfoFromInfoS(funType, n)(env3)
          env4.withLambdaIdx(lambdaIdx) {
            newEnv =>
              argTypeRes match {
                case Success(argTypes) =>
                  Type.uninstantiatedTypeValueTermFromTypesS(argTypes)(newEnv) match {
                    case (newEnv2, Success(argTypeValueTerms)) =>
                      val (newEnv3, newRes) = allocateTypeValueTermParamsWithKindsS(TypeParamApp(0, Nil, 0), Map(0 -> InferredKind(Star(KindType, NoPosition))))(Map(), 0)(newEnv2)
                      newRes.map {
                        case (_, _, _, tmpTypeValueTerm) =>
                          val tmpType = InferringType(tmpTypeValueTerm & TupleType(argTypeValueTerms))
                          val (newEnv4, retType2) = unifyInfosS(tmpType, retType)(newEnv3)
                          if(!retType.isNoType)
                            (newEnv4.withCurrentInstanceTypes(Seq(retType2)), funType)
                          else
                            (newEnv4, funType)
                      }.valueOr { (newEnv3, _) }
                    case (newEnv2, Failure(noType))            =>
                      (newEnv2, noType)
                  }
                case Failure(noType)   =>
                  (newEnv, noType)
              }
          }
      }
      
    private def inferSelectTypeS(select: Select[Symbol, lmbdindexer.LambdaInfo[T], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]])(env: SymbolTypeInferenceEnvironment[T, U]) =
      select match {
        case Select(term, cases, lmbdindexer.LambdaInfo(_, lambdaIdx)) =>
          val (env2, termType) = inferS(term)(env)
          val (env3, firstCaseType) = inferCaseTypeS(cases.head)(env2)
          val res = firstCaseType match {
            case noType: NoType[GlobalSymbol] => noType.failure
            case caseType                     => NonEmptyList(caseType).success
          }
          val (env4, res2) = cases.tail.foldLeft((env3, res)) {
            case ((newEnv, newRes), cas) =>
              inferCaseTypeS(cas)(newEnv) match {
                case (newEnv2, noType: NoType[GlobalSymbol]) =>
                  (newEnv2, newRes.flatMap { _ => noType.failure }.valueOr { nt => (nt |+| noType).failure })
                case (newEnv2, caseInfo)                     =>
                  (newEnv2, newRes.map { caseInfo <:: _})
              }
          }.mapElements(identity, _.map { _.reverse })
          res2 match {
            case Success(caseTypes) =>
              env4.withLambdaIdx(lambdaIdx) {
                newEnv =>
                  val (newEnv3, bodyType) = caseTypes.tail.foldLeft((newEnv, caseTypes.head)) { 
                    case ((newEnv2, newType), caseType) => unifyInfosS(newType, caseType)(newEnv2)
                  }
                  val argTypes = cases.list.flatMap { 
                    cas => 
                      newEnv3.lambdaInfos.getOrElse(newEnv3.currentCombSym, Map()).get(cas.lambdaInfo.idx).toSeq.flatMap {
                        _.instanceTypes
                      } 
                  }
                  Type.uninstantiatedTypeValueTermFromTypesS(argTypes)(newEnv3) match {
                    case (newEnv4, Success(argTypeValueTerms)) =>
                      val (newEnv6, newRes2) = argTypeValueTerms.headOption.map {
                        tvt =>
                          val tvt2 = argTypeValueTerms.tail.foldLeft(tvt) { _ | _ }
                          val (newEnv5, newRes) = symbolTypeInferenceEnvironmentState.inferTypeValueTermKindS(tvt2)(newEnv4)
                          (newEnv5, newRes.map { _ => InferringType(argTypeValueTerms.tail.foldLeft(tvt) { _ | _ }) })
                      }.getOrElse((newEnv4, NoType.fromError[GlobalSymbol](FatalError("no type value term", none, NoPosition)).failure))
                      newRes2 match {
                        case Success(tmpType) =>
                          val (newEnv7, termType2) = unifyInfosS(tmpType, termType)(newEnv6)
                          if(!termType2.isNoType && !bodyType.isNoType)
                            (newEnv7.withCurrentInstanceTypes(Seq(termType)), bodyType)
                          else
                            (newEnv7, concatErrors(termType, bodyType))
                        case Failure(noType) =>
                          (newEnv4, concatErrors(termType, concatErrors(bodyType, noType)))
                      }
                    case (newEnv4, Failure(noType))            =>
                      (newEnv4, noType)
                  }
              }
            case Failure(noType)    =>
              (env4, concatErrors(termType, noType))
          }
      }
    
    private def inferExtractTypeS(extract: Extract[Symbol, lmbdindexer.LambdaInfo[T], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]])(env: SymbolTypeInferenceEnvironment[T, U]) =
      extract match {
        case Extract(term, args, body, lmbdindexer.LambdaInfo(_, lambdaIdx)) =>
          val (env2, termType) = inferS(term)(env)
          env2.withLambdaIdx(lambdaIdx) {
            _.withLocalVarTypes(args.list.flatMap { a => a.name.map { s => (LocalSymbol(s), a.typ) } }.toMap) {
              newEnv =>
                val (newEnv2, bodyType) = inferS(body)(newEnv)
                val (newEnv3, argTypes) = newEnv2.typesFromArgs(args.list)
                Type.uninstantiatedTypeValueTermFromTypesS(argTypes)(newEnv3) match {
                  case (newEnv4, Success(argTypeValueTerms)) =>
                    val tmpType = InferringType(TupleType(argTypeValueTerms))
                    val (newEnv5, termType2) = unifyArgInfosS(tmpType, termType)(newEnv4)
                    if(!termType2.isNoType && !bodyType.isNoType)
                      (newEnv5.withCurrentInstanceTypes(Seq(termType)), bodyType)
                    else 
                      (newEnv5, concatErrors(termType, bodyType))
                  case (newEnv4, Failure(noType))            =>
                    (newEnv4, noType)
                }
            }
          }
      }
    
    override def inferSimpleTermInfoS(simpleTerm: SimpleTerm[Symbol, lmbdindexer.LambdaInfo[T], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]])(env: SymbolTypeInferenceEnvironment[T, U]) =
      simpleTerm match {
        case Let(binds, body, lmbdindexer.LambdaInfo(_, lambdaIdx)) =>
          val (env2, res) = binds.foldLeft((env, Map[LocalSymbol, Type[GlobalSymbol]]().success[NoType[GlobalSymbol]])) {
            case ((newEnv, newRes), Bind(bindName, bindBody, _)) =>
              val (newEnv2, bindInfo) = inferS(bindBody)(newEnv)
              (newEnv2, bindInfo match {
                case noInfo: NoType[GlobalSymbol] => newRes.flatMap { _ => noInfo.failure }.valueOr { ni => (ni |+| noInfo).failure }
                case _                            => newRes.map { _ + (LocalSymbol(bindName) -> bindInfo) }
              })
          }
          res match {
            case Success(bindInfos) =>
              env2.withLambdaIdx(lambdaIdx) { _.withLocalVarTypesForLet(bindInfos) { inferS(body)(_) } }
            case Failure(noType)    =>
              (env2, noType)
          }
        case Lambda(args, body, lmbdindexer.LambdaInfo(_, lambdaIdx)) =>
          env.withLambdaIdx(lambdaIdx) {
            _.withLocalVarTypes(args.list.flatMap { a => a.name.map { s => (LocalSymbol(s), a.typ) } }.toMap) {
              newEnv =>
                val (newEnv2, retInfo) = inferS(body)(newEnv)
                val (newEnv3, argInfos) = newEnv2.typesFromArgs(args.list)
                functionTypeFromTypesS(argInfos, retInfo)(newEnv3)
            }
          }
        case Var(loc) =>
          (env, env.varType(loc))
        case Literal(value) =>
          value match {
            case BooleanValue(_)          => (env, InferredType.booleanType)
            case CharValue(_)             => (env, InferredType.charType)
            case ByteValue(x)             => (env, InferredType.fromByte(x))
            case ShortValue(x)            => (env, InferredType.fromShort(x))
            case IntValue(x)              => (env, InferredType.fromInt(x))
            case LongValue(x)             => (env, InferredType.fromLong(x))
            case FloatValue(_)            => (env, InferredType.floatType)
            case DoubleValue(_)           => (env, InferredType.doubleType)
            case TupleFunValue(n)         => (env, InferredType.tupleFunType(n))
            case TupleFieldFunValue(n, i) => (env, InferredType.tupleFieldFunType(n, i))
            case BuiltinFunValue(bf)      => (env, InferredType.fromBuiltinFunction(bf))
          }
        case TypedTerm(term, typ) =>
          val (env2, info) = inferS(term)(env)
          val (env3, res) = env2.definedTypeFromTypeTerm(typ)
          res.map { dt => unifyArgInfosS(InferringType(dt.term), info)(env3.withDefinedType(dt)) }.valueOr { (env3, _) }
        case construct @ Construct(_, _) =>
          inferConstructTypeS(construct)(env)
        case select @ Select(_, _, _) =>
          inferSelectTypeS(select)(env)
        case extract @ Extract(_, _, _, _) =>
          inferExtractTypeS(extract)(env)
      }
    
    private def unifyTypesForTypeMatchingS(type1: Type[GlobalSymbol], type2: Type[GlobalSymbol], typeMatching: TypeMatching.Value)(env: SymbolTypeInferenceEnvironment[T, U]) = {
      val (env2, res) = type1.instantiatedTypeValueTermWithKindsS(env)
      val (env3, res2) = type2.instantiatedTypeValueTermWithKindsS(env2)
      (res |@| res2) {
        case ((tvt1, ks1), (tvt2, ks2)) =>
          env3.withTypePair(some((InferredType(tvt1, ks1), InferredType(tvt2, ks2)))) { 
            _.withTypeMatching(typeMatching){ env4 => unifyTypesS(type1, type2)(env4) }
          }
      }.valueOr { (env3, _) }
    }
    
    override def unifyInfosS(info1: Type[GlobalSymbol], info2: Type[GlobalSymbol])(env: SymbolTypeInferenceEnvironment[T, U]) =
      unifyTypesForTypeMatchingS(info1, info2, TypeMatching.Types)(env)

    override def unifyArgInfosS(funArgInfo: Type[GlobalSymbol], argInfo: Type[GlobalSymbol])(env: SymbolTypeInferenceEnvironment[T, U]) =
      unifyTypesForTypeMatchingS(funArgInfo, argInfo, TypeMatching.SupertypeWithType)(env)
    
    override def argInfosFromInfoS(info: Type[GlobalSymbol], argCount: Int)(env: SymbolTypeInferenceEnvironment[T, U]) =
      argTypesFromTypeS(info, argCount)(env)
    
    override def returnInfoFromInfoS(info: Type[GlobalSymbol], argCount: Int)(env: SymbolTypeInferenceEnvironment[T, U]) =
      returnTypeFromTypeS(info, argCount)(env)
    
    override def isNoInfo(info: Type[GlobalSymbol]) =
      info.isNoType
    
    override def functionInfo(argCount: Int) =
      functionType(argCount)
    
    override def concatErrors(info1: Type[GlobalSymbol], info2: Type[GlobalSymbol]) =
      (info1, info2) match {
        case (noType1: NoType[GlobalSymbol], noType2: NoType[GlobalSymbol]) => noType1 |+| noType2
        case (noType: NoType[GlobalSymbol], _)                              => noType
        case (_, noType: NoType[GlobalSymbol])                              => noType
        case _                                                              => NoType.fromError[GlobalSymbol](FatalError("can't concat errors", none, NoPosition))
      }
    
    override def unequalListLengthNoInfo =
      NoType.fromError(FatalError("unequal list lengths", none, NoPosition))
    
    override def withPos(res: (SymbolTypeInferenceEnvironment[T, U], Type[GlobalSymbol]))(pos: Position) =
      (res._1, res._2.withPos(pos))
  }
  
  implicit def symbolCombinatorTypeRecursiveInitializer[T, U](implicit unifier: Unifier[NoType[GlobalSymbol], TypeValueTerm[GlobalSymbol], SymbolTypeInferenceEnvironment[T, U], Int], envSt: TypeInferenceEnvironmentState[SymbolTypeInferenceEnvironment[T, U], GlobalSymbol, GlobalSymbol]): RecursiveInitializer[NoType[GlobalSymbol], GlobalSymbol, AbstractCombinator[Symbol, lmbdindexer.LambdaInfo[T], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]], CombinatorNode[Symbol, T, TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol], SymbolTypeInferenceEnvironment[T, U]] = new RecursiveInitializer[NoType[GlobalSymbol], GlobalSymbol, AbstractCombinator[Symbol, lmbdindexer.LambdaInfo[T], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]], CombinatorNode[Symbol, T, TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol], SymbolTypeInferenceEnvironment[T, U]] {
    override def combinatorFromNode(node: CombinatorNode[Symbol, T, TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]) = node.comb
    
    override def recursiveCombinatorsFromNode(node: CombinatorNode[Symbol, T, TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]) = node.recursiveCombSyms
    
    override def markedRecursiveCombinatorsFromNode(node: CombinatorNode[Symbol, T, TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]) = node.markedRecCombSyms
    
    override def createNode(comb: AbstractCombinator[Symbol, lmbdindexer.LambdaInfo[T], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]], recursiveCombLocs: Set[GlobalSymbol], markedRecCombLocs: Set[GlobalSymbol]) =
      CombinatorNode(comb, recursiveCombLocs, markedRecCombLocs)
    
    override def addNodeS(loc: GlobalSymbol, node: CombinatorNode[Symbol, T, TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol])(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env.withComb(loc, node), ())
    
    override def isRecursiveFromEnvironmentS(env: SymbolTypeInferenceEnvironment[T, U]) = (env, env.isRecursive)
    
    override def isUninitializedGlobalVarS(loc: GlobalSymbol)(env: SymbolTypeInferenceEnvironment[T, U]) = (env, env.varType(loc).isUninferredType)
    
    private def instantiateTypesFromGlobalVarsS(syms: Set[GlobalSymbol])(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], Unit]) = {
      val (env2, res) = instantiateTypeMapS(syms.map { s => (s, env.varType(s)) }.toMap)(env)
      res.map {
        ts =>
          val (env3, res2) = syms.flatMap { s => env2.lambdaInfos.get(some(s)).map { (s, _) } }.foldLeft((env2, Map[Option[GlobalSymbol], Map[Int, InferenceLambdaInfo[LocalSymbol, GlobalSymbol]]]().success[NoType[GlobalSymbol]])) {
            case ((newEnv, Success(liMaps)), (s, lis)) =>
              lis.foldLeft((newEnv, Map[Int, InferenceLambdaInfo[LocalSymbol, GlobalSymbol]]().success[NoType[GlobalSymbol]])) {
                case ((newEnv2, Success(newLis)), (i, li)) =>
                  val (newEnv3, newRes) = instantiateTypeMapS(li.typeTable.types)(newEnv2)
                  newRes.map {
                    ts => instantiateTypesS(li.instanceTypes)(newEnv3).mapElements(identity, _.map { its => newLis + (i -> li.copy(typeTable = TypeTable(ts), instanceTypes = its)) })
                  }.valueOr { nt => (newEnv2, nt.failure) }
                case ((newEnv2, Failure(nt)), _)           =>
                  (newEnv2, nt.failure)
              }.mapElements(identity, _.map { lis2 => liMaps + (some(s) -> lis2) })
            case ((newEnv, Failure(nt)), _)            =>
              (newEnv, nt.failure)
          }
          res2.map {
            lis => (env3.withGlobalVarTypes(ts).withLambdaInfos(env3.lambdaInfos ++ lis), ().success)
          }.valueOr { nt => (env3, nt.failure) }
      }.valueOr { nt => (env2, nt.failure) }
    }
    
    private def failInitializationS(noType: NoType[GlobalSymbol], syms: Set[GlobalSymbol])(env: SymbolTypeInferenceEnvironment[T, U]) =
      if(noType.errs.forall { _.isInstanceOf[Error] })
        (env.withErrs(noType).withGlobalVarTypes(syms.map { s => (s, NoType.fromError[GlobalSymbol](Error("uninferred type of global variable " + s, none, NoPosition))) }.toMap), ().success)
      else
        (env, noType.failure)
    
    override def nonRecursivelyInitializeGlobalVarS(loc: GlobalSymbol, comb: AbstractCombinator[Symbol, lmbdindexer.LambdaInfo[T], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]])(env: SymbolTypeInferenceEnvironment[T, U]) = {
      comb match {
        case Combinator(typ, args, body, lmbdindexer.LambdaInfo(_, lambdaIdx), file) =>
          (for {
            // Infers the type.
            tmpCombType <- State((_: SymbolTypeInferenceEnvironment[T, U]).withCombSym(some(loc)) {
              _.withLambdaIdx(lambdaIdx) {
                _.withLocalVarTypes(args.flatMap { a => a.name.map { s => (LocalSymbol(s), a.typ) } }.toMap) {
                  newEnv =>
                    val (newEnv2, retInfo) = inferS(body)(newEnv)
                    val (newEnv3, argInfos) = newEnv2.typesFromArgs(args)
                    functionTypeFromTypesS(argInfos, retInfo)(newEnv3)
                }
              }
            })
            // Unifies the inferred type with the defined type.
            tmpCombType2 <- typ.map {
              tt =>
                for {
                  res <- State((_: SymbolTypeInferenceEnvironment[T, U]).definedTypeFromTypeTerm(tt))
                  tmpType2 <- res.map {
                    dt =>
                      for {
                        _ <- State((env2: SymbolTypeInferenceEnvironment[T, U]) => (env2.withDefinedType(dt), ()))
                        tmpType <- State(symbolSimpleTermTypeInferrer.unifyArgInfosS(InferringType(dt.term), tmpCombType)(_: SymbolTypeInferenceEnvironment[T, U]))
                      } yield tmpType
                  }.valueOr { nt => State((_: SymbolTypeInferenceEnvironment[T, U], nt))}
                } yield tmpType2
            }.getOrElse(State((_: SymbolTypeInferenceEnvironment[T, U], tmpCombType)))
            // Checks the defined types.
            isRecursive <- State(isRecursiveFromEnvironmentS)
            res2 <- if(!isRecursive)
              for {
                definedTypes <- State((env2: SymbolTypeInferenceEnvironment[T, U]) => (env2, env2.definedTypes))
                res <- checkDefinedTypes(definedTypes)
              } yield res
            else
              State((_: SymbolTypeInferenceEnvironment[T, U], ().success))
            // Instantiates the inferred types.
            res4 <- res2.map {
              _ =>
                tmpCombType2 match {
                  case noType: NoType[GlobalSymbol] =>
                    State(failInitializationS(noType, Set(loc)))
                  case _                            =>
                    for {
                      _ <- State((env2: SymbolTypeInferenceEnvironment[T, U]) => (env2.withGlobalVarType(loc, tmpCombType), ()))
                      res3 <- if(!isRecursive)
                        State(instantiateTypesFromGlobalVarsS(Set(loc)))
                      else
                        State((_: SymbolTypeInferenceEnvironment[T, U], ().success))
                    } yield res3
                }
            }.valueOr { nk => State(failInitializationS(nk, Set(loc))) }
          } yield res4).run(env)
      }
    }
    
    override def checkInitializationS(res: Validation[NoType[GlobalSymbol], Unit], combLocs: Set[GlobalSymbol], oldNodes: Map[GlobalSymbol, CombinatorNode[Symbol, T, TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]])(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], Unit]) = {
      val (env2, res2) = checkDefinedTypesS(env.definedTypes)(env)
      (res |@| res2) {
        (_, _) => instantiateTypesFromGlobalVarsS(combLocs)(env2)
      }.valueOr { failInitializationS(_, combLocs)(env2) }
    }
    
    override def nodesFromEnvironmentS(env: SymbolTypeInferenceEnvironment[T, U]) = (env, env.combNodes)
    
    override def withRecursiveS[V](combLocs: Set[GlobalSymbol], newNodes: Map[GlobalSymbol, CombinatorNode[Symbol, T, TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]])(f: SymbolTypeInferenceEnvironment[T, U] => (SymbolTypeInferenceEnvironment[T, U], V))(env: SymbolTypeInferenceEnvironment[T, U]) = {
      val (env2, res) = f(env.withRecursive(true).withoutGlobalVarTypes(combLocs))
      (env2.withRecursive(false).withCombNodes(newNodes), res)
    }
    
    override def withClearS[V](f: SymbolTypeInferenceEnvironment[T, U] => (SymbolTypeInferenceEnvironment[T, U], V))(env: SymbolTypeInferenceEnvironment[T, U]) =
      env.withClear(f)
  }
  
  implicit def symbolCombinatorTypeInitializer[T, U]: Initializer[NoType[GlobalSymbol], GlobalSymbol, AbstractCombinator[Symbol, lmbdindexer.LambdaInfo[T], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]], SymbolTypeInferenceEnvironment[T, U]] = new Initializer[NoType[GlobalSymbol], GlobalSymbol, AbstractCombinator[Symbol, lmbdindexer.LambdaInfo[T], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]], SymbolTypeInferenceEnvironment[T, U]] {
    override def globalVarsFromEnvironmentS(env: SymbolTypeInferenceEnvironment[T, U]) = (env, env.globalVarTypes.keySet)
    
    override def usedGlobalVarsFromCombinator(comb: AbstractCombinator[Symbol, lmbdindexer.LambdaInfo[T], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]]) =
      comb match {
        case Combinator(_, _, body, _, _) => usedGlobalVarsFromTerm(body)
      }
    
    override def prepareGlobalVarS(loc: GlobalSymbol)(env: SymbolTypeInferenceEnvironment[T, U]) =
      if(!env.isRecursive) {
        (env.withGlobalVarType(loc, UninferredType[GlobalSymbol]()), ())
      } else {
        val (env2, res) = allocateTypeValueTermParamsWithKindsS(TypeParamApp(0, Nil, 0), Map(0 -> InferredKind(Star(KindType, NoPosition))))(Map(), 0)(env)
        res.map { f => (env2.withGlobalVarType(loc, InferringType(f._4)), ()) }.valueOr { nt => (env2.withGlobalVarType(loc, nt), ()) }
      }
    
    override def initializeGlobalVarS(loc: GlobalSymbol, comb: AbstractCombinator[Symbol, lmbdindexer.LambdaInfo[T], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]])(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], Validation[NoType[GlobalSymbol], Unit]) = {
      val (env2, res) = recursivelyInitializeGlobalVarS(loc, comb)(resolver.TreeInfo(Tree(Map[GlobalSymbol, AbstractTypeCombinator[Symbol, TypeLambdaInfo[U, LocalSymbol]]](), resolver.TypeTreeInfo)))(env)
      (env2, res.swap.map { _.forFile(comb.file) }.swap)
    }
    
    override def checkEnvironmentS(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env, env.errNoType.map { _.failure}.getOrElse(().success))
    
    override def undefinedGlobalVarError =
      NoType.fromError(FatalError("undefined global variable", none, NoPosition))
    
    override def withSaveS[V, W](f: SymbolTypeInferenceEnvironment[T, U] => (SymbolTypeInferenceEnvironment[T, U], Validation[V, W]))(env: SymbolTypeInferenceEnvironment[T, U]) = {
      val (env2, res) = f(env)
      res.map { x => (env2, x.success) }.valueOr { e => (env, e.failure ) }        
    }
  }
  
  implicit def symbolTypeInferenceEnvironmental[T, U]: TypeInferenceEnvironmental[SymbolTypeInferenceEnvironment[T, U], GlobalSymbol, LocalSymbol, GlobalSymbol] = new TypeInferenceEnvironmental[SymbolTypeInferenceEnvironment[T, U], GlobalSymbol, LocalSymbol, GlobalSymbol] {
    override def copyEnvironment(env: SymbolTypeInferenceEnvironment[T, U]) = env
    
    override def globalVarTypeFromEnvironment(env: SymbolTypeInferenceEnvironment[T, U])(sym: GlobalSymbol) =
      env.varType(sym)
      
    override def lambdaInfosFromEnvironment(env: SymbolTypeInferenceEnvironment[T, U])(sym: Option[GlobalSymbol]) =
      env.lambdaInfos.getOrElse(sym, Map())
    
    override def getLambdaInfoFromEnvironment(env: SymbolTypeInferenceEnvironment[T, U])(lambdaIdx: Int) =
      env.lambdaInfos.getOrElse(env.currentCombSym, Map()).get(lambdaIdx)
    
    override def globalTypeTableFromEnvironment(env: SymbolTypeInferenceEnvironment[T, U]) =
      TypeTable(env.globalVarTypes)
    
    override def withCurrentCombinatorLocation(env: SymbolTypeInferenceEnvironment[T, U])(loc: Option[GlobalSymbol]) =
      env.withCurrentCombSym(loc)
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