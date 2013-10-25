package pl.luckboy.purfuncor.frontend.typer
import scala.collection.immutable.IntMap
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol
import pl.luckboy.purfuncor.frontend.kinder.Kind
import pl.luckboy.purfuncor.frontend.kinder.SymbolKindInferenceEnvironment
import pl.luckboy.purfuncor.frontend.kinder.TypeLambdaInfo
import TypeValueTermUnifier._

case class SymbolTypeInferenceEnvironment[T, U](
    typeEnv: SymbolTypeEnvironment[TypeLambdaInfo[T, LocalSymbol]],
    kindInferenceEnv: SymbolKindInferenceEnvironment[U],
    currentCombSym: Option[GlobalSymbol],
    currentLambdaIdx: Int,
    globalVarTypes: Map[GlobalSymbol, Type[GlobalSymbol]],
    localVarTypes: Map[LocalSymbol, NonEmptyList[Type[GlobalSymbol]]],
    localTypeTables: Map[Option[GlobalSymbol], Map[Int, TypeTable[LocalSymbol, GlobalSymbol]]],
    typeParamForest: ParamForest[TypeValueTerm[GlobalSymbol]],
    typeRetKind: Kind,
    combNodes: Map[GlobalSymbol, CombinatorNode[Symbol, U, TypeSimpleTerm[Symbol, T], GlobalSymbol]],
    definedTypes: List[DefinedType[GlobalSymbol]],
    irreplaceableTypeParams: Map[Int, NonEmptyList[DefinedType[GlobalSymbol]]],
    matchingGlobalTypeSyms: Set[GlobalSymbol],
    delayedErrNoTypes: Map[Int, NoType[GlobalSymbol]],
    prevDelayedErrTypeParamAppIdxs: Set[Int],
    nextTypeParamAppIdx: Int,
    typeLambdaArgParams: Map[Int, Int],
    typeLambdaArgCount: Int,
    currentTypeMatching: TypeMatching.Value,
    currentTypeValueTermPair: (TypeValueTerm[GlobalSymbol], TypeValueTerm[GlobalSymbol]),
    errNoType: Option[NoType[GlobalSymbol]],
    isRecursive: Boolean)
{
  def typeParamKinds = kindInferenceEnv.typeParamKinds
  
  def withTypeEnv(env: SymbolTypeEnvironment[TypeLambdaInfo[T, LocalSymbol]]) = copy(typeEnv = env)
  
  def withKindInferenceEnv(env: SymbolKindInferenceEnvironment[U]) = copy(kindInferenceEnv = env)
  
  def withCurrentCombSym(sym: Option[GlobalSymbol]) = copy(currentCombSym = sym)
  
  def withCombSym(sym: Option[GlobalSymbol])(f: SymbolTypeInferenceEnvironment[T, U] => (SymbolTypeInferenceEnvironment[T, U], Type[GlobalSymbol])) = {
    val oldSym = currentCombSym
    val (env, value) = f(withCurrentCombSym(sym))
    (env.withCurrentCombSym(oldSym), value)
  }
  
  def withCurrentLambdaIdx(lambdaIdx: Int) = copy(currentLambdaIdx = lambdaIdx)
  
  def withLambdaIdx(lambdaIdx: Int)(f: SymbolTypeInferenceEnvironment[T, U] => (SymbolTypeInferenceEnvironment[T, U], Type[GlobalSymbol])) = {
    val oldLambdaIdx = currentLambdaIdx
    val (env, value) = f(withCurrentLambdaIdx(lambdaIdx))
    (env.withCurrentLambdaIdx(oldLambdaIdx), value)
  }
  
  def varType(sym: Symbol) =
    sym match {
      case globalSym @ GlobalSymbol(_) =>
        globalVarTypes.getOrElse(globalSym, NoType.fromError(FatalError("undefined global variable", none, NoPosition)))
      case localSym @ LocalSymbol(_)   =>
        localVarTypes.get(localSym).map { _.head }.getOrElse(NoType.fromError(FatalError("undefined local variable", none, NoPosition)))
    }
  
  def pushLocalVarTypes(types: Map[LocalSymbol, Type[GlobalSymbol]]) = copy(localVarTypes = types.mapValues { NonEmptyList(_) } |+| localVarTypes)
  
  def popLocalVarTypes(syms: Set[LocalSymbol]) =  copy(localVarTypes = localVarTypes.flatMap { case (s, ts) => if(syms.contains(s)) ts.tail.toNel.map { (s, _) } else some((s, ts)) }.toMap)
  
  def currentLocalTypeTable = localTypeTables.getOrElse(currentCombSym, Map()).getOrElse(currentLambdaIdx, TypeTable(Map()))
  
  def withCurrentLocalTypeTable(typeTable: TypeTable[LocalSymbol, GlobalSymbol]) = copy(localTypeTables = localTypeTables + (currentCombSym -> (localTypeTables.getOrElse(currentCombSym, IntMap()) + (currentLambdaIdx -> typeTable))))
  
  def withLocalTypeTables(typeTables: Map[Option[GlobalSymbol], Map[Int, TypeTable[LocalSymbol, GlobalSymbol]]]) = copy(localTypeTables = typeTables)
  
  def definedTypeFromTypeTerm(typeTerm: Term[TypeSimpleTerm[Symbol, TypeLambdaInfo[T, LocalSymbol]]]) = {
    val (typeEnv2, res) = typeEnv.withPartialEvaluation(false)(DefinedType.evaluateDefinedTypeTerm(typeTerm).run)
    val env = withTypeEnv(typeEnv2)
    res.map {
      case (typeValueTerm, kinds) =>
        val inferredKinds = kinds.map { _._2 }.zipWithIndex.map { _.swap }.toMap 
        val (env2, res2) = allocateTypeValueTermParamsWithKindsS(typeValueTerm, inferredKinds)(Map(), 0)(env)
        (env2, res2.map {
          case (allocatedParams, _, _, typeValueTerm2) =>
            val args = kinds.map { _._1 }.zipWithIndex.map {
              case (kt, p) => allocatedParams.get(p).map { p2 => DefinedTypeArg(some(p2), kt) }.getOrElse(DefinedTypeArg(none, kt))
            }
            DefinedType(args, typeValueTerm2, typeTerm.pos)
        })
    }.valueOr { nt => (env, NoType.fromNoTypeValue(nt).failure) }
  }
  
  def withLocalVarTypes(typeTerms: Map[LocalSymbol, Option[Term[TypeSimpleTerm[Symbol, TypeLambdaInfo[T, LocalSymbol]]]]])(f: SymbolTypeInferenceEnvironment[T, U] => (SymbolTypeInferenceEnvironment[T, U], Type[GlobalSymbol])) = {
    val (env, res) = typeTerms.foldLeft((this, Map[LocalSymbol, Type[GlobalSymbol]]().success[NoType[GlobalSymbol]])) {
      case ((newEnv, Success(newTypes)), (sym, typeTerm)) =>
        val (newEnv2, newRes2) = typeTerm.map {
          newEnv.definedTypeFromTypeTerm(_).mapElements(identity, _.map { dt => (some(dt), InferringType(dt.term)) })
        }.getOrElse {
          val (newEnv2, newRes) = allocateTypeValueTermParamsS(TypeParamApp(0, Nil, 0))(Map(), 0)(newEnv)
          (newEnv2, newRes.map { f => (none, InferringType(f._4)) })
        }
        newRes2.map {
          case (dt, t) => (dt.map(newEnv2.withDefinedType).getOrElse(newEnv2), (newTypes + (sym -> t)).success)
        }.valueOr { nt => (newEnv2, nt.failure) }
      case ((newEnv, Failure(noType)), _ )                =>
        (newEnv, noType.failure)
    }
    res.map {
      newTypes =>
        val (env2, typ) = f(env.pushLocalVarTypes(newTypes).withCurrentLocalTypeTable(TypeTable(env.currentLocalTypeTable.types ++ newTypes)))
        (env2.popLocalVarTypes(newTypes.keySet), typ)
    }.valueOr { nt => (env, nt.failure) }
  }
  
  def withTypeParamForest(paramForest: ParamForest[TypeValueTerm[GlobalSymbol]]) = copy(typeParamForest = paramForest)
  
  def withTypeRetKind(kind: Kind) = copy(typeRetKind = kind)
  
  def withCombNodes(nodes: Map[GlobalSymbol, CombinatorNode[Symbol, U, TypeSimpleTerm[Symbol, T], GlobalSymbol]]) = copy(combNodes = nodes)
  
  def withComb(sym: GlobalSymbol, node: CombinatorNode[Symbol, U, TypeSimpleTerm[Symbol, T], GlobalSymbol]) = copy(combNodes = combNodes + (sym -> node))
  
  def withoutCombs(syms: Set[GlobalSymbol]) = copy(combNodes = combNodes -- syms)
  
  def withDefinedType(definedType: DefinedType[GlobalSymbol]): SymbolTypeInferenceEnvironment[T, U] =
    copy(
        definedTypes = definedTypes :+ definedType,
        irreplaceableTypeParams = IntMap() ++ (irreplaceableTypeParams |+| definedType.args.flatMap { _.param.map { (_, NonEmptyList(definedType)) } }.toMap))
  
  def withMatchingGlobalTypes(syms: Set[GlobalSymbol]) = copy(matchingGlobalTypeSyms = matchingGlobalTypeSyms | syms)
  
  def withoutMatchingGlobalTypes(syms: Set[GlobalSymbol]) = copy(matchingGlobalTypeSyms = matchingGlobalTypeSyms -- syms)
  
  def withGlobalTypes[V](syms: Set[GlobalSymbol])(f: SymbolTypeInferenceEnvironment[T, U] => (SymbolTypeInferenceEnvironment[T, U], V)) = {
    val (env, res) = f(withMatchingGlobalTypes(syms))
    (env.withoutMatchingGlobalTypes(syms), res)
  }

  def withDelayedErrNoTypes(noTypes: Map[Int, NoType[GlobalSymbol]]) = copy(delayedErrNoTypes = noTypes)
  
  def withDelayedErrs(noTypes: Map[Int, NoType[GlobalSymbol]]) = copy(delayedErrNoTypes = delayedErrNoTypes ++ noTypes)

  def withPrevDelayedErrTypeParamAppIdxs(paramAppIdxs: Set[Int]) = copy(prevDelayedErrTypeParamAppIdxs = paramAppIdxs)
  
  def allocateTypeParamAppIdx =
    if(nextTypeParamAppIdx < Integer.MAX_VALUE) {
      val typeParamAppIdx = nextTypeParamAppIdx
      (copy(nextTypeParamAppIdx = nextTypeParamAppIdx + 1), typeParamAppIdx).success
    } else 
      NoType.fromError[GlobalSymbol](FatalError("can't allocate type parameter index", none, NoPosition)).failure
  
  def withTypeLambdaArgParams(params: Map[Int, Int]) = copy(typeLambdaArgParams = params)
  
  def withTypeLambdaArgCount(argCount: Int) = copy(typeLambdaArgCount = argCount)
  
  def withTypeLambdaArgs[V](argParams: Seq[Set[Int]])(f: SymbolTypeInferenceEnvironment[T, U] => (SymbolTypeInferenceEnvironment[T, U], V)): (SymbolTypeInferenceEnvironment[T, U], V) = {
    val env2 = (0 until argParams.size).foldLeft(this: SymbolTypeInferenceEnvironment[T, U]) {
      case (env, i) =>
        val argParamSet = argParams(i)
        env.withTypeLambdaArgParams(env.typeLambdaArgParams ++ argParamSet.map { _ -> (typeLambdaArgCount + i) }.toMap)
    }
    val oldArgCount = env2.typeLambdaArgCount
    val newArgCount = env2.typeLambdaArgCount + argParams.size
    val (env3, res) = f(env2.withTypeLambdaArgCount(newArgCount))
    (env3.withTypeLambdaArgParams(env3.typeLambdaArgParams -- argParams.flatten).withTypeLambdaArgCount(oldArgCount), res)
  }
  
  def withCurrentTypeMatching(typeMatching: TypeMatching.Value) = copy(currentTypeMatching = typeMatching)
  
  def withCurrentTypeValueTermPair(pair: (TypeValueTerm[GlobalSymbol], TypeValueTerm[GlobalSymbol])) = copy(currentTypeValueTermPair = pair)
  
  def withTypeValueTermPair[V](pair: (TypeValueTerm[GlobalSymbol], TypeValueTerm[GlobalSymbol]))(f: SymbolTypeInferenceEnvironment[T, U] => (SymbolTypeInferenceEnvironment[T, U], V)) = {
    val oldPair = currentTypeValueTermPair
    val (env, res) = f(withCurrentTypeValueTermPair(pair))
    (env.withCurrentTypeValueTermPair(oldPair), res)
  }
  
  def withErrs(noType: NoType[GlobalSymbol]) = copy(errNoType = errNoType.map { nt => some(nt |+| noType) }.getOrElse(some(noType)))
  
  def withRecursive(isRecursive: Boolean) = copy(isRecursive = isRecursive)
  
  def withGlobalVarType(sym: GlobalSymbol, typ: Type[GlobalSymbol]) = copy(globalVarTypes = globalVarTypes + (sym -> typ))
  
  def withGlobalVarTypes(types: Map[GlobalSymbol, Type[GlobalSymbol]]) = copy(globalVarTypes = globalVarTypes ++ types)
  
  def withoutGlobalVarTypes(syms: Set[GlobalSymbol]) = copy(globalVarTypes = globalVarTypes -- syms)
}