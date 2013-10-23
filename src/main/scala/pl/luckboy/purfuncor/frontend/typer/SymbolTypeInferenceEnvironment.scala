package pl.luckboy.purfuncor.frontend.typer
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

case class SymbolTypeInferenceEnvironment[T, U](
    typeEnv: SymbolTypeEnvironment[lmbdindexer.LambdaInfo[T]],
    kindInferenceEnv: SymbolKindInferenceEnvironment[U],
    currentCombSym: Option[GlobalSymbol],
    currentLambdaIdx: Int,
    globalVarTypes: Map[GlobalSymbol, Type[GlobalSymbol]],
    localVarTypes: Map[LocalSymbol, NonEmptyList[Type[GlobalSymbol]]],
    localTypeTables: Map[Option[GlobalSymbol], TypeTable[LocalSymbol, GlobalSymbol]],
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
    currentTypePair: (TypeValueTerm[GlobalSymbol], TypeValueTerm[GlobalSymbol]),
    errNoType: Option[NoType[GlobalSymbol]],
    isRecursive: Boolean)
{
  def typeParamKinds = kindInferenceEnv.typeParamKinds
  
  def withTypeEnv(env: SymbolTypeEnvironment[lmbdindexer.LambdaInfo[T]]) = copy(typeEnv = env)
  
  def withKindInferenceEnv(env: SymbolKindInferenceEnvironment[U]) = copy(kindInferenceEnv = env)
  
  def withTypeParamForest(paramForest: ParamForest[TypeValueTerm[GlobalSymbol]]) = copy(typeParamForest = paramForest)
  
  def withTypeRetKind(kind: Kind) = copy(typeRetKind = kind)
  
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
}