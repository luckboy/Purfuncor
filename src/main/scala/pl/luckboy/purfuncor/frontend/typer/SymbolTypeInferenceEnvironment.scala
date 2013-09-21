package pl.luckboy.purfuncor.frontend.typer
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
    typeParamKinds: Map[GlobalSymbol, Kind],
    typeRetKind: Kind,
    combNodes: Map[GlobalSymbol, CombinatorNode[Symbol, U, TypeSimpleTerm[Symbol, T], GlobalSymbol]],
    defindTypes: List[InferredType[GlobalSymbol]],
    irreplaceableTypeParams: Map[Int, TypeValueTerm[GlobalSymbol]],
    currentTypePair: (TypeValueTerm[GlobalSymbol], TypeValueTerm[GlobalSymbol]),
    errNoType: Option[NoType[GlobalSymbol]],
    isRecursive: Boolean)