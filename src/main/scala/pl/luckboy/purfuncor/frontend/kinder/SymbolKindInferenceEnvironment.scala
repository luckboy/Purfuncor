package pl.luckboy.purfuncor.frontend.kinder
import scala.collection.immutable.IntMap
import scala.util.parsing.input.NoPosition
import scala.annotation.tailrec
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol

case class SymbolKindInferenceEnvironment(
    currentTypeCombSym: Option[GlobalSymbol],
    currentTypeLambdaIdx: Int,
    globalTypeVarKinds: Map[GlobalSymbol, Kind],
    localTypeVarKindMapMaps: Map[GlobalSymbol, IntMap[Map[LocalSymbol, Kind]]],
    kindParamForest: ParamForest[KindTerm[StarKindTerm[Int]]],
    currentKindTermPair: Option[(KindTerm[StarKindTerm[Int]], KindTerm[StarKindTerm[Int]])])
{
  def withCurrentTypeCombSym(sym: Option[GlobalSymbol]) = copy(currentTypeCombSym = sym)
  
  def withCurrentTypeLambdaIdx(lambdaIdx: Int) = copy(currentTypeLambdaIdx = lambdaIdx)
  
  def withTypeLambdaIdx[T](lambdaIdx: Int)(f: SymbolKindInferenceEnvironment => (SymbolKindInferenceEnvironment, T)) = {
    val oldLambdaIdx = currentTypeLambdaIdx
    val (env, res) = f(withCurrentTypeLambdaIdx(lambdaIdx))
    (env.withCurrentTypeLambdaIdx(oldLambdaIdx), res)
  }
  
  def localTypeVarKinds = currentTypeCombSym.map { localTypeVarKindMapMaps.getOrElse(_, IntMap()).getOrElse(currentTypeLambdaIdx, Map()) }
  
  def typeVarKind(sym: Symbol) =
    sym match {
      case globalSym @ GlobalSymbol(_) =>
        globalTypeVarKinds.getOrElse(globalSym, NoKind.fromError(FatalError("undefined global type variable", none, NoPosition)))
      case localSym @ LocalSymbol(_)   =>
        currentTypeCombSym.map {
          localTypeVarKindMapMaps.get(_).map {
            _.get(currentTypeLambdaIdx).map {
              _.getOrElse(localSym, NoKind.fromError(FatalError("undefined local type variable", none, NoPosition)))
            }.getOrElse(NoKind.fromError(FatalError("current type lambda index is illegal", none, NoPosition)))
          }.getOrElse(NoKind.fromError(FatalError("current type combinator symbol is illegal", none, NoPosition)))
        }.getOrElse(NoKind.fromError(FatalError("no current type combinator symbol", none, NoPosition)))
    }
  
  def withLocalTypeVarKinds(kindTerms: Map[LocalSymbol, Option[KindTerm[StarKindTerm[Int]]]]): SymbolKindInferenceEnvironment =
    throw new UnsupportedOperationException
    
  def withKindParamForest(kindParamForest: ParamForest[KindTerm[StarKindTerm[Int]]]) = copy(kindParamForest = kindParamForest)
  
  def withCurrentKindTermPair(pair: Option[(KindTerm[StarKindTerm[Int]], KindTerm[StarKindTerm[Int]])]) = copy(currentKindTermPair = pair)
  
  def withKindTermPair[T](pair: Option[(KindTerm[StarKindTerm[Int]], KindTerm[StarKindTerm[Int]])])(f: SymbolKindInferenceEnvironment => (SymbolKindInferenceEnvironment, T)) = {
    val oldKindTermPair = currentKindTermPair
    val (env, res) = f(withCurrentKindTermPair(pair))
    (env.withCurrentKindTermPair(oldKindTermPair), res)
  }
}

object SymbolKindInferenceEnvironment
{
  val empty = SymbolKindInferenceEnvironment(
      currentTypeCombSym = none,
      currentTypeLambdaIdx = 0,
      globalTypeVarKinds = Map(),
      localTypeVarKindMapMaps = Map(),
      kindParamForest = ParamForest.empty,
      currentKindTermPair = none)
}