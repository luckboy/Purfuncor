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
import KindTermUnifier._

case class SymbolKindInferenceEnvironment(
    currentTypeCombSym: Option[GlobalSymbol],
    currentTypeLambdaIdx: Int,
    globalTypeVarKinds: Map[GlobalSymbol, Kind],
    localTypeVarKindMapMaps: Map[GlobalSymbol, IntMap[Map[LocalSymbol, Kind]]],
    kindParamForest: ParamForest[KindTerm[StarKindTerm[Int]]],
    definedKindTerms: List[KindTerm[StarKindTerm[Int]]],
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
  
  def putLocalTypeVarKinds(kindTerms: Map[LocalSymbol, Option[KindTerm[StarKindTerm[Int]]]]) =
    currentTypeCombSym.map {
      typeCombSym =>
        val localTypeVarKindMap = localTypeVarKindMapMaps.getOrElse(typeCombSym, IntMap())
        val localTypeVarKinds = localTypeVarKindMap.getOrElse(currentTypeLambdaIdx, Map())
        kindTerms.foldLeft((this: SymbolKindInferenceEnvironment, localTypeVarKinds).success[NoKind]) {
          case (Success((newEnv, newLocalTypeVarKinds)), (sym, kt)) =>
            val (newEnv2, kindTerm) = kt.map { kt2 => (newEnv.withDefinedKindTerm(kt2), kt2) } .getOrElse((newEnv, Star(KindParam(0), NoPosition)))
            val (newEnv3, res) = allocateKindTermParamsS(kindTerm)(Map())(newEnv2)
            res.map { case (_, kt2) => (newEnv3, newLocalTypeVarKinds + (sym -> InferringKind(kt2))) }
          case (Failure(nk), _)                                     =>
            nk.failure
        }.map {
          case (newEnv, newLocalTypeVarKinds) =>
            newEnv.copy(localTypeVarKindMapMaps = newEnv.localTypeVarKindMapMaps + (typeCombSym -> (localTypeVarKindMap + (currentTypeLambdaIdx -> newLocalTypeVarKinds))))
        }
    }.getOrElse(NoKind.fromError(FatalError("no current type combinator symbol", none, NoPosition)).failure)
    
  def withKindParamForest(kindParamForest: ParamForest[KindTerm[StarKindTerm[Int]]]) = copy(kindParamForest = kindParamForest)
  
  def withDefinedKindTerm(kindTerm: KindTerm[StarKindTerm[Int]]) = copy(definedKindTerms = definedKindTerms :+ kindTerm)
  
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
      definedKindTerms = Nil,
      currentKindTermPair = none)
}