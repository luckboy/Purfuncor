package pl.luckboy.purfuncor.frontend.kinder
import scala.collection.immutable.BitSet
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
import pl.luckboy.purfuncor.frontend.KindTermUtils._
import KindTermUnifier._

case class SymbolKindInferenceEnvironment(
    currentTypeCombSym: Option[GlobalSymbol],
    currentTypeLambdaIdx: Int,
    globalTypeVarKinds: Map[GlobalSymbol, Kind],
    localTypeVarKindMapMaps: Map[Option[GlobalSymbol], IntMap[Map[LocalSymbol, Kind]]],
    kindParamForest: ParamForest[KindTerm[StarKindTerm[Int]]],
    definedKindTerms: List[KindTerm[StarKindTerm[Int]]],
    definedKindTermNels: Map[Int, NonEmptyList[KindTerm[StarKindTerm[Int]]]],
    currentKindTermPair: Option[(KindTerm[StarKindTerm[Int]], KindTerm[StarKindTerm[Int]])])
{
  def withCurrentTypeCombSym(sym: Option[GlobalSymbol]) = copy(currentTypeCombSym = sym)
  
  def withCurrentTypeLambdaIdx(lambdaIdx: Int) = copy(currentTypeLambdaIdx = lambdaIdx)
  
  def withTypeLambdaIdx[T](lambdaIdx: Int)(f: SymbolKindInferenceEnvironment => (SymbolKindInferenceEnvironment, T)) = {
    val oldLambdaIdx = currentTypeLambdaIdx
    val (env, res) = f(withCurrentTypeLambdaIdx(lambdaIdx))
    (env.withCurrentTypeLambdaIdx(oldLambdaIdx), res)
  }
  
  def localTypeVarKinds = localTypeVarKindMapMaps.getOrElse(currentTypeCombSym, IntMap()).getOrElse(currentTypeLambdaIdx, Map())
  
  def typeVarKind(sym: Symbol) =
    sym match {
      case globalSym @ GlobalSymbol(_) =>
        globalTypeVarKinds.getOrElse(globalSym, NoKind.fromError(FatalError("undefined global type variable", none, NoPosition)))
      case localSym @ LocalSymbol(_)   =>
        localTypeVarKindMapMaps.get(currentTypeCombSym).map {
          _.get(currentTypeLambdaIdx).map {
            _.getOrElse(localSym, NoKind.fromError(FatalError("undefined local type variable", none, NoPosition)))
          }.getOrElse(NoKind.fromError(FatalError("current type lambda index is illegal", none, NoPosition)))
        }.getOrElse(NoKind.fromError(FatalError("current type combinator symbol is illegal", none, NoPosition)))
    }
  
  def putLocalTypeVarKinds[T](kindTerms: Map[LocalSymbol, Option[KindTerm[StarKindTerm[T]]]]) = {
    val localTypeVarKindMap = localTypeVarKindMapMaps.getOrElse(currentTypeCombSym, IntMap())
    val localTypeVarKinds = localTypeVarKindMap.getOrElse(currentTypeLambdaIdx, Map())
    kindTerms.foldLeft((this, localTypeVarKinds).success[NoKind]) {
      case (Success((newEnv, newLocalTypeVarKinds)), (sym, kt)) =>
        val kindTerm = kt.getOrElse(Star(KindParam(0), NoPosition))
        val (newEnv2, res) = allocateKindTermParamsS(kindTerm)(Map())(newEnv)
        res.map {
          case (_, kt2) =>
            (kt.map { _ => newEnv2.withDefinedKindTerm(kt2) }.getOrElse(newEnv2), newLocalTypeVarKinds + (sym -> InferringKind(kt2)))
        }
      case (Failure(nk), _)                                     =>
        nk.failure
    }.map {
      case (newEnv, newLocalTypeVarKinds) =>
        newEnv.copy(localTypeVarKindMapMaps = newEnv.localTypeVarKindMapMaps + (currentTypeCombSym -> (localTypeVarKindMap + (currentTypeLambdaIdx -> newLocalTypeVarKinds))))
    }
  }
    
  def withKindParamForest(kindParamForest: ParamForest[KindTerm[StarKindTerm[Int]]]) = copy(kindParamForest = kindParamForest)
  
  def withDefinedKindTerm(kindTerm: KindTerm[StarKindTerm[Int]]) =
    copy(
        definedKindTerms = definedKindTerms :+ kindTerm,
        definedKindTermNels = IntMap() ++ (definedKindTermNels.toMap |+| kindParamsFromKindTerm(kindTerm).map { (_, NonEmptyList(kindTerm)) }.toMap))
  
  def withCurrentKindTermPair(pair: Option[(KindTerm[StarKindTerm[Int]], KindTerm[StarKindTerm[Int]])]) = copy(currentKindTermPair = pair)
  
  def withKindTermPair[T](pair: Option[(KindTerm[StarKindTerm[Int]], KindTerm[StarKindTerm[Int]])])(f: SymbolKindInferenceEnvironment => (SymbolKindInferenceEnvironment, T)) = {
    val oldKindTermPair = currentKindTermPair
    val (env, res) = f(withCurrentKindTermPair(pair))
    (env.withCurrentKindTermPair(oldKindTermPair), res)
  }
  
  def withGlobalTypeVarKind(sym: GlobalSymbol, kind: Kind): SymbolKindInferenceEnvironment = copy(globalTypeVarKinds = globalTypeVarKinds + (sym -> kind))
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
      definedKindTermNels = IntMap(),
      currentKindTermPair = none)
}