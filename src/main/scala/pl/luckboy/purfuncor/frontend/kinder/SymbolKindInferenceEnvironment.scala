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
    currentLambdaIdx: Int,
    globalTypeVarKinds: Map[GlobalSymbol, Kind],
    localTypeVarKindMaps: IntMap[Map[LocalSymbol, Kind]],
    kindParamForest: ParamForest[KindParamTerm])
{
  def withCurrentLambdaIdx(idx: Int) = copy(currentLambdaIdx = idx)
  
  def localTypeVarKinds = localTypeVarKindMaps.getOrElse(currentLambdaIdx, Map())
  
  def typeVarKinds(sym: Symbol) =
    sym match {
      case globalSym @ GlobalSymbol(_) =>
        globalTypeVarKinds.getOrElse(globalSym, NoKind.fromError(FatalError("undefined global type variable", none, NoPosition)))
      case localSym @ LocalSymbol(_)   =>
        localTypeVarKindMaps.get(currentLambdaIdx).map {
          _.getOrElse(localSym, NoKind.fromError(FatalError("undefined local type variable", none, NoPosition)))
        }.getOrElse(NoKind.fromError(FatalError("current lambda index is illegal", none, NoPosition)))
    }
  
  def withLocalTypeVarKinds(kindTerms: Map[LocalSymbol, Option[KindTerm[StarKindTerm[String]]]]): SymbolKindInferenceEnvironment =
    throw new UnsupportedOperationException
    
  def withKindParamForest(kindParamForest: ParamForest[KindParamTerm]) = copy(kindParamForest = kindParamForest)
}