package pl.luckboy.purfuncor.frontend.kinder
import scala.collection.immutable.IntMap
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol

case class SymbolKindInferenceEnvironment(
    currentLambdaIdx: Int,
    globalTypeVarKinds: Map[GlobalSymbol, Kind[String]],
    localTypeVarKindMaps: IntMap[Map[LocalSymbol, Kind[String]]])
{
  def withCurrentLambdaIdx(idx: Int) = copy(currentLambdaIdx = idx)
  
  def localTypeVarKinds = localTypeVarKindMaps.getOrElse(currentLambdaIdx, Map())
  
  def typeVarKinds(sym: Symbol): Kind[String] =
    throw new UnsupportedOperationException
  
  def withLocalTypeVarKinds(kindTerms: Map[LocalSymbol, Option[KindTerm[StarKindTerm[String]]]]): SymbolKindInferenceEnvironment =
    throw new UnsupportedOperationException
}