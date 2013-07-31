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
import pl.luckboy.purfuncor.frontend.KindTermUtils._
import KindTermUnifier._

case class SymbolKindInferenceEnvironment(
    currentTypeCombSym: Option[GlobalSymbol],
    currentTypeLambdaIdx: Int,
    globalTypeVarKinds: Map[GlobalSymbol, Kind],
    localTypeVarKinds: Map[LocalSymbol, NonEmptyList[Kind]],
    localKindTables: Map[Option[GlobalSymbol], Map[Int, KindTable[LocalSymbol]]],
    kindParamForest: ParamForest[KindTerm[StarKindTerm[Int]]],
    globalInferringKindCount: Int,
    definedKindTerms: List[KindTerm[StarKindTerm[Int]]],
    irreplaceableKindParams: Map[Int, NonEmptyList[KindTerm[StarKindTerm[Int]]]],
    currentKindTermPair: Option[(KindTerm[StarKindTerm[Int]], KindTerm[StarKindTerm[Int]])])
{
  def withCurrentTypeCombSym(sym: Option[GlobalSymbol]) = copy(currentTypeCombSym = sym)
  
  def withCurrentTypeLambdaIdx(lambdaIdx: Int) = copy(currentTypeLambdaIdx = lambdaIdx)
  
  def withTypeLambdaIdx(lambdaIdx: Int)(f: SymbolKindInferenceEnvironment => (SymbolKindInferenceEnvironment, Kind)) = {
    val oldLambdaIdx = currentTypeLambdaIdx
    val (env, kind) = f(withCurrentTypeLambdaIdx(lambdaIdx))
    (env.withCurrentTypeLambdaIdx(oldLambdaIdx), kind)
  }
  
  def typeVarKind(sym: Symbol) =
    sym match {
      case globalSym @ GlobalSymbol(_) =>
        globalTypeVarKinds.getOrElse(globalSym, NoKind.fromError(FatalError("undefined global type variable", none, NoPosition)))
      case localSym @ LocalSymbol(_)   =>
        localTypeVarKinds.get(localSym).map { _ head}.getOrElse(NoKind.fromError(FatalError("undefined local type variable", none, NoPosition)))
    }
  
  def pushLocalVarKinds(kinds: Map[LocalSymbol, Kind]) = copy(localTypeVarKinds = localTypeVarKinds |+| kinds.mapValues { NonEmptyList(_) })
  
  def popLocalVarKinds(syms: Set[LocalSymbol]) = copy(localTypeVarKinds = localTypeVarKinds.flatMap { case (s, ks) => if(syms.contains(s)) ks.tail.toNel.map { (s, _) } else some((s, ks)) })
  
  def currentLocalKindTable = localKindTables.getOrElse(currentTypeCombSym, Map()).getOrElse(currentTypeLambdaIdx, KindTable.empty[LocalSymbol])
  
  def withCurrentLocalKindTable(kindTable: KindTable[LocalSymbol]) = copy(localKindTables = localKindTables ++ Map(currentTypeCombSym -> (localKindTables.getOrElse(currentTypeCombSym, IntMap()) + (currentTypeLambdaIdx -> kindTable))))
  
  def withLocalTypeVarKinds[T](kindTerms: Map[LocalSymbol, Option[KindTerm[StarKindTerm[T]]]])(f: SymbolKindInferenceEnvironment => (SymbolKindInferenceEnvironment, Kind)) = {
    val kinds = localTypeVarKinds.mapValues { _.head }
    val (env2, res) = kindTerms.foldLeft((this, kinds.success[NoKind])) {
      case ((newEnv, Success(newKinds)), (sym, kt)) =>
        val kindTerm = kt.getOrElse(Star(KindParam(0), NoPosition))
        val (newEnv2, newRes) = allocateKindTermParamsS(kindTerm)(Map())(newEnv)
        newRes.map {
          p => (kt.map { _ => newEnv2.withDefinedKindTerm(p._2) }.getOrElse(newEnv2), (newKinds + (sym -> InferringKind(p._2))).success)
        }.valueOr { nk => (newEnv2, nk.failure) }
      case ((newEnv, Failure(nk)), _)               =>
        (newEnv, nk.failure)
    }
    res.map {
      newKinds =>
        val (env3, kind) = f(env2.pushLocalVarKinds(newKinds).withCurrentLocalKindTable(KindTable(currentLocalKindTable.kinds ++ newKinds)))
        (env3.popLocalVarKinds(newKinds.keySet), kind)
    }.valueOr { (env2, _) } 
  }
    
  def withKindParamForest(kindParamForest: ParamForest[KindTerm[StarKindTerm[Int]]]) = copy(kindParamForest = kindParamForest)
  
  def withDefinedKindTerm(kindTerm: KindTerm[StarKindTerm[Int]]) =
    copy(
        definedKindTerms = definedKindTerms :+ kindTerm,
        irreplaceableKindParams = IntMap() ++ (irreplaceableKindParams.toMap |+| kindParamsFromKindTerm(kindTerm).map { (_, NonEmptyList(kindTerm)) }.toMap))
  
  def withCurrentKindTermPair(pair: Option[(KindTerm[StarKindTerm[Int]], KindTerm[StarKindTerm[Int]])]) = copy(currentKindTermPair = pair)
  
  def withKindTermPair[T](pair: Option[(KindTerm[StarKindTerm[Int]], KindTerm[StarKindTerm[Int]])])(f: SymbolKindInferenceEnvironment => (SymbolKindInferenceEnvironment, T)) = {
    val oldKindTermPair = currentKindTermPair
    val (env, res) = f(withCurrentKindTermPair(pair))
    (env.withCurrentKindTermPair(oldKindTermPair), res)
  }
  
  def withGlobalTypeVarKind(sym: GlobalSymbol, kind: Kind): SymbolKindInferenceEnvironment =
    copy(
        globalTypeVarKinds = globalTypeVarKinds + (sym -> kind),
        globalInferringKindCount = globalInferringKindCount - (if(typeVarKind(sym).isInferringKind) 1 else 0) + (if(kind.isInferringKind) 1 else 0))
}

object SymbolKindInferenceEnvironment
{
  val empty = SymbolKindInferenceEnvironment(
      currentTypeCombSym = none,
      currentTypeLambdaIdx = 0,
      globalTypeVarKinds = Map(),
      localTypeVarKinds = Map(),
      localKindTables = Map(),
      kindParamForest = ParamForest.empty,
      globalInferringKindCount = 0,
      definedKindTerms = Nil,
      irreplaceableKindParams = IntMap(),
      currentKindTermPair = none)
}