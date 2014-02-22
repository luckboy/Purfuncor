/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
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
import pl.luckboy.purfuncor.frontend.TypeCombinatorNode

case class SymbolKindInferenceEnvironment[T](
    currentTypeCombSym: Option[GlobalSymbol],
    currentTypeLambdaIdx: Int,
    globalTypeVarKinds: Map[GlobalSymbol, Kind],
    localTypeVarKinds: Map[LocalSymbol, NonEmptyList[Kind]],
    typeParamKinds: Map[Int, Kind],
    localKindTables: Map[Option[GlobalSymbol], Map[Int, KindTable[LocalSymbol]]],
    kindParamForest: ParamForest[KindTerm[StarKindTerm[Int]]],
    typeCombNodes: Map[GlobalSymbol, TypeCombinatorNode[Symbol, lmbdindexer.TypeLambdaInfo[T], GlobalSymbol]],
    definedKindTerms: List[KindTerm[StarKindTerm[Int]]],
    irreplaceableKindParams: Map[Int, NonEmptyList[KindTerm[StarKindTerm[Int]]]],
    currentKindTermPair: Option[(KindTerm[StarKindTerm[Int]], KindTerm[StarKindTerm[Int]])],
    errNoKind: Option[NoKind],
    isRecursive: Boolean)
{
  def withCurrentTypeCombSym(sym: Option[GlobalSymbol]) = copy(currentTypeCombSym = sym)
  
  def withTypeCombSym(sym: Option[GlobalSymbol])(f: SymbolKindInferenceEnvironment[T] => (SymbolKindInferenceEnvironment[T], Kind)) = {
    val oldSym =  currentTypeCombSym
    val (env, kind) = f(withCurrentTypeCombSym(sym))
    (env.withCurrentTypeCombSym(oldSym), kind)
  }
  
  def withCurrentTypeLambdaIdx(lambdaIdx: Int) = copy(currentTypeLambdaIdx = lambdaIdx)
  
  def withTypeLambdaIdx(lambdaIdx: Int)(f: SymbolKindInferenceEnvironment[T] => (SymbolKindInferenceEnvironment[T], Kind)) = {
    val oldLambdaIdx = currentTypeLambdaIdx
    val (env, kind) = f(withCurrentTypeLambdaIdx(lambdaIdx))
    (env.withCurrentTypeLambdaIdx(oldLambdaIdx), kind)
  }
  
  def typeVarKind(sym: Symbol) =
    sym match {
      case globalSym @ GlobalSymbol(_) =>
        globalTypeVarKinds.getOrElse(globalSym, NoKind.fromError(FatalError("undefined global type variable", none, NoPosition)))
      case localSym @ LocalSymbol(_)   =>
        localTypeVarKinds.get(localSym).map { _.head }.getOrElse(NoKind.fromError(FatalError("undefined local type variable", none, NoPosition)))
    }
  
  def typeParamKind(param: Int) = typeParamKinds.getOrElse(param, NoKind.fromError(FatalError("undefined type parameter", none, NoPosition)))
  
  def withTypeParamKind(param: Int, kind: Kind) = copy(typeParamKinds = typeParamKinds + (param -> kind))
  
  def withTypeParamKinds(kinds: Map[Int, Kind]) = copy(typeParamKinds = typeParamKinds ++ kinds)
  
  def pushLocalTypeVarKinds(kinds: Map[LocalSymbol, Kind]) = copy(localTypeVarKinds = kinds.mapValues { NonEmptyList(_) } |+| localTypeVarKinds)

  def popLocalTypeVarKinds(syms: Set[LocalSymbol]) = copy(localTypeVarKinds = localTypeVarKinds.flatMap { case (s, ks) => if(syms.contains(s)) ks.tail.toNel.map { (s, _) } else some((s, ks)) })

  def currentLocalKindTable = localKindTables.getOrElse(currentTypeCombSym, Map()).getOrElse(currentTypeLambdaIdx, KindTable.empty[LocalSymbol])
  
  def withCurrentLocalKindTable(kindTable: KindTable[LocalSymbol]) = copy(localKindTables = localKindTables ++ Map(currentTypeCombSym -> (localKindTables.getOrElse(currentTypeCombSym, IntMap()) + (currentTypeLambdaIdx -> kindTable))))
  
  def withLocalKindTables(kindTables: Map[Option[GlobalSymbol], Map[Int, KindTable[LocalSymbol]]]) = copy(localKindTables = kindTables)
  
  def withLocalTypeVarKinds[U](kindTerms: Map[LocalSymbol, Option[KindTerm[StarKindTerm[U]]]])(f: SymbolKindInferenceEnvironment[T] => (SymbolKindInferenceEnvironment[T], Kind)) = {
    val (env, res) = kindTerms.foldLeft((this, Map[LocalSymbol, Kind]().success[NoKind])) {
      case ((newEnv, Success(newKinds)), (sym, kt)) =>
        val kindTerm = kt.map(intKindTermFromKindTerm).getOrElse(Star(KindParam(0), NoPosition))
        val (newEnv2, newRes) = allocateKindTermParamsS(kindTerm)(Map())(newEnv)
        newRes.map {
          p => (kt.map { _ => newEnv2.withDefinedKind(p._2) }.getOrElse(newEnv2), (newKinds + (sym -> InferringKind(p._2))).success)
        }.valueOr { nk => (newEnv2, nk.failure) }
      case ((newEnv, Failure(nk)), _)               =>
        (newEnv, nk.failure)
    }
    res.map {
      newKinds =>
        val (env2, kind) = f(env.pushLocalTypeVarKinds(newKinds).withCurrentLocalKindTable(KindTable(env.currentLocalKindTable.kinds ++ newKinds.filterKeys(kindTerms.keySet.contains))))
        (env2.popLocalTypeVarKinds(newKinds.keySet), kind)
    }.valueOr { (env, _) } 
  }
    
  def withKindParamForest(paramForest: ParamForest[KindTerm[StarKindTerm[Int]]]) = copy(kindParamForest = paramForest)
  
  def withTypeCombNodes(nodes: Map[GlobalSymbol, TypeCombinatorNode[Symbol, lmbdindexer.TypeLambdaInfo[T], GlobalSymbol]]) = copy(typeCombNodes = typeCombNodes)
  
  def withTypeComb(sym: GlobalSymbol, node: TypeCombinatorNode[Symbol, lmbdindexer.TypeLambdaInfo[T], GlobalSymbol]) = copy(typeCombNodes = typeCombNodes + (sym -> node))
  
  def withoutTypeCombs(syms: Set[GlobalSymbol]) = copy(typeCombNodes = typeCombNodes -- syms)
  
  def withDefinedKind(kindTerm: KindTerm[StarKindTerm[Int]]) =
    copy(
        definedKindTerms = definedKindTerms :+ kindTerm,
        irreplaceableKindParams = IntMap() ++ (irreplaceableKindParams |+| kindParamsFromKindTerm(kindTerm).map { (_, NonEmptyList(kindTerm)) }.toMap))
  
  def withCurrentKindTermPair(pair: Option[(KindTerm[StarKindTerm[Int]], KindTerm[StarKindTerm[Int]])]) = copy(currentKindTermPair = pair)
  
  def withKindTermPair[U](pair: Option[(KindTerm[StarKindTerm[Int]], KindTerm[StarKindTerm[Int]])])(f: SymbolKindInferenceEnvironment[T] => (SymbolKindInferenceEnvironment[T], U)) = {
    val oldKindTermPair = currentKindTermPair
    val (env, res) = f(withCurrentKindTermPair(pair))
    (env.withCurrentKindTermPair(oldKindTermPair), res)
  }
  
  def withErrs(noKind: NoKind) = copy(errNoKind = errNoKind.map { nk => some(nk |+| noKind) }.getOrElse(some(noKind)))

  def withRecursive(isRecursive: Boolean) = copy(isRecursive = isRecursive)
  
  def withGlobalTypeVarKind(sym: GlobalSymbol, kind: Kind) = copy(globalTypeVarKinds = globalTypeVarKinds + (sym -> kind))
  
  def withGlobalTypeVarKinds(kinds: Map[GlobalSymbol, Kind]) = copy(globalTypeVarKinds = globalTypeVarKinds ++ kinds)
  
  def withoutGlobalTypeVarKinds(syms: Set[GlobalSymbol]) = copy(globalTypeVarKinds = globalTypeVarKinds -- syms)
  
  def withClear[U](f: SymbolKindInferenceEnvironment[T] => (SymbolKindInferenceEnvironment[T], U)) =
    if(!isRecursive) {
      val (env, res) = f(copy(kindParamForest = ParamForest.empty, definedKindTerms = Nil, irreplaceableKindParams = Map()))
      (env.copy(kindParamForest = ParamForest.empty, definedKindTerms = Nil, irreplaceableKindParams = Map()), res)
    } else {
      f(this)
    }  
}

object SymbolKindInferenceEnvironment
{
  def empty[T] = fromInferredKindTable[T](InferredKindTable.empty)
  
  def fromInferredKindTable[T](kindTable: InferredKindTable[GlobalSymbol]) = SymbolKindInferenceEnvironment[T](
      currentTypeCombSym = none,
      currentTypeLambdaIdx = 0,
      globalTypeVarKinds = kindTable.kinds,
      localTypeVarKinds = Map(),
      typeParamKinds = IntMap(),
      localKindTables = Map(),
      kindParamForest = ParamForest.empty,
      typeCombNodes = Map(),
      definedKindTerms = Nil,
      irreplaceableKindParams = IntMap(),
      currentKindTermPair = none,
      errNoKind =  none,
      isRecursive = false)
}
