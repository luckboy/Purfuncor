/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.instant
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol
import pl.luckboy.purfuncor.frontend.kinder.TypeLambdaInfo
import pl.luckboy.purfuncor.frontend.typer.SymbolTypeInferenceEnvironment
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.common.RecursiveInitializer._

case class SymbolInstantiationEnvironment[T, U](
    typeInferenceEnv: SymbolTypeInferenceEnvironment[T, U],
    currentCombSym: Option[GlobalSymbol],
    globalInstTree: InstanceTree[AbstractPolyFunction[GlobalSymbol], GlobalSymbol, GlobalInstance[GlobalSymbol]],
    firstGlobalInstCounts: Map[AbstractPolyFunction[GlobalSymbol], Int],
    instArgs: Map[GlobalSymbol, Seq[InstanceArg[GlobalSymbol, GlobalSymbol]]],
    lambdaInfos: Map[Option[GlobalSymbol], Map[Int, InstantiationLambdaInfo[GlobalSymbol]]],
    combNodes: Map[GlobalSymbol, CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]],
    recursiveCombSyms: Set[GlobalSymbol],
    uninitializedCombSyms: Set[GlobalSymbol],
    polyCombSyms: Set[GlobalSymbol],
    errs: List[AbstractError],
    isRecursive: Boolean)
{
  def withTypeInferenceEnv(env: SymbolTypeInferenceEnvironment[T, U]) = copy(typeInferenceEnv = env)
  
  def withCurrentCombSym(sym: Option[GlobalSymbol]) = copy(currentCombSym = sym)
  
  def withGlobalInstTree(instTree: InstanceTree[AbstractPolyFunction[GlobalSymbol], GlobalSymbol, GlobalInstance[GlobalSymbol]]) = copy(globalInstTree = instTree)
  
  def withInstArgs(instArgs: Map[GlobalSymbol, Seq[InstanceArg[GlobalSymbol, GlobalSymbol]]]) = copy(instArgs = instArgs)
  
  def currentLambdaInfos = lambdaInfos.getOrElse(currentCombSym, Map())
  
  def withLambdaInfos(lambdaInfos: Map[Option[GlobalSymbol], Map[Int, InstantiationLambdaInfo[GlobalSymbol]]]) = copy(lambdaInfos = lambdaInfos)
  
  def withCombNodes(nodes: Map[GlobalSymbol, CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]]) = copy(combNodes = nodes)
  
  def withComb(sym: GlobalSymbol, node: CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]) = copy(combNodes = combNodes + (sym -> node))
  
  def withoutCombs(syms: Set[GlobalSymbol]) = copy(combNodes = combNodes -- syms)
  
  def withRecursiveCombSyms(syms: Set[GlobalSymbol]) = copy(recursiveCombSyms = syms)
  
  def withUninitializedCombSyms(syms: Set[GlobalSymbol]) = copy(uninitializedCombSyms = syms)

  def withPolyCombSyms(syms: Set[GlobalSymbol]) = copy(polyCombSyms = syms)
  
  def withPolyCombs(syms: Set[GlobalSymbol]) = copy(polyCombSyms = polyCombSyms | syms)
  
  def withErrs(errs: NonEmptyList[AbstractError]) = copy(errs = this.errs ++ errs.toList)
  
  def withRecursive(isRecursive: Boolean) = copy(isRecursive = isRecursive)
  
  def hasPolyComb(sym: GlobalSymbol) =
    instArgs.get(sym) match {
      case Some(Seq(InstanceArg(PolyFunction(polyFunSym), _))) => polyFunSym === sym
      case None                                                => polyCombSyms.contains(sym)
      case _                                                   => false
    }
}

object SymbolInstantiationEnvironment
{
  def empty[T, U] = fromInstanceTree[T, U](InstanceTree.empty)
  
  def fromInstanceTree[T, U](instTree: InstanceTree[AbstractPolyFunction[GlobalSymbol], GlobalSymbol, GlobalInstance[GlobalSymbol]]) = SymbolInstantiationEnvironment[T, U](
      typeInferenceEnv = SymbolTypeInferenceEnvironment.empty,
      currentCombSym = none,
      globalInstTree = instTree,
      firstGlobalInstCounts = instTree.instTables.mapValues { _.instCount },
      instArgs = Map(),
      lambdaInfos = Map(),
      combNodes = Map(),
      recursiveCombSyms = Set(),
      uninitializedCombSyms = Set(),
      polyCombSyms = Set(),
      errs = Nil,
      isRecursive = false) 
}
