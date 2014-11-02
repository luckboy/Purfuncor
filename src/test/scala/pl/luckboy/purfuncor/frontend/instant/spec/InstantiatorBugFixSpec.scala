/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.instant.spec
import scalaz._
import scalaz.Scalaz._
import org.scalatest.FlatSpec
import org.scalatest.Inside
import org.scalatest.matchers.ShouldMatchers
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.instant._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol
import pl.luckboy.purfuncor.frontend.resolver.NameTree
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbolTabular
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbolTabular
import pl.luckboy.purfuncor.frontend.kinder.InferredKind
import pl.luckboy.purfuncor.frontend.kinder.InferredKindTable
import pl.luckboy.purfuncor.frontend.kinder.TypeLambdaInfo
import pl.luckboy.purfuncor.frontend.kinder.TypeTreeInfo
import pl.luckboy.purfuncor.frontend.typer.Type
import pl.luckboy.purfuncor.frontend.typer.InferredType
import pl.luckboy.purfuncor.frontend.typer.InferredTypeTable
import pl.luckboy.purfuncor.frontend.typer.BuiltinType
import pl.luckboy.purfuncor.frontend.typer.TupleType
import pl.luckboy.purfuncor.frontend.typer.GlobalTypeApp
import pl.luckboy.purfuncor.frontend.typer.TypeParamApp
import pl.luckboy.purfuncor.frontend.typer.TypeConjunction
import pl.luckboy.purfuncor.frontend.typer.TypeDisjunction
import pl.luckboy.purfuncor.frontend.typer.TypeValueLambda
import pl.luckboy.purfuncor.frontend.typer.TypeBuiltinFunction
import pl.luckboy.purfuncor.frontend.typer.TreeInfo
import pl.luckboy.purfuncor.frontend.typer.SymbolTypeEnvironment
import pl.luckboy.purfuncor.frontend
import pl.luckboy.purfuncor.common.Tree

class InstantiatorBugFixSpec  extends FlatSpec with ShouldMatchers with Inside
{
  def instantiator[T, U, V, W, X, Y, Z, TT, TU, TV, E, TE, D](emptyEnv: E, emptyTypeEnv: TE, initData: D)(makeData: String => ValidationNel[AbstractError, D])(f4: D => (Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]], InferredKindTable[X], InferredTypeTable[T, X]) => State[TE, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, typer.LambdaInfo[V, W, X], TypeSimpleTerm[Y, TypeLambdaInfo[Z, TT]]], typer.TreeInfo[TU, T, X]]]])(g3: (InferredKindTable[X], InferredTypeTable[T, X], InstanceTree[AbstractPolyFunction[T], X, GlobalInstance[T]], InstanceArgTable[T, X]) => State[TE, E])(h3: D => (InferredKindTable[X], InferredTypeTable[T, X], TE) => Term[SimpleTerm[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]]] => ValidationNel[AbstractError, (Term[SimpleTerm[U, typer.LambdaInfo[V, W, X], TypeSimpleTerm[Y, TypeLambdaInfo[Z, TT]]]], Type[X])])(implicit init: Initializer[NonEmptyList[AbstractError], T, AbstractCombinator[U, typer.LambdaInfo[V, W, X], TypeSimpleTerm[Y, TypeLambdaInfo[Z, TT]]], E], polyFunInstantiator: PolyFunInstantiator[T, Y, X, TypeLambdaInfo[Z, TT], E], enval: InstantiationEnvironmental[E, T, X], treeInfoExtractor: TreeInfoExtractor[TU, Tree[X, AbstractTypeCombinator[Y, TypeLambdaInfo[Z, TT]], TypeTreeInfo[TV, X]]], instTreeInfoExtractor: InstantiationTreeInfoExtractor[TU, T, frontend.Instance[T], SelectConstructInstance[Y, TypeLambdaInfo[Z, TT]]], globalSymTabular: GlobalSymbolTabular[TU, T], typeGlobalSymTabular: GlobalSymbolTabular[TV, X], localSymTabular: LocalSymbolTabular[V, W], typeLocalSymTabular: LocalSymbolTabular[Z, TT], locational: Locational[U, T, W])
  {
    val emptyInstTree = InstanceTree.empty[AbstractPolyFunction[T], X, GlobalInstance[T]]
    
    def f3 = f4(initData)
    def h = h3(initData)(InferredKindTable.empty, InferredTypeTable.empty, emptyTypeEnv)
    
    it should "complain on a non-existent instance with unused type parameter" in {
      val s = """
poly f
instance f => g
g = ((\x => x), (\y => y))
h = #2 1 f
i = #2 2 f
"""
      val (typeEnv, res) = Instantiator.transformString(s)(NameTree.empty, InferredKindTable.empty, InferredTypeTable.empty, emptyInstTree, InstanceArgTable.empty)(f3)(g3).run(emptyTypeEnv)
      inside(res) {
        case Success(Tree(_, treeInfo)) =>
          val nameTree = NameTree.fromGlobalSymbols(Set(
              GlobalSymbol(NonEmptyList("f")),
              GlobalSymbol(NonEmptyList("g")),
              GlobalSymbol(NonEmptyList("h")),
              GlobalSymbol(NonEmptyList("i"))))
          val typeTree = treeInfoExtractor.typeTreeFromTreeInfo(treeInfo.treeInfo)
          val typeTreeInfo = typeTree.treeInfo
          val (typeEnv2, env) = g3(typeTreeInfo.kindTable, treeInfo.typeTable, treeInfo.instTree, treeInfo.instArgTable).run(typeEnv)
          inside(makeData(s)) {
            case Success(data) =>
              val res2 = Instantiator.transformTermStringWithInstantiation("h")(nameTree, env)(h3(data)(typeTreeInfo.kindTable, treeInfo.typeTable, typeEnv2))
              inside(res2) {
                case Failure(errs) =>
                  errs.map { _.msg } should be ===(NonEmptyList("couldn't find instance for #.f with type \\(t1: *) (t2: *) => (t1, t2)"))
              }
          }
      }
    }
    
    it should "transform the instances for the bug of the incorrect construct type" in {
      val s = "datatype T t u = C1 t | C2 t u"
      val (typeEnv, res) = Instantiator.transformString(s)(NameTree.empty, InferredKindTable.empty, InferredTypeTable.empty, emptyInstTree, InstanceArgTable.empty)(f3)(g3).run(emptyTypeEnv)
      res should be ===(().success.success)
    }
  }
  
  "An Instantiator" should behave like instantiator(SymbolInstantiationEnvironment.empty[parser.LambdaInfo, parser.TypeLambdaInfo], SymbolTypeEnvironment.empty[TypeLambdaInfo[parser.TypeLambdaInfo, LocalSymbol]], ())(_ => ().successNel)(_ => Instantiator.statefullyTransformToSymbolTree3)(Instantiator.statefullyMakeSymbolInstantiationEnvironment3)(_ => Instantiator.transformToSymbolTerm2)
}
