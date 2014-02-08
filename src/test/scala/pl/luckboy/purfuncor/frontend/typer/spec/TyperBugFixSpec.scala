/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer.spec
import scalaz._
import scalaz.Scalaz._
import org.scalatest.FlatSpec
import org.scalatest.Inside
import org.scalatest.matchers.ShouldMatchers
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.typer._
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
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.common.Arrow
import pl.luckboy.purfuncor.frontend.typer.TypeBuiltinFunction

class TyperBugFixSpec extends FlatSpec with ShouldMatchers with Inside with TyperSpecUtils
{
  def typer[T, U, V, W, X, Y, Z, TT, TU, TV, E, TC, TE, D](emptyEnv: E, emptyTypeEnv: TE, initData: D)(makeData: String => ValidationNel[AbstractError, D])(kindTableFromData: D => InferredKindTable[TT])(withKindTable: (D, InferredKindTable[TT]) => D)(f2: D => Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], Z]])(g3: (InferredKindTable[TT], InferredTypeTable[T, TT]) => State[TE, E])(h2: D => Term[SimpleTerm[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]]] => ValidationNel[AbstractError, Term[SimpleTerm[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]]]])(implicit init: Initializer[NoType[TT], T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], E], inferrer: Inferrer[SimpleTerm[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], E, Type[TT]], typeInit: Initializer[NoTypeValue[TT, W, TypeLambdaInfo[X, Y], TC], TT, AbstractTypeCombinator[W, TypeLambdaInfo[X, Y]], TE], envSt: TypeInferenceEnvironmentState[E, T, TT], enval: TypeInferenceEnvironmental[E, T, TU, TT], treeInfoExtractor: TreeInfoExtractor[Z, Tree[TT, AbstractTypeCombinator[W, TypeLambdaInfo[X, Y]], TypeTreeInfo[TV, TT]]], globalSymTabular: GlobalSymbolTabular[Z, T], typeGlobalSymTabular: GlobalSymbolTabular[TV, TT], localSymTabular: LocalSymbolTabular[V, TU])
  {
    val f4 = {
      (data: D) => (tree: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]], kindTable: InferredKindTable[TT]) =>
        State((typeEnv: TE) => (typeEnv, f2(withKindTable(data, kindTable))(tree)))
    }
    val f3 = f4(initData)
    val f = f2(initData)
    val h = h2(initData)
    
    it should "infer the type with the recursive type conjunction" in {
     val s = """
unittype 1 T
type U t1 = #Any
type V t1 = (T t1) #& (V t1)
type W t1 = tuple 1 t1
type X t1 = (V t1) #& (W t1)
f x = construct 1 x: X
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              res2 should be ===(().success.success)
              inside(typeGlobalSymTabular.getGlobalLocationFromTable(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo).treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("X")))) {
                case Some(xLoc) =>
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("f")))) {
                    case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), argKinds) =>
                      // \t1 t2 => t1 #-> X t2
                      inside(argType1) {
                        case TypeParamApp(param1, Seq(), 0) =>
                          inside(retType1) {
                            case GlobalTypeApp(loc1, Seq(arg11), GlobalSymbol(NonEmptyList("X"))) =>
                              loc1 should be ===(xLoc)
                              inside(arg11) {
                                case TypeValueLambda(Seq(), TypeParamApp(param11, Seq(), 0)) =>
                                  List(param1, param11).toSet should have size(2)
                              }
                          }
                      }
                      inside(argKinds) {
                        case Seq(
                            InferredKind(Star(KindType, _)) /* * */,
                            InferredKind(Star(KindType, _)) /* * */) =>
                          ()
                      }
                  }
              }
          }
      }
    }
    
    it should "infer the type with the type function of the identity" in {
      val s = """
type TID x = x
unittype 1 T
f x = construct 1 x: \t => TID (T t) #& (tuple 1 t)
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              res2 should be ===(().success.success)
              val syms = List(
                  GlobalSymbol(NonEmptyList("TID")),
                  GlobalSymbol(NonEmptyList("T")))
              inside(syms.flatMap(typeGlobalSymTabular.getGlobalLocationFromTable(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo).treeInfo.treeInfo))) {
                case List(tidLoc, tLoc) =>
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("f")))) {
                    case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), argKinds) =>
                      // \t1 => t1 #-> (TID (T t1) #& tuple 1 t1)
                      inside(argType1) {
                        case TypeParamApp(param1, Seq(), 0) =>
                          inside(retType1) {
                            case TypeConjunction(types11) =>
                              inside(for {
                                x1 <- types11.collectFirst { case GlobalTypeApp(loc111, Seq(arg111), GlobalSymbol(NonEmptyList("TID"))) => (loc111, arg111) }
                                x2 <- types11.collectFirst { case TupleType(Seq(type112)) => type112 }
                              } yield (x1, x2)) {
                                case Some(((loc111, arg111), type112)) =>
                                  loc111 should be ===(tidLoc)
                                  inside(arg111) {
                                    case TypeValueLambda(Seq(), GlobalTypeApp(loc1111, Seq(arg1111), GlobalSymbol(NonEmptyList("T")))) =>
                                      loc1111 should be ===(tLoc)
                                      inside(arg1111) {
                                        case TypeValueLambda(Seq(), TypeParamApp(param1111, Seq(), 0)) =>
                                          inside(type112) {
                                            case TypeParamApp(param112, Seq(), 0) =>
                                              List(param1111, param112).toSet should have size(1)
                                          }
                                      }
                                  }
                              }
                          }
                      }
                      inside(argKinds) {
                        case Seq(
                            InferredKind(Star(KindType, _)) /* * */) =>
                          ()
                      }
                  }
              }
          }
      }
    }

    it should "infer the global type with the zero tuple tuple and the unit types" in {
      val s = """
unittype 0 T
unittype 0 U
type V = #Any
type W = T #& U #& V
type X = ()
type Y = W #& X
f = construct 0: Y
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              res2 should be ===(().success.success)
              inside(typeGlobalSymTabular.getGlobalLocationFromTable(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo).treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("Y")))) {
                case Some(yLoc) =>
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("f")))) {
                    case InferredType(GlobalTypeApp(loc1, Seq(), GlobalSymbol(NonEmptyList("Y"))), Seq()) =>
                      // Y
                      loc1 should be ===(yLoc)
                  }
              }
          }
      }
    }
    
    it should "infer the recursive data types" in {
      val s = """
datatype List t = Nil | t :: (List t)
datatype Tree t = Leaf | Node (Tree t) t (Tree t)
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              res2 should be ===(().success.success)
          }
      }
    }
  }
  
  "A Typer" should behave like typer(SymbolTypeInferenceEnvironment.empty[parser.LambdaInfo, parser.TypeLambdaInfo], SymbolTypeEnvironment.empty[TypeLambdaInfo[parser.TypeLambdaInfo, LocalSymbol]], InferredKindTable.empty[GlobalSymbol])(makeInferredKindTable)(identity)((kt1, kt2) => InferredKindTable(kt1.kinds ++ kt2.kinds))(Typer.transformToSymbolTree2)(Typer.statefullyMakeSymbolTypeInferenceEnvironment3)(Typer.transformToSymbolTerm2)
}
