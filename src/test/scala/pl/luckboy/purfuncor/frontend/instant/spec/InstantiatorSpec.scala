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

class InstantiatorSpec extends FlatSpec with ShouldMatchers with Inside
{
  def instantiator[T, U, V, W, X, Y, Z, TT, TU, TV, E, TE, D](emptyEnv: E, emptyTypeEnv: TE, initData: D)(makeData: String => ValidationNel[AbstractError, D])(f4: D => (Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]], InferredKindTable[X], InferredTypeTable[T, X]) => State[TE, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, typer.LambdaInfo[V, W, X], TypeSimpleTerm[Y, TypeLambdaInfo[Z, TT]]], typer.TreeInfo[TU, T, X]]]])(g3: (InferredKindTable[X], InferredTypeTable[T, X], InstanceTree[AbstractPolyFunction[T], X, GlobalInstance[T]], InstanceArgTable[T, X]) => State[TE, E])(h3: D => (InferredKindTable[X], InferredTypeTable[T, X], TE) => Term[SimpleTerm[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]]] => ValidationNel[AbstractError, (Term[SimpleTerm[U, typer.LambdaInfo[V, W, X], TypeSimpleTerm[Y, TypeLambdaInfo[Z, TT]]]], Type[X])])(implicit init: Initializer[NonEmptyList[AbstractError], T, AbstractCombinator[U, typer.LambdaInfo[V, W, X], TypeSimpleTerm[Y, TypeLambdaInfo[Z, TT]]], E], polyFunInstantiator: PolyFunInstantiator[T, Y, X, TypeLambdaInfo[Z, TT], E], enval: InstantiationEnvironmental[E, T, X], treeInfoExtractor: TreeInfoExtractor[TU, Tree[X, AbstractTypeCombinator[Y, TypeLambdaInfo[Z, TT]], TypeTreeInfo[TV, X]]], instTreeInfoExtractor: InstantiationTreeInfoExtractor[TU, T, frontend.Instance[T], SelectConstructInstance[Y, TypeLambdaInfo[Z, TT]]], globalSymTabular: GlobalSymbolTabular[TU, T], typeGlobalSymTabular: GlobalSymbolTabular[TV, X], localSymTabular: LocalSymbolTabular[V, W], typeLocalSymTabular: LocalSymbolTabular[Z, TT], locational: Locational[U, T, W])
  {
    val emptyInstTree = InstanceTree.empty[AbstractPolyFunction[T], X, GlobalInstance[T]]
    
    def f3 = f4(initData)
    def h = h3(initData)(InferredKindTable.empty, InferredTypeTable.empty, emptyTypeEnv)
    
    it should "transform the string" in {
      val (typeEnv, res) = Instantiator.transformString("""
poly f
poly g
h x = tuple 3 f g x
""")(NameTree.empty, InferredKindTable.empty, InferredTypeTable.empty, emptyInstTree, InstanceArgTable.empty)(f3)(g3).run(emptyTypeEnv)
      inside(res) {
        case Success(Tree(combs, treeInfo)) =>
          val combSyms = Set(
              GlobalSymbol(NonEmptyList("f")),
              GlobalSymbol(NonEmptyList("g")),
              GlobalSymbol(NonEmptyList("h")))
          val insts = instTreeInfoExtractor.instancesFromTreeInfo(treeInfo.treeInfo)
          val selectConstructInsts = instTreeInfoExtractor.selectConstructInstancesFromTreeInfo(treeInfo.treeInfo)
          val combLocs = combSyms.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo))
          combLocs should have size(3)
          treeInfo.typeTable.types.keySet should be ===(combLocs)
          treeInfo.instArgTable.instArgs.keySet should be ===(combLocs)
          // f
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))).flatMap(combs.get)) {
            case Some(PolyCombinator(None, _)) => ()
          }
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))).flatMap(treeInfo.typeTable.types.get)) {
            case Some(InferredType(TypeParamApp(_, Seq(), 0), argKinds)) =>
              // \t1 => t1
              inside(argKinds) {
                case Seq(
                    InferredKind(Star(KindType, _)) /* * */) =>
                  ()
              }
          }
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))).flatMap(treeInfo.instArgTable.instArgs.get)) {
            case Some(Seq(instArg1)) =>
              inside(instArg1) {
                case InstanceArg(polyFun1, type1) =>
                  some(polyFun1) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))).map { PolyFunction(_) })
                  inside(type1) {
                    case InferredType(TypeParamApp(_, Seq(), 0), argKinds1) =>
                      // \t1 => t1
                      inside(argKinds1) {
                        case Seq(
                            InferredKind(Star(KindType, _)) /* * */) =>
                          ()
                      }
                  }
              }
          }
          // g
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("g"))).flatMap(combs.get)) {
            case Some(PolyCombinator(None, _)) => ()
          }
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("g"))).flatMap(treeInfo.typeTable.types.get)) {
            case Some(InferredType(TypeParamApp(_, Seq(), 0), argKinds)) =>
              // \t1 => t1
              inside(argKinds) {
                case Seq(
                    InferredKind(Star(KindType, _)) /* * */) =>
                  ()
              }
          }
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("g"))).flatMap(treeInfo.instArgTable.instArgs.get)) {
            case Some(Seq(instArg1)) =>
              inside(instArg1) {
                case InstanceArg(polyFun1, type1) =>
                  some(polyFun1) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("g"))).map { PolyFunction(_) })
                  inside(type1) {
                    case InferredType(TypeParamApp(_, Seq(), 0), argKinds1) =>
                      // \t1 => t1
                      inside(argKinds1) {
                        case Seq(
                            InferredKind(Star(KindType, _)) /* * */) =>
                          ()
                      }
                  }
              }
          }
          // h
          def testInstArgs(localInstIdx1: Int, localInstIdx2: Int)
          {
            inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("h"))).flatMap(treeInfo.instArgTable.instArgs.get)) {
              case Some(instArgs) =>
                instArgs should have size(2)
                inside(instArgs.lift(localInstIdx1)) {
                  case Some(InstanceArg(polyFun1, type1)) =>
                    some(polyFun1) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))).map { PolyFunction(_) })
                    inside(type1) {
                      case InferredType(TypeParamApp(param11, Seq(), 0), argKinds1) =>
                        inside(instArgs.lift(localInstIdx2)) {
                          case Some(InstanceArg(polyFun2, type2)) =>
                            some(polyFun2) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("g"))).map { PolyFunction(_) })
                            inside(type2) {
                              case InferredType(TypeParamApp(param21, Seq(), 0), argKinds2) =>
                                List(param11, param21).toSet should have size(2)
                                val otherParams = List(0, 1, 2).filter { !List(param11, param21).contains(_) }
                                argKinds1 should have size(3)
                                inside(argKinds1.lift(param11)) { case Some(InferredKind(Star(KindType, _))) => () }
                                inside(argKinds1.lift(param21)) { case Some(InferredKind(Star(KindType, _))) => () }
                                inside(otherParams.flatMap { argKinds1.lift(_) }.toList) { case List(InferredKind(Star(KindParam(_), _))) => () }
                                argKinds2 should have size(3)
                                inside(argKinds2.lift(param11)) { case Some(InferredKind(Star(KindType, _))) => () }
                                inside(argKinds2.lift(param21)) { case Some(InferredKind(Star(KindType, _))) => () }
                                inside(otherParams.flatMap { argKinds2.lift(_) }.toList) { case List(InferredKind(Star(KindParam(_), _))) => () }
                            }
                        }
                    }
                }
            }
          }
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("h"))).flatMap(combs.get)) {
            case Some(Combinator(None, args, body, LambdaInfo(lambdaInfo, 0, typeTable, Seq()), _)) =>
              inside(args) { case List(Arg(Some("x"), None, _)) => () }
              val syms = Set(LocalSymbol("x"))
              val locs = syms.flatMap(localSymTabular.getLocalLocationFromTable(lambdaInfo))
              locs should have size(1)
              typeTable.types.keySet should be ===(locs)
              inside(body) {
                case App(fun1, args1, _) =>
                  inside(fun1) { case Simple(Literal(TupleFunValue(3)), _) => () }
                  inside(args1) {
                    case NonEmptyList(arg11, arg12, arg13) =>
                      inside(arg11) {
                        case Simple(Var(loc11, LambdaInfo(lambdaInfo11, 1, typeTable11, insts11)), _) =>
                          some(loc11) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))))
                          typeTable11.types should be ('empty)
                          inside(insts11) { 
                            case Seq(LocalInstance(localInstIdx11)) =>
                              inside(arg12) {
                                case Simple(Var(loc12, LambdaInfo(lambdaInfo12, 2, typeTable12, insts12)), _) =>
                                  some(loc12) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("g"))))
                                  typeTable12.types should be ('empty)
                                  inside(insts12) { 
                                    case Seq(LocalInstance(localInstIdx12)) =>
                                      List(localInstIdx11, localInstIdx12).toSet should have size(2)
                                      testInstArgs(localInstIdx11, localInstIdx12)
                                  }
                              }
                          }
                      }
                      inside(arg13) {
                        case Simple(Var(loc13, LambdaInfo(lambdaInfo13, 3, typeTable13, Seq())), _) =>
                          some(loc13) should be ===(localSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("x")))
                          typeTable13.types should be ('empty)
                      }
                  }
              }
              inside(localSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("x")).flatMap(typeTable.types.get)) {
                case Some(InferredType(TypeParamApp(_, Seq(), 0), argKinds)) =>
                  // \t1 => t1
                  inside(argKinds) {
                    case Seq(
                        InferredKind(Star(KindType, _)) /* * */) =>
                      ()
                  }
              }
          }
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("h"))).flatMap(treeInfo.typeTable.types.get)) {
            case Some(InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(arg1, ret1)), argKinds)) =>
              // \t1 t2 => t1 #-> (t2, t3, t1)
              inside(arg1) {
                case TypeParamApp(param1, Seq(), 0) =>
                  inside(ret1) {
                    case TupleType(Seq(TypeParamApp(param2, Seq(), 0), TypeParamApp(param3, Seq(), 0), TypeParamApp(param4, Seq(), 0))) =>
                      List(param1, param4).toSet should have size(1)
                      List(param1, param2, param3, param4).toSet should have size(3)
                  }
              }
              inside(argKinds) {
                case Seq(
                    InferredKind(Star(KindType, _)) /* * */,
                    InferredKind(Star(KindType, _)) /* * */,
                    InferredKind(Star(KindType, _)) /* * */) =>
                  ()
              }
          }
          // instances
          insts should be ('empty)
          selectConstructInsts should be ('empty)
          treeInfo.instTree should be ===(InstanceTree.empty)
      }
    }
    
    it should "transform the instantiation fields for the construct-expression" in {
      val (typeEnv, res) = Instantiator.transformString("""
f x y = construct 2 x y
""")(NameTree.empty, InferredKindTable.empty, InferredTypeTable.empty, emptyInstTree, InstanceArgTable.empty)(f3)(g3).run(emptyTypeEnv)
      inside(res) {
        case Success(Tree(combs, treeInfo)) =>
          val combSyms = Set(GlobalSymbol(NonEmptyList("f")))
          val combLocs = combSyms.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo))
          combLocs should have size(1)
          treeInfo.instArgTable.instArgs.keySet should be ===(combLocs)
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))).flatMap(combs.get)) {
            case Some(Combinator(None, args, body, LambdaInfo(lambdaInfo, 0, typeTable, Seq()), _)) =>
              inside(body) {
                case App(Simple(Construct(2, LambdaInfo(_, 1, _, insts1)), _), _, _) =>
                  inside(insts1) { case Seq(LocalInstance(0)) => () }
              }
          }
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))).flatMap(treeInfo.instArgTable.instArgs.get)) {
            case Some(Seq(instArg1)) =>
              inside(instArg1) {
                case InstanceArg(ConstructFunction, type1) =>
                  inside(type1) {
                    case InferredType(TypeConjunction(types11), argKinds1) =>
                      // \t1 t2 t3 => t1 #& (t2, t3)
                      inside(for {
                        x1 <- types11.collectFirst { case TypeParamApp(param111, Seq(), 0) => param111 }
                        x2 <- types11.collectFirst { case TupleType(Seq(TypeParamApp(param112, Seq(), 0), TypeParamApp(param113, Seq(), 0))) => (param112, param113) }
                      } yield (x1, x2)) {
                        case Some((param111, (param112, param113))) =>
                          List(param111, param112, param113).toSet should have size(3)
                      }
                      inside(argKinds1) {
                        case Seq(
                            InferredKind(Star(KindType, _)) /* * */,
                            InferredKind(Star(KindType, _)) /* * */,
                            InferredKind(Star(KindType, _)) /* * */) =>
                          ()
                      }
                  }
              }
          }
      }
    }
    
    it should "transform the instantiation fields for the select-expression" in {
      val (typeEnv, res) = Instantiator.transformString("""
unittype 2 T
unittype 0 U
f x = x select {
    (y: \t1 t2 => T t1 t2) => 1
    (y: U)                 => 2
  }
""")(NameTree.empty, InferredKindTable.empty, InferredTypeTable.empty, emptyInstTree, InstanceArgTable.empty)(f3)(g3).run(emptyTypeEnv)
      inside(res) {
        case Success(Tree(combs, treeInfo)) =>
          val typeTree = treeInfoExtractor.typeTreeFromTreeInfo(treeInfo.treeInfo)
          val typeCombs = typeTree.combs
          val typeTreeInfo = typeTree.treeInfo
          val combSyms = Set(GlobalSymbol(NonEmptyList("f")))
          val combLocs = combSyms.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo))
          combLocs should have size(1)
          treeInfo.instArgTable.instArgs.keySet should be ===(combLocs)
          val typeCombSyms = List(GlobalSymbol(NonEmptyList("T")), GlobalSymbol(NonEmptyList("U")))
          inside(typeCombSyms.flatMap(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo))) {
            case List(tLoc, uLoc) =>
              def testInstArgs(localInstIdx1: Int, localInstIdx2: Int, localInstIdx3: Int)
              {
                inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))).flatMap(treeInfo.instArgTable.instArgs.get)) {
                  case Some(instArgs) =>
                    instArgs should have size(3)
                    inside(instArgs.lift(localInstIdx1)) {
                      case Some(InstanceArg(SelectFunction, type1)) =>
                        inside(type1) {
                          case InferredType(TypeDisjunction(types11), argKinds1) =>
                            // \t1 t2 => (T t1 t2) #| U
                            inside(for {
                              x1 <- types11.collectFirst { case GlobalTypeApp(loc111, Seq(arg111, arg112), GlobalSymbol(NonEmptyList("T"))) => (loc111, arg111, arg112) }
                              x2 <- types11.collectFirst { case GlobalTypeApp(loc112, Seq(), GlobalSymbol(NonEmptyList("U"))) => loc112 }
                            } yield (x1, x2)) {
                              case Some(((loc111, arg111, arg112), loc112)) =>
                                loc111 should be ===(tLoc)
                                loc112 should be ===(uLoc)
                                inside(arg111) {
                                  case TypeValueLambda(Seq(), TypeParamApp(param111, Seq(), 0)) =>
                                    inside(arg112) {
                                      case TypeValueLambda(Seq(), TypeParamApp(param112, Seq(), 0)) =>
                                        inside(instArgs.lift(localInstIdx2)) {
                                          case Some(InstanceArg(ConstructFunction, type2)) =>
                                            inside(type2) {
                                              case InferredType(GlobalTypeApp(loc21, args21, GlobalSymbol(NonEmptyList("T"))), argKinds2) =>
                                                // \t1 t2 => T t1 t2
                                                loc111 should be ===(tLoc)
                                                inside(args21) {
                                                  case Seq(arg211, arg212) =>
                                                    inside(arg211) {
                                                      case TypeValueLambda(Seq(), TypeParamApp(param211, Seq(), 0)) =>
                                                        inside(arg212) {
                                                          case TypeValueLambda(Seq(), TypeParamApp(param212, Seq(), 0)) =>
                                                            List(param111, param211).toSet should have size(1)
                                                            List(param112, param212).toSet should have size(1)
                                                            List(param111, param112).toSet should have size(2)
                                                        }
                                                    }
                                                }
                                                inside(argKinds2) {
                                                  case Seq(
                                                      InferredKind(Star(KindParam(_), _)) /* k1 */,
                                                      InferredKind(Star(KindParam(_), _)) /* k1 */) =>
                                                    ()
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            inside(argKinds1) {
                              case Seq(
                                  InferredKind(Star(KindParam(_), _)) /* k1 */,
                                  InferredKind(Star(KindParam(_), _)) /* k1 */) =>
                               ()
                            }
                        }
                    }
                    inside(instArgs.lift(localInstIdx3)) {
                      case Some(InstanceArg(ConstructFunction, type3)) =>
                        inside(type3) {
                          case InferredType(GlobalTypeApp(loc31, Seq(), GlobalSymbol(NonEmptyList("U"))), argKinds3) =>
                            loc31 should be ===(uLoc)
                            inside(argKinds3) {
                              case Seq(
                                  InferredKind(Star(KindParam(_), _)) /* k1 */,
                                  InferredKind(Star(KindParam(_), _)) /* k1 */) =>
                                ()
                            }
                        }
                    }
                 }
              }
              inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))).flatMap(combs.get)) {
                case Some(Combinator(None, args, body, LambdaInfo(lambdaInfo, 0, typeTable, Seq()), _)) =>
                  inside(args) { case List(Arg(Some("x"), None, _)) => () }
                  val syms = Set(LocalSymbol("x"))
                  val locs = syms.flatMap(localSymTabular.getLocalLocationFromTable(lambdaInfo))
                  locs should have size(1)
                  typeTable.types.keySet should be ===(locs)
                  inside(body) {
                    case Simple(Select(_, cases1, LambdaInfo(lambdaInfo1, 1, typeTable1, insts1)), _) =>
                      inside(insts1) {
                        case Seq(LocalInstance(localInstIdx11)) =>
                          inside(cases1) {
                            case NonEmptyList(case11, case12) =>
                              inside(case11) {
                                case Case(_, _, _, LambdaInfo(lambdaInfo11, 3, _, insts11), _) =>
                                  inside(insts11) { 
                                    case Seq(LocalInstance(localInstIdx111)) =>
                                      inside(case12) {
                                        case Case(_, _, _, LambdaInfo(lambdaInfo12, 4, _, insts12), _) =>
                                          inside(insts12) { 
                                            case Seq(LocalInstance(localInstIdx121)) =>
                                              testInstArgs(localInstIdx11, localInstIdx111, localInstIdx121)
                                          }
                                      }
                                  }
                              }
                          }
                      }
                  }
              }
          }
      }
    }
    
    it should "transform the instances to the instance tree" in {
      val (typeEnv, res) = Instantiator.transformString("""
f = tuple 2 true 'a'
g = tuple 2 true 0.1
h = 'c'
instance i => f
instance i => g
instance j => h
poly i
poly j
unittype 2 T
unittype 0 U
instance select \t1 t2 => ##| (##& (T t1 t2) (tuple 2 t1 t2)) (##& U tuple 0) construct {
  \t1 t2 => ##& (T t1 t2) (tuple 2 t1 t2)
  ##& U tuple 0
}
""")(NameTree.empty, InferredKindTable.empty, InferredTypeTable.empty, emptyInstTree, InstanceArgTable.empty)(f3)(g3).run(emptyTypeEnv)
      inside(res) {
        case Success(Tree(combs, treeInfo)) =>
          val typeTree = treeInfoExtractor.typeTreeFromTreeInfo(treeInfo.treeInfo)
          val typeCombs = typeTree.combs
          val typeTreeInfo = typeTree.treeInfo
          val combSyms = Set(
              GlobalSymbol(NonEmptyList("f")),
              GlobalSymbol(NonEmptyList("g")),
              GlobalSymbol(NonEmptyList("h")),
              GlobalSymbol(NonEmptyList("i")),
              GlobalSymbol(NonEmptyList("j")))
          val combLocs = combSyms.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo))
          combLocs should have size(5)
          treeInfo.typeTable.types.keySet should be ===(combLocs)
          val typeCombSyms = List(GlobalSymbol(NonEmptyList("T")), GlobalSymbol(NonEmptyList("U")))
          inside(typeCombSyms.flatMap(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo))) {
            case List(tLoc, uLoc) =>
              treeInfo.instTree.instCount should be ===(6)
              // i 
              inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("i"))).flatMap { l => treeInfo.instTree.instGroupTables.get(PolyFunction(l)) }) {
                case Some(instGroupTable) =>
                  inside(instGroupTable.instGroups.find { _._1 == GroupIdentity(DefaultGroupNodeIdentity, Nil) }) {
                    case Some((_, instGroup)) =>
                      instGroup.pairs should have size(2)
                      inside(for {
                        x1 <- instGroup.pairs.collectFirst { 
                          case (GlobalInstanceType(InferredType(TupleType(Seq(BuiltinType(TypeBuiltinFunction.Boolean, Seq()), BuiltinType(TypeBuiltinFunction.Char, Seq()))), Seq())), inst1) => inst1
                        }
                        x2 <- instGroup.pairs.collectFirst { 
                          case (GlobalInstanceType(InferredType(TupleType(Seq(BuiltinType(TypeBuiltinFunction.Boolean, Seq()), BuiltinType(TypeBuiltinFunction.Double, Seq()))), Seq())), inst2) => inst2
                        }
                      } yield (x1, x2)) {
                        case Some((inst1, inst2)) =>
                          inside(inst1) {
                            case PolyFunInstance(loc1, _, _) =>
                              some(loc1) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))))
                          }
                          inside(inst2) {
                            case PolyFunInstance(loc2, _, _) =>
                              some(loc2) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("g"))))
                          }
                      }
                  }
              }
              // j
              inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("j"))).flatMap { l => treeInfo.instTree.instGroupTables.get(PolyFunction(l)) }) {
                case Some(instGroupTable) =>
                  inside(instGroupTable.instGroups.find { _._1 == GroupIdentity(BuiltinTypeGroupNodeIdentity(GroupTypeBuiltinFunction.Char), Nil) }) {
                    case Some((_, instGroup)) =>
                      instGroup.pairs should have size(1)
                      inside(for {
                        x1 <- instGroup.pairs.collectFirst { 
                          case (GlobalInstanceType(InferredType(BuiltinType(TypeBuiltinFunction.Char, Seq()), Seq())), inst1) => inst1
                        }
                      } yield x1) {
                        case Some(inst1) =>
                          inside(inst1) {
                            case PolyFunInstance(loc1, _, _) =>
                              some(loc1) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("h"))))
                          }
                      }
                  }
              }
              // select
              inside(treeInfo.instTree.instGroupTables.get(SelectFunction)) {
                case Some(instGroupTable) =>
                  inside(instGroupTable.instGroups.find { _._1 == GroupIdentity(DefaultGroupNodeIdentity, Nil) }) {
                    case Some((_, instGroup)) =>
                      instGroup.pairs should have size(1)
                      inside(for {
                        x1 <- instGroup.pairs.collectFirst { case (GlobalInstanceType(type1), inst1) => (type1, inst1) }
                      } yield (x1)) {
                        case Some((type1, inst1)) =>
                          inside(type1) {
                            case InferredType(TypeDisjunction(types11), argKinds1) =>
                              // \t1 t2 => ((T t1 t2) #& (t1, t2)) #| (U #& ())
                              types11 should have size(2)
                              inside(for {
                                x1 <- types11.collectFirst { 
                                  case type111 @ TypeConjunction(types111) if types111.size == 2 && types111.collectFirst { case TupleType(Seq(_, _)) => () }.size == 1 => type111
                                }
                                x2 <- types11.collectFirst { 
                                  case type112 @ TypeConjunction(types112) if types112.size == 2 && types112.collectFirst { case TupleType(Seq()) => () }.size == 1 => type112
                                }
                              } yield (x1, x2)) {
                                case Some((type111, type112)) =>
                                  inside(type111) {
                                    case TypeConjunction(types111) =>
                                      types111 should have size(2)
                                      inside(for {
                                        x1 <- types111.collectFirst { case GlobalTypeApp(loc1111, Seq(TypeValueLambda(Seq(), TypeParamApp(param1111, Seq(), 0)), TypeValueLambda(Seq(), TypeParamApp(param1112, Seq(), 0))), GlobalSymbol(NonEmptyList("T"))) => (loc1111, param1111, param1112)}
                                        x2 <- types111.collectFirst { case TupleType(Seq(TypeParamApp(param1113, Seq(), 0), TypeParamApp(param1114, Seq(), 0))) => (param1113, param1114) }
                                      } yield (x1, x2)) {
                                        case Some(((loc1111, param1111, param1112), (param1113, param1114))) =>
                                          loc1111 should be ===(tLoc)
                                          List(param1111, param1113).toSet should have size(1)
                                          List(param1112, param1114).toSet should have size(1)
                                          List(param1111, param1112, param1113, param1114).toSet should have size(2)
                                      }
                                  }
                                  inside(type112) {
                                    case TypeConjunction(types112) =>
                                      types112 should have size(2)
                                      inside(for {
                                        x1 <- types112.collectFirst { case GlobalTypeApp(loc1121, Seq(), GlobalSymbol(NonEmptyList("U"))) => loc1121 }
                                        _ <- types112.collectFirst { case TupleType(Seq()) => () }
                                      } yield x1) {
                                        case Some(loc1121) =>
                                          loc1121 should be ===(uLoc)
                                      }
                                  }
                              }
                              inside(argKinds1) {
                                case Seq(
                                    InferredKind(Star(KindType, _)) /* * */,
                                    InferredKind(Star(KindType, _)) /* * */) =>
                                  ()
                              }
                          }
                          inside(inst1) { case SelectInstance(2, _, _) => () }
                      }
                  }
              }
              // construct
              inside(treeInfo.instTree.instGroupTables.get(ConstructFunction)) {
                case Some(instGroupTable) =>
                  inside(instGroupTable.instGroups.find { _._1 == GroupIdentity(DefaultGroupNodeIdentity, Nil) }) { 
                    case Some((_, instGroup)) =>
                      instGroup.pairs should have size(2)
                      inside(for {
                        x1 <- instGroup.pairs.collectFirst { 
                          case (GlobalInstanceType(type1 @ InferredType(TypeConjunction(types11), _)), inst1) if types11.size == 2 && types11.collectFirst { case TupleType(Seq(_, _)) => () }.size == 1 => (type1, inst1)
                        }
                        x2 <- instGroup.pairs.collectFirst { 
                          case (GlobalInstanceType(type2 @ InferredType(TypeConjunction(types21), _)), inst2) if types21.size == 2 && types21.collectFirst { case TupleType(Seq()) => () }.size == 1 => (type2, inst2)
                        }
                      } yield (x1, x2)) {
                        case Some(((type1, inst1), (type2, inst2))) =>
                          inside(type1) {
                            case InferredType(TypeConjunction(types11), argKinds1) =>
                              types11 should have size(2)
                              inside(for {
                                x1 <- types11.collectFirst { case GlobalTypeApp(loc111, Seq(TypeValueLambda(Seq(), TypeParamApp(param111, Seq(), 0)), TypeValueLambda(Seq(), TypeParamApp(param112, Seq(), 0))), GlobalSymbol(NonEmptyList("T"))) => (loc111, param111, param112) }
                                x2 <- types11.collectFirst { case TupleType(Seq(TypeParamApp(param113, Seq(), 0), TypeParamApp(param114, Seq(), 0))) => (param113, param114) }
                              } yield (x1, x2)) {
                                case Some(((loc111, param111, param112), (param113, param114))) =>
                                  loc111 should be ===(tLoc)
                                  List(param111, param113).toSet should have size(1)
                                  List(param112, param114).toSet should have size(1)
                                  List(param111, param112, param113, param114).toSet should have size(2)
                              }
                              inside(argKinds1) {
                                case Seq(
                                    InferredKind(Star(KindType, _)) /* * */,
                                    InferredKind(Star(KindType, _)) /* * */) =>
                                  ()
                              }
                          }
                          inside(inst1) { case ConstructInstance(0, _, _) => () }
                          inside(type2) {
                            case InferredType(TypeConjunction(types21), Seq()) =>
                              types21 should have size(2)
                              inside(for {
                                x1 <- types21.collectFirst { case GlobalTypeApp(loc211, Seq(), GlobalSymbol(NonEmptyList("U"))) => loc211 }
                                _ <- types21.collectFirst { case TupleType(Seq()) => () }
                              } yield x1) {
                                case Some(loc211) =>
                                  loc211 should be ===(uLoc)
                              }
                          }
                          inside(inst2) { case ConstructInstance(1, _, _) => () }
                      }
                  }
              }
          }
      }
    }
    
    it should "transform the instantiation fields for the non-recursive dependent combinators with the local instances" in {
val (typeEnv, res) = Instantiator.transformString("""
poly f
poly g
poly h
poly i
instance f => j
instance h => k
j x = x
k = 0.1
l x = tuple 2 ((f: \t1 => ##-> t1 t1) x) g
(m: \t1 t2 => tuple 3 t1 t1 t2) = tuple 3 f f i
n = tuple 3 (l 1.0f) (h: #Double) m
""")(NameTree.empty, InferredKindTable.empty, InferredTypeTable.empty, emptyInstTree, InstanceArgTable.empty)(f3)(g3).run(emptyTypeEnv)
      inside(res) {
        case Success(Tree(combs, treeInfo)) =>
          val typeTree = treeInfoExtractor.typeTreeFromTreeInfo(treeInfo.treeInfo)
          val typeCombs = typeTree.combs
          val typeTreeInfo = typeTree.treeInfo
          val combSyms = Set(
              GlobalSymbol(NonEmptyList("f")),
              GlobalSymbol(NonEmptyList("g")), 
              GlobalSymbol(NonEmptyList("h")),
              GlobalSymbol(NonEmptyList("i")),
              GlobalSymbol(NonEmptyList("j")),
              GlobalSymbol(NonEmptyList("k")),
              GlobalSymbol(NonEmptyList("l")),
              GlobalSymbol(NonEmptyList("n")),
              GlobalSymbol(NonEmptyList("m")))
          val combLocs = combSyms.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo))
          combLocs should have size(9)
          treeInfo.instArgTable.instArgs.keySet should be ===(combLocs)
          // l
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("l"))).flatMap(combs.get)) {
            case Some(Combinator(None, _, body, LambdaInfo(lambdaInfo, 0, typeTable, Seq()), _)) =>
              inside(body) {
                case App(_, args1, _) =>
                  inside(args1) {
                    case NonEmptyList(arg11, arg12) =>
                      inside(arg11) {
                        case App(fun2, _, _) =>
                          inside(fun2) {
                            case Simple(TypedTerm(Simple(Var(_, LambdaInfo(lambdaInfo2, 1, _, insts2)), _), _), _) =>
                              inside(insts2) {
                                case Seq(PolyFunInstance(loc211, _, _)) =>
                                  some(loc211) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("j"))))
                              }
                          }
                      }
                      inside(arg12) {
                        case Simple(Var(_, LambdaInfo(lambdaInfo12, 3, _, insts12)), _) =>
                          inside(insts12) { case Seq(LocalInstance(0)) => () }
                      }
                  }
              }
          }
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("l"))).flatMap(treeInfo.instArgTable.instArgs.get)) {
            case Some(Seq(instArg1)) =>
              inside(instArg1) {
                case InstanceArg(PolyFunction(loc1), type1) =>
                  some(loc1) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("g"))))
                  inside(type1) {
                    case InferredType(TypeParamApp(_, Seq(), 0), argKinds1) =>
                      // \(t1: *) => t1
                      inside(argKinds1) {
                        case Seq(
                            InferredKind(Star(KindType, _)) /* * */,
                            InferredKind(Star(KindType, _)) /* * */) =>
                          ()
                      }
                  }
              }
          }
          // m
          def testMInstArgs(localInstIdx1: Int, localInstIdx2: Int)
          {
            inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("m"))).flatMap(treeInfo.instArgTable.instArgs.get)) {
              case Some(instArgs) =>
                instArgs should have size(2)
                inside(instArgs.lift(localInstIdx1)) {
                  case Some(InstanceArg(PolyFunction(loc1), type1)) =>
                    some(loc1) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))))
                    inside(type1) {
                      case InferredType(TypeParamApp(param11, Seq(), 0), argKinds1) =>
                        // \(t1: *) (t2: *) => t1
                        inside(argKinds1) {
                          case Seq(
                              InferredKind(Star(KindType, _)) /* * */,
                              InferredKind(Star(KindType, _)) /* * */) =>
                            ()
                        }
                        inside(instArgs.lift(localInstIdx2)) {
                          case Some(InstanceArg(PolyFunction(loc2), type2)) =>
                            some(loc2) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("i"))))
                            inside(type2) {
                              case InferredType(TypeParamApp(param21, Seq(), 0), argKinds2) =>
                                // \(t1: *) (t2: *) => t2
                                List(param11, param21).toSet should have size(2)
                                inside(argKinds1) {
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
          }
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("m"))).flatMap(combs.get)) {
            case Some(Combinator(Some(_), Nil, body, LambdaInfo(lambdaInfo, 0, typeTable, Seq()), _)) =>
              inside(body) {
                case App(_, args1, _) =>
                  inside(args1) {
                    case NonEmptyList(arg11, arg12, arg13) =>
                      inside(arg11) {
                        case Simple(Var(_, LambdaInfo(lambdaInfo11, 1, _, insts11)), _) =>
                          inside(insts11) {
                            case Seq(LocalInstance(localInstIdx111)) =>
                              inside(arg12) {
                                case Simple(Var(_, LambdaInfo(lambdaInfo12, 2, _, insts12)), _) =>
                                  inside(insts12) {
                                    case Seq(LocalInstance(localInstIdx121)) =>
                                      inside(arg13) {
                                        case Simple(Var(_, LambdaInfo(lambdaInfo13, 3, _, insts13)), _) =>
                                          inside(insts13) {
                                            case Seq(LocalInstance(localInstIdx131)) =>
                                              List(localInstIdx111, localInstIdx121).toSet should have size(1)
                                              List(localInstIdx111, localInstIdx121, localInstIdx131).toSet should have size(2)
                                              testMInstArgs(localInstIdx111, localInstIdx131)
                                          }
                                      }
                                  }
                              }
                          }
                      }
                  }
              }
          }
          // n
          def testNInstArgs(localInstIdx1: Int, localInstIdx2: Int, localInstIdx3: Int)
          {
            inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("n"))).flatMap(treeInfo.instArgTable.instArgs.get)) {
              case Some(instArgs) =>
                instArgs should have size(3)
                inside(instArgs.lift(localInstIdx1)) {
                  case Some(InstanceArg(PolyFunction(loc1), type1)) =>
                    some(loc1) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("g"))))
                    inside(type1) {
                      case InferredType(TypeParamApp(param11, Seq(), 0), argKinds1) =>
                        // \(t1: *) (t2: *) (t3: *) => t1
                        inside(argKinds1) {
                          case Seq(
                              InferredKind(Star(KindType, _)) /* * */,
                              InferredKind(Star(KindType, _)) /* * */,
                              InferredKind(Star(KindType, _)) /* * */) =>
                            ()
                        }
                        val tmpInstArgs = instArgs.zipWithIndex.filter { _._2 != localInstIdx1 }.map { _._1 }
                        val syms = List(GlobalSymbol(NonEmptyList("f")), GlobalSymbol(NonEmptyList("i")))
                        inside(syms.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo))) {
                          case List(fLoc, iLoc) =>
                            inside(for {
                              x1 <- tmpInstArgs.collectFirst { case InstanceArg(PolyFunction(loc21), type2) if loc21 == fLoc => type2 }
                              x2 <- tmpInstArgs.collectFirst { case InstanceArg(PolyFunction(loc31), type3) if loc31 == iLoc => type3 }
                            } yield (x1, x2)) {
                              case Some((type2, type3)) =>
                                inside(type2) {
                                  case InferredType(TypeParamApp(param21, Seq(), 0), argKinds2) =>
                                    // \(t1: *) (t2: *) (t3: *) => t2
                                    inside(argKinds2) {
                                      case Seq(
                                          InferredKind(Star(KindType, _)) /* * */,
                                          InferredKind(Star(KindType, _)) /* * */,
                                          InferredKind(Star(KindType, _)) /* * */) =>
                                        ()
                                    }
                                    inside(type3) {
                                      case InferredType(TypeParamApp(param31, Seq(), 0), argKinds3) =>
                                        // \(t1: *) (t2: *) (t3: *) => t3
                                        List(param11, param21, param31).toSet should have size(3)
                                        inside(argKinds3) {
                                          case Seq(
                                              InferredKind(Star(KindType, _)) /* * */,
                                              InferredKind(Star(KindType, _)) /* * */,
                                              InferredKind(Star(KindType, _)) /* * */) =>
                                            ()
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
          }
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("n"))).flatMap(combs.get)) {
            case Some(Combinator(None, Nil, body, LambdaInfo(lambdaInfo, 0, typeTable, Seq()), _)) =>
              inside(body) {
                case App(_, args1, _) =>
                  inside(args1) {
                    case NonEmptyList(arg11, arg12, arg13) =>
                      inside(arg11) {
                       case App(fun2, _, _) => 
                          inside(fun2) {
                            case Simple(Var(_, LambdaInfo(lambdaInfo21, 1, _, insts21)), _) =>
                              inside(insts21) {
                                case Seq(LocalInstance(localInstIdx211)) =>
                                  inside(arg13) {
                                    case Simple(Var(_, LambdaInfo(lambdaInfo13, 3, _, insts13)), _) =>
                                      inside(insts13) {
                                        case Seq(LocalInstance(localInstIdx131), LocalInstance(localInstIdx132)) =>
                                          List(localInstIdx211, localInstIdx131, localInstIdx132).toSet should have size(3)
                                          testNInstArgs(localInstIdx211, localInstIdx131, localInstIdx132)
                                      }
                                  }
                              }
                          }
                      }
                      inside(arg12) {
                        case Simple(TypedTerm(Simple(Var(_, LambdaInfo(lambdaInfo12, 2, _, insts12)), _), _), _) =>
                          inside(insts12) {
                            case Seq(PolyFunInstance(loc121, _, _)) =>
                              some(loc121) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("k"))))
                          }
                      }
                  }
              }
          }
	  }
    }
    
    it should "transform the instantiation fields for the recursive dependent combinators with the local instances" in {
      val (typeEnv, res) = Instantiator.transformString("""
f = g true j
g x y = #cond (\_ => h x) (\_ => i) x
h x = #cond (\_ => f) (\_ => #fAdd k l) x
i = #cond (\_ => g true (construct 1)) (\_ => j) true
poly j
poly k
poly l
instance j => m
instance l => n
m = 0.1f
n = 0.2f
""")(NameTree.empty, InferredKindTable.empty, InferredTypeTable.empty, emptyInstTree, InstanceArgTable.empty)(f3)(g3).run(emptyTypeEnv)
      inside(res) {
        case Success(Tree(combs, treeInfo)) =>
          val typeTree = treeInfoExtractor.typeTreeFromTreeInfo(treeInfo.treeInfo)
          val typeCombs = typeTree.combs
          val typeTreeInfo = typeTree.treeInfo
          val combSyms = Set(
              GlobalSymbol(NonEmptyList("f")),
              GlobalSymbol(NonEmptyList("g")), 
              GlobalSymbol(NonEmptyList("h")),
              GlobalSymbol(NonEmptyList("i")),
              GlobalSymbol(NonEmptyList("j")),
              GlobalSymbol(NonEmptyList("k")),
              GlobalSymbol(NonEmptyList("l")),
              GlobalSymbol(NonEmptyList("n")),
              GlobalSymbol(NonEmptyList("m")))
          val combLocs = combSyms.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo))
          combLocs should have size(9)
          treeInfo.instArgTable.instArgs.keySet should be ===(combLocs)
          def testInstArgs(sym: GlobalSymbol, optLocalInstIdx1: Option[Int], optLocalInstIdx2: Option[Int], optLocalInstIdx3: Option[Int])
          {
            inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(sym).flatMap(treeInfo.instArgTable.instArgs.get)) {
              case Some(instArgs) =>
                val syms = List(GlobalSymbol(NonEmptyList("j")), GlobalSymbol(NonEmptyList("k")))
                inside(syms.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo))) {
                  case List(jLoc, kLoc) =>
                    instArgs should have size(3)
                    inside(for {
                      x1 <- instArgs.zipWithIndex.collectFirst { case (InstanceArg(PolyFunction(loc1), type1), expectedLocalInstIdx1) if loc1 == jLoc => (type1, expectedLocalInstIdx1) }
                      x2 <- instArgs.zipWithIndex.collectFirst { case (InstanceArg(PolyFunction(loc2), type2), expectedLocalInstIdx2) if loc2 == kLoc => (type2, expectedLocalInstIdx2) }
                      x3 <- instArgs.zipWithIndex.collectFirst { case (InstanceArg(ConstructFunction, type3), expectedLocalInstIdx3) => (type3, expectedLocalInstIdx3) } 
                    } yield (x1, x2, x3)) {
                      case Some(((type1, expectedLocalInstIdx1), (type2, expectedLocalInstIdx2), (type3, expectedLocalInstIdx3))) =>
                        inside(type1) {
                          case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(arg11, ret11)), argKinds1) =>
                            // \(t1: *) (t2: *) (t3: *) => t1 #-> (t2 #& (tuple 1 t1))
                            inside(arg11) {
                              case TypeParamApp(param11, Seq(), 0) =>
                                inside(ret11) {
                                  case TypeConjunction(types11)  =>
                                    inside(for {
                                      x1 <- types11.collectFirst { case TypeParamApp(param111, Seq(), 0) => param111 }
                                      x2 <- types11.collectFirst { case TupleType(Seq(TypeParamApp(param112, Seq(), 0))) => param112 }
                                    } yield (x1, x2)) {
                                      case Some((param111, param112)) =>
                                        inside(type3) {
                                          case InferredType(TypeConjunction(types31), argKinds3) =>
                                            // \(t1: *) (t2: *) => t2 #& (tuple 1 t1)
                                            inside(for {
                                              x1 <- types31.collectFirst { case TypeParamApp(param311, Seq(), 0) => param311 }
                                              x2 <- types31.collectFirst { case TupleType(Seq(TypeParamApp(param312, Seq(), 0))) => param312 }
                                            } yield (x1, x2)) {
                                              case Some((param311, param312)) =>
                                                List(param111, param311).toSet should have size(1)
                                                List(param112, param312).toSet should have size(1)
                                                List(param111, param112, param311, param312).toSet should have size(2)
                                            }
                                            inside(argKinds3) {
                                              case Seq(
                                                  InferredKind(Star(KindType, _)) /* * */,
                                                  InferredKind(Star(KindType, _)) /* * */) =>
                                                ()
                                            }
                                        }
                                    }
                                }
                            }
                            inside(argKinds1) {
                              case Seq(
                                  InferredKind(Star(KindType, _)) /* * */,
                                  InferredKind(Star(KindType, _)) /* * */) =>
                                ()
                            }
                        }
                        inside(type2) {
                          case InferredType(BuiltinType(TypeBuiltinFunction.Float, Seq()), argKinds2) =>
                            // \(t1: *) (t2: *) => #Float
                            inside(argKinds2) {
                              case Seq(
                                  InferredKind(Star(KindType, _)) /* * */,
                                  InferredKind(Star(KindType, _)) /* * */) =>
                                ()
                            }
                        }
                        for(localInstIdx1 <- optLocalInstIdx1) {
                          localInstIdx1 should be ===(expectedLocalInstIdx1)
                        }
                        for(localInstIdx2 <- optLocalInstIdx2) {
                          localInstIdx2 should be ===(expectedLocalInstIdx2)
                        }
                        for(localInstIdx3 <- optLocalInstIdx3) {
                          localInstIdx3 should be ===(expectedLocalInstIdx3)
                        }
                    }
                }
            }
          }
          // f 
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))).flatMap(combs.get)) {
            case Some(Combinator(None, _, body, LambdaInfo(lambdaInfo, 0, typeTable, Seq()), _)) =>
              inside(body) {
                case App(fun1, args1, _) =>
                  inside(fun1) {
                    case Simple(Var(_, LambdaInfo(lambdaInfo1, 1, _, insts1)), _) =>
                      inside(insts1) { case Seq(LocalInstance(0), LocalInstance(1), LocalInstance(2)) => () }
                      inside(args1) {
                        case NonEmptyList(_, arg11) =>
                          inside(arg11) {
                            case Simple(Var(_, LambdaInfo(lambdaInfo11, 2, _, insts11)), _) =>
                              inside(insts11) {
                                case Seq(LocalInstance(localInstIdx11)) =>
                                  testInstArgs(GlobalSymbol(NonEmptyList("f")), some(localInstIdx11), none, none)
                              }
                          }
                      }
                  }
              }
              
          }
          // g
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("g"))).flatMap(combs.get)) {
            case Some(Combinator(None, _, body, LambdaInfo(lambdaInfo, 0, typeTable, Seq()), _)) =>
              inside(body) {
                case App(_, args1, _) =>
                  inside(args1) {
                    case NonEmptyList(arg11, arg12, _) =>
                      inside(arg11) {
                        case Simple(Lambda(_, body11, _), _) =>
                          inside(body11) {
                            case App(fun111, _, _) =>
                              inside(fun111) {
                                case Simple(Var(_, LambdaInfo(lambdaInfo111, 2, _, insts111)), _) =>
                                  inside(insts111) { case Seq(LocalInstance(0), LocalInstance(1), LocalInstance(2)) => () }
                              }
                          }
                      }
                      inside(arg12) {
                        case Simple(Lambda(_, body12, _), _) =>
                          inside(body12) {
                            case Simple(Var(_, LambdaInfo(lambdaInfo121, 5, _, insts121)), _) =>
                              inside(insts121) { case Seq(LocalInstance(0), LocalInstance(1), LocalInstance(2)) => () }
                          }
                      }
                  }
              }
          }
          testInstArgs(GlobalSymbol(NonEmptyList("g")), none, none, none)
          // h
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("h"))).flatMap(combs.get)) {
            case Some(Combinator(None, _, body, LambdaInfo(lambdaInfo, 0, typeTable, Seq()), _)) =>
              inside(body) {
                case App(_, args1, _) =>
                  inside(args1) {
                    case NonEmptyList(arg11, arg12, _) =>
                      inside(arg11) {
                        case Simple(Lambda(_, body11, _), _) =>
                          inside(body11) {
                            case Simple(Var(_, LambdaInfo(lambdaInfo111, 2, _, insts111)), _) =>
                              inside(insts111) { case Seq(LocalInstance(0), LocalInstance(1), LocalInstance(2)) => () }
                          }
                      }
                      inside(arg12) {
                        case Simple(Lambda(_, body12, _), _) =>
                          inside(body12) {
                            case App(_, args121, _) =>
                              inside(args121) {
                                case NonEmptyList(arg1211, arg1212) =>
                                  inside(arg1211) {
                                    case Simple(Var(_, LambdaInfo(lambdaInfo1211, 4, _, insts1211)), _) =>
                                      inside(insts1211) {
                                        case Seq(LocalInstance(localInstIdx12111)) =>
                                          testInstArgs(GlobalSymbol(NonEmptyList("h")), none, some(localInstIdx12111), none)
                                      }
                                      inside(arg1212) {
                                        case Simple(Var(_, LambdaInfo(lambdaInfo1212, 5, _, insts1212)), _) =>
                                          inside(insts1212) {
                                            case Seq(PolyFunInstance(loc12111, _, _)) =>
                                              some(loc12111) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("n"))))
                                          }
                                      }
                                  }
                              }
                          }
                      }
                  }
              }
          }
          // i
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("i"))).flatMap(combs.get)) {
            case Some(Combinator(None, _, body, LambdaInfo(lambdaInfo, 0, typeTable, Seq()), _)) =>
              inside(body) {
                case App(_, args1, _) =>
                  inside(args1) {
                    case NonEmptyList(arg11, arg12, _) =>
                      inside(arg11) {
                        case Simple(Lambda(_, body11, _), _) =>
                          inside(body11) {
                            case App(fun111, args111, _) =>
                              inside(fun111) {
                                case Simple(Var(_, LambdaInfo(lambdaInfo111, 2, _, insts111)), _) =>
                                  inside(insts111) { case Seq(LocalInstance(0), LocalInstance(1), LocalInstance(2)) => () }
                              }
                              inside(args111) {
                                case NonEmptyList(_, arg1111) =>
                                  inside(arg1111) {
                                    case Simple(Construct(1, LambdaInfo(lambdaInfo1111, 3, _, insts1111)), _) =>
                                      inside(insts1111) {
                                        case Seq(LocalInstance(localInstIdx11111)) =>
                                          testInstArgs(GlobalSymbol(NonEmptyList("i")), none, none, some(localInstIdx11111))
                                      }
                                  }
                              }
                          }
                      }
                      inside(arg12) {
                        case Simple(Lambda(_, body12, _), _) =>
                          inside(body12) {
                            case Simple(Var(_, LambdaInfo(lambdaInfo121, 5, _, insts121)), _) =>
                              inside(insts121) { 
                                case Seq(PolyFunInstance(loc1211, _, _)) =>
                                  some(loc1211) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("m"))))
                              }
                          }
                      }
                  }
              }
          }
      }
    }
    
    it should "transform the string with the instances of the other tree" in {
      val s = """
unittype 0 T
instance select ##& T tuple 0 construct {
  ##& T tuple 0
}
f x = x
instance g => h
poly g
h = true
"""
      val (typeEnv, res) = Instantiator.transformString(s)(NameTree.empty, InferredKindTable.empty, InferredTypeTable.empty, emptyInstTree, InstanceArgTable.empty)(f3)(g3).run(emptyTypeEnv)
      inside(res) {
        case Success(Tree(_, treeInfo)) =>
          val nameTree = NameTree.fromGlobalSymbols(Set(
              GlobalSymbol(NonEmptyList("f")),
              GlobalSymbol(NonEmptyList("g")),
              GlobalSymbol(NonEmptyList("h")))) |+| NameTree.fromTypeGlobalSymbol(GlobalSymbol(NonEmptyList("T")))
          val typeTree = treeInfoExtractor.typeTreeFromTreeInfo(treeInfo.treeInfo)
          val typeTreeInfo = typeTree.treeInfo
          inside(makeData(s)) {
            case Success(data) =>
              val (typeEnv2, res2) = Instantiator.transformString("""
i x = tuple 2 (construct 0: ##& T tuple 0) (f x)
instance g => j
j = 'a'
""")(nameTree, typeTreeInfo.kindTable, treeInfo.typeTable, treeInfo.instTree, treeInfo.instArgTable)(f4(data))(g3).run(typeEnv)
              inside(res2) {
                case Success(Tree(combs2, treeInfo2)) =>
                  val combSyms2 = Set(
                      GlobalSymbol(NonEmptyList("i")),
                      GlobalSymbol(NonEmptyList("j")))
                  val insts2 = instTreeInfoExtractor.instancesFromTreeInfo(treeInfo2.treeInfo)
                  val selectConstructInsts2 = instTreeInfoExtractor.selectConstructInstancesFromTreeInfo(treeInfo2.treeInfo)
                  val combLocs2 = combSyms2.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfo2.treeInfo))
                  combLocs2 should have size(2)
                  treeInfo2.typeTable.types.keySet should be ===(combLocs2)
                  treeInfo2.instArgTable.instArgs.keySet should be ===(combLocs2)
                  val instSyms2 = Set(GlobalSymbol(NonEmptyList("g")))
                  val instLocs2 = instSyms2.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo))
                  instLocs2 should have size(1)
                  inside(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("T")))) {
                    case Some(tLoc) =>
                      // i
                      inside(globalSymTabular.getGlobalLocationFromTable(treeInfo2.treeInfo)(GlobalSymbol(NonEmptyList("i"))).flatMap(combs2.get)) {
                        case Some(Combinator(None, args, body, LambdaInfo(lambdaInfo, 0, typeTable, Seq()), _)) =>
                          inside(args) { case List(Arg(Some("x"), None, _)) => () }
                          val syms = Set(LocalSymbol("x"))
                          val locs = syms.flatMap(localSymTabular.getLocalLocationFromTable(lambdaInfo))
                          locs should have size(syms.size)
                          inside(body) {
                            case App(fun1, args1, _) =>
                              inside(fun1) { case Simple(Literal(TupleFunValue(2)), _) => () }
                              inside(args1) {
                                case NonEmptyList(arg11, arg12) =>
                                  inside(arg11) {
                                    case Simple(TypedTerm(Simple(Construct(0, LambdaInfo(lambdaInfo11, 1, typeTable11, insts11)), _), type11), _) =>
                                      typeTable11.types should be ('empty)
                                      inside(insts11) { case Seq(ConstructInstance(0, _, _)) => () }
                                      inside(type11) {
                                        case App(typFun111, typArgs111, _) =>
                                          inside(typFun111) { case Simple(TypeLiteral(TypeBuiltinFunValue(frontend.TypeBuiltinFunction.Conj)), _) => () }
                                          inside(typArgs111) {
                                            case NonEmptyList(typArg1111, typArg1112) =>
                                              inside(typArg1111) {
                                                case Simple(TypeVar(typLoc1111), _) =>
                                                  some(typLoc1111) should be ===(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("T"))))
                                              }
                                              inside(typArg1112) { case Simple(TypeLiteral(TupleTypeFunValue(0)), _) => () }
                                          }
                                      }
                                  }
                                  inside(arg12) {
                                    case App(fun121, args121, _) =>
                                      inside(fun121) {
                                        case Simple(Var(loc121, LambdaInfo(lambdaInfo121, 2, typeTable121, Seq())), _) =>
                                          some(loc121) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))))
                                          typeTable121.types should be ('empty)
                                      }
                                      inside(args121) {
                                        case NonEmptyList(arg1211) =>
                                          inside(arg1211) {
                                            case Simple(Var(loc1211, LambdaInfo(lambdaInfo1211, 3, typeTable1211, Seq())), _) =>
                                              some(loc1211) should be ===(localSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("x")))
                                              typeTable1211.types should be ('empty)
                                          }
                                      }
                                  }
                              }
                          }
                          inside(localSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("x")).flatMap(typeTable.types.get)) {
                            case Some(InferredType(TypeParamApp(_, Seq(), 0), argKinds)) =>
                              // \(t1: *) => t1
                              inside(argKinds) {
                                case Seq(
                                    InferredKind(Star(KindType, _)) /* * */) =>
                                  ()
                              }
                          }
                      }
                      inside(globalSymTabular.getGlobalLocationFromTable(treeInfo2.treeInfo)(GlobalSymbol(NonEmptyList("i"))).flatMap(treeInfo2.typeTable.types.get)) {
                        case Some(InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(arg1, ret1)), argKinds)) =>
                          // \t1 => t1 #-> (T #& (), t1) 
                          inside(arg1) {
                            case TypeParamApp(param1, Seq(), 0) =>
                              inside(ret1) {
                                case TupleType(Seq(TypeConjunction(types11), TypeParamApp(param12, Seq(), 0))) =>
                                  inside(for {
                                    x1 <- types11.collectFirst { case GlobalTypeApp(loc111, Seq(), GlobalSymbol(NonEmptyList("T"))) => loc111 }
                                    _ <- types11.collectFirst { case TupleType(Seq()) => () }
                                  } yield x1) {
                                    case Some(loc111) =>
                                      loc111 should be ===(tLoc)
                                  }
                                  List(param1, param12).toSet should have size(1)
                              }
                          }
                          inside(argKinds) {
                            case Seq(
                                InferredKind(Star(KindType, _)) /* * */) =>
                              ()
                          }
                      }
                      inside(globalSymTabular.getGlobalLocationFromTable(treeInfo2.treeInfo)(GlobalSymbol(NonEmptyList("i"))).flatMap(treeInfo2.instArgTable.instArgs.get)) {
                        case Some(Seq()) => ()
                      }
                      // j
                      inside(globalSymTabular.getGlobalLocationFromTable(treeInfo2.treeInfo)(GlobalSymbol(NonEmptyList("j"))).flatMap(combs2.get)) {
                        case Some(Combinator(None, Nil, body, LambdaInfo(lambdaInfo, 0, typeTable, Seq()), _)) =>
                          inside(body) { case Simple(Literal(CharValue('a')), _) => () }
                          typeTable.types should be ('empty)
                      }
                      inside(globalSymTabular.getGlobalLocationFromTable(treeInfo2.treeInfo)(GlobalSymbol(NonEmptyList("j"))).flatMap(treeInfo2.typeTable.types.get)) {
                        case Some(InferredType(BuiltinType(TypeBuiltinFunction.Char, Seq()), Seq())) =>
                          // #Char
                          ()
                      }
                      inside(globalSymTabular.getGlobalLocationFromTable(treeInfo2.treeInfo)(GlobalSymbol(NonEmptyList("j"))).flatMap(treeInfo2.instArgTable.instArgs.get)) {
                        case Some(Seq()) => ()
                      }
                      // instances
                      inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("g"))).flatMap(insts2.get)) {
                        case Some(List(frontend.Instance(loc1, _, _))) =>
                          some(loc1) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("j"))))
                      }
                      selectConstructInsts2 should be ('empty)
                      treeInfo2.instTree.instCount should be ===(1)
                      // g
                      inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("g"))).flatMap { l => treeInfo2.instTree.instGroupTables.get(PolyFunction(l)) }) {
                        case Some(instGroupTable) =>
                          inside(instGroupTable.instGroups.find { _._1 == GroupIdentity(BuiltinTypeGroupNodeIdentity(GroupTypeBuiltinFunction.Char), Nil) }) {
                            case Some((_, instGroup)) =>
                              instGroup.pairs should have size(1)
                              inside(for { 
                                x1 <- instGroup.pairs.collectFirst { case (GlobalInstanceType(type1), inst1) => (type1, inst1) }
                               } yield x1) {
                                case Some((type1, inst1)) =>
                                  inside(type1) {
                                    case InferredType(BuiltinType(TypeBuiltinFunction.Char, Seq()), Seq()) =>
                                      // #Char
                                      ()
                                  }
                                  inside(inst1) {
                                    case PolyFunInstance(loc1, _, _) =>
                                      some(loc1) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo2.treeInfo)(GlobalSymbol(NonEmptyList("j"))))
                                  }
                              }
                          }
                      }
                  }
              }
          }
      }
    }
    
    it should "transform the string of the term with the instantiation" in {
      val s = """
type T = #Float
instance f => g
poly f
g x y = #fAdd x y
"""
      val (typeEnv, res) = Instantiator.transformString(s)(NameTree.empty, InferredKindTable.empty, InferredTypeTable.empty, emptyInstTree, InstanceArgTable.empty)(f3)(g3).run(emptyTypeEnv)
      inside(res) {
        case Success(Tree(_, treeInfo)) =>
          val nameTree = NameTree.fromGlobalSymbols(Set(
              GlobalSymbol(NonEmptyList("f")),
              GlobalSymbol(NonEmptyList("g")))) |+| NameTree.fromTypeGlobalSymbol(GlobalSymbol(NonEmptyList("T")))
          val typeTree = treeInfoExtractor.typeTreeFromTreeInfo(treeInfo.treeInfo)
          val typeTreeInfo = typeTree.treeInfo
          val (typeEnv2, env) = g3(typeTreeInfo.kindTable, treeInfo.typeTable, treeInfo.instTree, treeInfo.instArgTable).run(typeEnv)
          inside(makeData(s)) {
            case Success(data) =>
              val res2 = Instantiator.transformTermStringWithInstantiation("""
(#fAdd 1.0f (f 2.0f 3.0f)): T
""")(nameTree, env)(h3(data)(typeTreeInfo.kindTable, treeInfo.typeTable, typeEnv2))
              inside(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("T")))) {
                case Some(tLoc) =>
                  inside(res2) {
                    case Success((Simple(TypedTerm(App(fun1, args1, _), type1), _), typ)) =>
                      inside(fun1) { case Simple(Literal(BuiltinFunValue(BuiltinFunction.FAdd)), _) => () }
                      inside(args1) {
                        case NonEmptyList(arg11, arg12) =>
                          inside(arg11) { case Simple(Literal(FloatValue(1.0f)), _) => () }
                          inside(arg12) {
                            case App(fun2, args2, _) =>
                              inside(fun2) {
                                case Simple(Var(loc2, LambdaInfo(lambdaInfo2, 0, typeTable2, insts2)), _) =>
                                  some(loc2) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))))
                                  typeTable2.types should be ('empty)
                                  inside(insts2) {
                                    case Seq(PolyFunInstance(loc21, _, _)) =>
                                      some(loc21) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("g"))))
                                  }
                              }
                              inside(args2) {
                                case NonEmptyList(arg21, arg22) =>
                                  inside(arg21) { case Simple(Literal(FloatValue(2.0f)), _) => () }
                                  inside(arg22) { case Simple(Literal(FloatValue(3.0f)), _) => () }
                              }
                          }
                      }
                      inside(type1) {
                        case Simple(TypeVar(typLoc1), _) =>
                          some(typLoc1) should be ===(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("T"))))
                      }
                      inside(typ) {
                        case InferredType(GlobalTypeApp(loc1, Seq(), GlobalSymbol(NonEmptyList("T"))), Seq()) =>
                          // T
                          loc1 should be ===(tLoc)
                      }
                  }
              }
          }
      }
    }
    
    it should "complain on the already defined instances" in {
      val (typeEnv, res) = Instantiator.transformString("""
instance f => g
instance f => h
instance f => i
instance f => j
instance f => k
poly f
g = 1.0
h = 2.0
i = 3.0
j = 1: #Int
k = 2
""")(NameTree.empty, InferredKindTable.empty, InferredTypeTable.empty, emptyInstTree, InstanceArgTable.empty)(f3)(g3).run(emptyTypeEnv)
      inside(res) {
        case Failure(errs) =>
          errs.map { _.msg } should be ===(NonEmptyList(
              "already defined instance for #.f with #Double",
              "already defined instance for #.f with #Double",
              "already defined instance for #.f with #NonZero #& #Int"))
      }
    }
    
    it should "complain on the ambiguous instances" in {
      val (typeEnv, res) = Instantiator.transformString("""
instance f => g
instance f => h
poly f
g = 1
h = 0
i = f: #Int
j = f: #Int
""")(NameTree.empty, InferredKindTable.empty, InferredTypeTable.empty, emptyInstTree, InstanceArgTable.empty)(f3)(g3).run(emptyTypeEnv)
      inside(res) {
        case Failure(errs) =>
          errs.map { _.msg } should be ===(NonEmptyList(
              "ambiguous instance for #.f with type #Int",
              "ambiguous instance for #.f with type #Int"))
      }
    }
    
    it should "complain on a non-existent instance" in {
      val res = Instantiator.transformTermStringWithInstantiation("construct 2 true 'a'")(NameTree.empty, emptyEnv)(h)
      inside(res) {
        case Failure(errs) =>
          errs.map { _.msg } should be ===(NonEmptyList(
              "couldn't find instance for construct with type \\(t1: *) => t1 #& (#Boolean, #Char)"))
      }
    }
    
    it should "complain on the incorrect construct types for the type term of the tuple" in {
      val (typeEnv, res) = Instantiator.transformString("""
unittype 3 T
unittype 2 U
instance select \t1 t2 t3 => ##| (##& (T t1 t2 t3) (tuple 3 t1 t2 t3)) (##& (U t1 t2) (tuple 2 t1 t2)) construct {
  \t1 t2 t3 => ##& (##& (T t1 t2 t3) (tuple 3 t1 t2 t3)) (tuple 2 t1 t2)
  U
}
""")(NameTree.empty, InferredKindTable.empty, InferredTypeTable.empty, emptyInstTree, InstanceArgTable.empty)(f3)(g3).run(emptyTypeEnv)
      inside(res) {
        case Failure(errs) =>
          errs.map { _.msg } should be ===(NonEmptyList(
              "incorrect construct type \\t1 t2 t3 => (#.T t1 t2 t3) #& (t1, t2, t3) #& (t1, t2)",
              "incorrect construct type \\t1 t2 => #.U t1 t2"))
      }
    }
    
    it should "complain on the combinator that isn't ad-hoc polimorphic" in {
      val (typeEnv, res) = Instantiator.transformString("""
f = 1
g = 2
instance f => g
""")(NameTree.empty, InferredKindTable.empty, InferredTypeTable.empty, emptyInstTree, InstanceArgTable.empty)(f3)(g3).run(emptyTypeEnv)      
      inside(res) {
        case Failure(errs) =>
          errs.map { _.msg } should be ===(NonEmptyList(
              "combinator #.f isn't ad-hoc polimorphic"))
      }
    }
    
    it should "complain on the types of the instance which were matched" in {
      val (typeEnv, res) = Instantiator.transformString("""
poly (f: #Double)
g = 1.0f
instance f => g
""")(NameTree.empty, InferredKindTable.empty, InferredTypeTable.empty, emptyInstTree, InstanceArgTable.empty)(f3)(g3).run(emptyTypeEnv)      
      inside(res) {
        case Failure(errs) =>
          errs.map { _.msg } should be ===(NonEmptyList(
              "couldn't match type #Double with type #Float"))
      }
    }
    
    it should "complain on the instance combinator that requires an instance" in {
      val (typeEnv, res) = Instantiator.transformString("""
poly f
poly g
h x = tuple 2 g x
instance f => h
""")(NameTree.empty, InferredKindTable.empty, InferredTypeTable.empty, emptyInstTree, InstanceArgTable.empty)(f3)(g3).run(emptyTypeEnv)
      inside(res) {
        case Failure(errs) =>
          errs.map { _.msg } should be ===(NonEmptyList(
              "combinator #.h requires instance for #.g with type \\(t1: k1) (t2: *) => t2"))
      }
    }
    
    it should "transform the instantiation fields for the instance with the type parameters and application without the type parameters" in {
      val s = """
poly f
instance f => g
g x y = tuple 2 x y
"""
      val (typeEnv, res) = Instantiator.transformString(s)(NameTree.empty, InferredKindTable.empty, InferredTypeTable.empty, emptyInstTree, InstanceArgTable.empty)(f3)(g3).run(emptyTypeEnv)
      inside(res) {
        case Success(Tree(_, treeInfo)) =>
          val nameTree = NameTree.fromGlobalSymbols(Set(
              GlobalSymbol(NonEmptyList("f")),
              GlobalSymbol(NonEmptyList("g")))) |+| NameTree.fromTypeGlobalSymbol(GlobalSymbol(NonEmptyList("T")))
          val typeTree = treeInfoExtractor.typeTreeFromTreeInfo(treeInfo.treeInfo)
          val typeTreeInfo = typeTree.treeInfo
          val (typeEnv2, env) = g3(typeTreeInfo.kindTable, treeInfo.typeTable, treeInfo.instTree, treeInfo.instArgTable).run(typeEnv)
          inside(makeData(s)) {
            case Success(data) =>
              val res2 = Instantiator.transformTermStringWithInstantiation("""
(f 1.0 2.0): tuple 2 #Double #Double
""")(nameTree, env)(h3(data)(typeTreeInfo.kindTable, treeInfo.typeTable, typeEnv2))
              inside(res2) {
                case Success((Simple(TypedTerm(App(fun1, _, _), _), _), _)) =>
                  inside(fun1) {
                    case Simple(Var(_, LambdaInfo(lambdaInfo1, 0, typeTable1, insts1)), _) =>
                      inside(insts1) {
                        case Seq(PolyFunInstance(loc11, _, _)) =>
                          some(loc11) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("g"))))
                      }
                  }
              }
          }
      }
    }
    
    it should "complain on the incorrect construct types for the type term of the unittype" in {
      val (typeEnv, res) = Instantiator.transformString("""
instance select \t1 t2 => ##| (##| (tuple 2 t1 t2) (##& #Int (tuple 2 t1 t2))) (##& #Empty (tuple 2 t1 t2)) construct {
  \t1 t2 => tuple 2 t1 t2
  \t1 t2 => ##& #Int (tuple 2 t1 t2)
  \t1 t2 => ##& #Empty (tuple 2 t1 t2)
}
""")(NameTree.empty, InferredKindTable.empty, InferredTypeTable.empty, emptyInstTree, InstanceArgTable.empty)(f3)(g3).run(emptyTypeEnv)
      inside(res) {
        case Failure(errs) =>
          errs.map { _.msg } should be ===(NonEmptyList(
              "incorrect construct type \\t1 t2 => (t1, t2)",
              "incorrect construct type \\t1 t2 => #Int #& (t1, t2)",
              "incorrect construct type \\t1 t2 => #Empty #& (t1, t2)"))
      }
    }
  }
  
  "An Instantiator" should behave like instantiator(SymbolInstantiationEnvironment.empty[parser.LambdaInfo, parser.TypeLambdaInfo], SymbolTypeEnvironment.empty[TypeLambdaInfo[parser.TypeLambdaInfo, LocalSymbol]], ())(_ => ().successNel)(_ => Instantiator.statefullyTransformToSymbolTree3)(Instantiator.statefullyMakeSymbolInstantiationEnvironment3)(_ => Instantiator.transformToSymbolTerm2)
}
