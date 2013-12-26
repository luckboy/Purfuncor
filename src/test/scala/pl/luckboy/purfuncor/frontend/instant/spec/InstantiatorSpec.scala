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
  def instantiator[T, U, V, W, X, Y, Z, TT, TU, TV, E, TE, D](emptyEnv: E, emptyTypeEnv: TE)(makeData: String => ValidationNel[AbstractError, D])(f3: (Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]], InferredKindTable[X], InferredTypeTable[T, X]) => State[TE, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, typer.LambdaInfo[V, W, X], TypeSimpleTerm[Y, TypeLambdaInfo[Z, TT]]], typer.TreeInfo[TU, T, X]]]])(g3: (InferredKindTable[X], InferredTypeTable[T, X], InstanceTree[AbstractPolyFunction[T], X, GlobalInstance[T]], InstanceArgTable[T, X]) => State[TE, E])(implicit init: Initializer[NonEmptyList[AbstractError], T, AbstractCombinator[U, typer.LambdaInfo[V, W, X], TypeSimpleTerm[Y, TypeLambdaInfo[Z, TT]]], E], polyFunInstantiator: PolyFunInstantiator[T, Y, X, TypeLambdaInfo[Z, TT], E], enval: InstantiationEnvironmental[E, T, X], treeInfoExtractor: TreeInfoExtractor[TU, Tree[X, AbstractTypeCombinator[Y, TypeLambdaInfo[Z, TT]], TypeTreeInfo[TV, X]]], instTreeInfoExtractor: InstantiationTreeInfoExtractor[TU, T, frontend.Instance[T], SelectConstructInstance[Y, TypeLambdaInfo[Z, TT]]], globalSymTabular: GlobalSymbolTabular[TU, T], typeGlobalSymTabular: GlobalSymbolTabular[TV, X], localSymTabular: LocalSymbolTabular[V, W], typeLocalSymTabular: LocalSymbolTabular[Z, TT])
  {
    val emptyInstTree = InstanceTree.empty[AbstractPolyFunction[T], X, GlobalInstance[T]]
    
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
          def testInstArgs(localInstIdx1: Int, localInstIdx2: Int) {
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
              def testInstArgs(localInstIdx1: Int, localInstIdx2: Int, localInstIdx3: Int) {
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
                                                      InferredKind(Star(KindType, _)) /* * */,
                                                      InferredKind(Star(KindType, _)) /* * */) =>
                                                    ()
                                                }
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
                    }
                    inside(instArgs.lift(localInstIdx3)) {
                      case Some(InstanceArg(ConstructFunction, type3)) =>
                        inside(type3) {
                          case InferredType(GlobalTypeApp(loc31, Seq(), GlobalSymbol(NonEmptyList("U"))), argKinds3) =>
                            loc31 should be ===(uLoc)
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
                                case Case(_, _, _, LambdaInfo(lambdaInfo11, 3, _, insts11)) =>
                                  inside(insts11) { 
                                    case List(LocalInstance(localInstIdx111)) =>
                                      inside(case12) {
                                        case Case(_, _, _, LambdaInfo(lambdaInfo12, 4, _, insts12)) =>
                                          inside(insts12) { 
                                            case List(LocalInstance(localInstIdx121)) =>
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
    
    it should "transform the instances to the instance tree" is (pending)
    
    it should "transform the instantiation fields for the non-recursive dependent combinators with the local instances" is (pending)
    
    it should "transform the instantiation fields for the recursive dependent combinators with the local instances" is (pending)
    
    it should "transform the string with the instances of the other tree" is (pending)
    
    it should "transform the string of the term with the instantiation" is (pending)
    
    it should "complain on the already defined instances" is (pending)
    
    it should "complain on the ambiguous instances" is (pending)
    
    it should "complain on the not found instances" is (pending)
  }
  
  "An Instantiator" should behave like instantiator(SymbolInstantiationEnvironment.empty[parser.LambdaInfo, parser.TypeLambdaInfo], SymbolTypeEnvironment.empty[TypeLambdaInfo[parser.TypeLambdaInfo, LocalSymbol]])(_ => ().successNel)(Instantiator.statefullyTransformToSymbolTree3)(Instantiator.statefullyMakeSymbolTypeInferenceEnvironment3)
}