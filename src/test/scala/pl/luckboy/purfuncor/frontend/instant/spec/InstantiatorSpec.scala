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
import pl.luckboy.purfuncor.frontend.typer.TupleType
import pl.luckboy.purfuncor.frontend.typer.TypeParamApp
import pl.luckboy.purfuncor.frontend.typer.TypeConjunction
import pl.luckboy.purfuncor.frontend.typer.TreeInfo
import pl.luckboy.purfuncor.frontend.typer.SymbolTypeEnvironment
import pl.luckboy.purfuncor.frontend
import pl.luckboy.purfuncor.common.Tree

class InstantiatorSpec extends FlatSpec with ShouldMatchers with Inside
{
  def instantiator[T, U, V, W, X, Y, Z, TT, TU, TV, E, TE, D](emptyEnv: E, emptyTypeEnv: TE)(makeData: String => ValidationNel[AbstractError, D])(f3: (Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]], InferredKindTable[X], InferredTypeTable[T, X]) => State[TE, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, typer.LambdaInfo[V, W, X], TypeSimpleTerm[Y, TypeLambdaInfo[Z, TT]]], typer.TreeInfo[TU, T, X]]]])(g3: (InferredKindTable[X], InferredTypeTable[T, X], InstanceTree[AbstractPolyFunction[T], X, GlobalInstance[T]], InstanceArgTable[T, X]) => State[TE, E])(implicit init: Initializer[NonEmptyList[AbstractError], T, AbstractCombinator[U, typer.LambdaInfo[V, W, X], TypeSimpleTerm[Y, TypeLambdaInfo[Z, TT]]], E], polyFunInstantiator: PolyFunInstantiator[T, Y, X, TypeLambdaInfo[Z, TT], E], enval: InstantiationEnvironmental[E, T, X], treeInfoExtractor: TreeInfoExtractor[TU, Tree[X, AbstractTypeCombinator[Y, TypeLambdaInfo[Z, TT]], TypeTreeInfo[TV, X]]], instTreeInfoExtractor: InstantiationTreeInfoExtractor[TU, T, frontend.Instance[T], SelectConstructInstance[Y, TypeLambdaInfo[Z, TT]]], globalSymTabular: GlobalSymbolTabular[TU, T], typeGlobalSymTabular: GlobalSymbolTabular[TV, X], localSymTabular: LocalSymbolTabular[V, W])
  {
    val emptyInstTree = InstanceTree.empty[AbstractPolyFunction[T], X, GlobalInstance[T]]
    
    it should "transform the string" in {
      val (typeEnv, res) = Instantiator.transformString("""
poly f
poly g
h = tuple 2 f g
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
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("h"))).flatMap(combs.get)) {
            case Some(Combinator(None, Nil, body, LambdaInfo(lambdaInfo, 0, typeTable, Seq()), _)) =>
              inside(body) {
                case App(fun1, args1, _) =>
                  inside(fun1) { case Simple(Literal(TupleFunValue(2)), _) => () }
                  inside(args1) {
                    case NonEmptyList(arg11, arg12) =>
                      inside(arg11) {
                        case Simple(Var(loc11, LambdaInfo(lambdaInfo11, 1, typeTable11, insts11)), _) =>
                          some(loc11) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))))
                          typeTable11.types should be ('empty)
                          inside(insts11) {
                            case Seq(LocalInstance(instArgIdx111)) =>
                              inside(arg12) {
                                case Simple(Var(loc12, LambdaInfo(lambdaInfo12, 2, typeTable12, insts12)), _) =>
                                  some(loc12) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("g"))))
                                  typeTable12.types should be ('empty)
                                  inside(insts12) {
                                    case Seq(LocalInstance(instArgIdx121)) =>
                                      List(instArgIdx111, instArgIdx121).toSet should have size(2)
                                  }
                              }
                          }
                      }
                  }
              }
              typeTable.types should be ('empty)
          }
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("h"))).flatMap(treeInfo.typeTable.types.get)) {
            case Some(InferredType(TupleType(Seq(TypeParamApp(param1, Seq(), 0), TypeParamApp(param2, Seq(), 0))), argKinds)) =>
              // \t1 t2 => (t1, t2)
              List(param1, param2).toSet should have size(2)
              inside(argKinds) {
                case Seq(
                    InferredKind(Star(KindType, _)) /* * */,
                    InferredKind(Star(KindType, _)) /* * */) =>
                  ()
              }
          }
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("h"))).flatMap(treeInfo.instArgTable.instArgs.get)) {
            case Some(Seq(instArg1, instArg2)) =>
              inside(instArg1) {
                case InstanceArg(polyFun1, type1) =>
                  some(polyFun1) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))).map { PolyFunction(_) })
                  inside(type1) {
                    case InferredType(TypeParamApp(param1, Seq(), 0), argKinds1) =>
                      inside(argKinds1) {
                        case Seq(
                            InferredKind(Star(KindType, _)) /* * */,
                            InferredKind(Star(KindType, _)) /* * */) =>
                          ()
                      }
                      inside(instArg2) {
                        case InstanceArg(polyFun2, type2) =>
                          some(polyFun2) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("g"))).map { PolyFunction(_) })
                          inside(type2) {
                            case InferredType(TypeParamApp(param2, Seq(), 0), argKinds2) =>
                              List(param1, param2).toSet should have size(2)
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
          // instances
          insts should be ('empty)
          selectConstructInsts should be ('empty)
          treeInfo.instTree should be ===(InstanceTree.empty)
      }
    }
    
    it should "transform the string with the construct-expression" in {
      val (typeEnv, res) = Instantiator.transformString("""
f x y = construct 2 x y
""")(NameTree.empty, InferredKindTable.empty, InferredTypeTable.empty, emptyInstTree, InstanceArgTable.empty)(f3)(g3).run(emptyTypeEnv)
      inside(res) {
        case Success(Tree(combs, treeInfo)) =>
          val combSyms = Set(GlobalSymbol(NonEmptyList("f")))
          val insts = instTreeInfoExtractor.instancesFromTreeInfo(treeInfo.treeInfo)
          val selectConstructInsts = instTreeInfoExtractor.selectConstructInstancesFromTreeInfo(treeInfo.treeInfo)
          val combLocs = combSyms.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo))
          combLocs should have size(1)
          treeInfo.instArgTable.instArgs.keySet should be ===(combLocs)
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))).flatMap(combs.get)) {
            case Some(Combinator(None, args, body, LambdaInfo(lambdaInfo, 0, typeTable, Seq()), _)) =>
              inside(args) { case List(Arg(Some("x"), None, _), Arg(Some("y"), None, _)) => () }
              inside(body) {
                case App(fun1, args1, _) =>
                  inside(fun1) {
                    case Simple(Construct(2, LambdaInfo(lambdaInfo1, 1, typeTable1, insts1)), _) =>
                      typeTable1.types should be ('empty)
                      inside(insts1) { case Seq(LocalInstance(0)) => () }
                  }
                  inside(args1) {
                    case NonEmptyList(arg11, arg12) =>
                      inside(arg11) {
                        case Simple(Var(loc11, LambdaInfo(lambdaInfo11, 2, typeTable11, Seq())), _) =>
                          some(loc11) should be ===(localSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("x")))
                          typeTable11.types should be ('empty)
                      }
                      inside(arg12) {
                        case Simple(Var(loc12, LambdaInfo(lambdaInfo12, 3, typeTable12, Seq())), _) =>
                          some(loc12) should be ===(localSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("y")))
                          typeTable12.types should be ('empty)
                      }
                  }
              }
          }
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))).flatMap(treeInfo.instArgTable.instArgs.get)) {
            case Some(Seq(instArg1)) =>
              inside(instArg1) {
                case InstanceArg(ConstructFunction, type1) =>
                  inside(type1) {
                    case InferredType(TypeConjunction(types1), argKinds1) =>
                      // \t1 t2 t3 => t1 #& (t2, t3)
                      inside(for {
                        x1 <- types1.collectFirst { case TypeParamApp(param11, Seq(), 0) => param11 }
                        x2 <- types1.collectFirst { case TupleType(Seq(TypeParamApp(param12, Seq(), 0), TypeParamApp(param13, Seq(), 0))) => (param12, param13) }
                      } yield (x1, x2)) {
                        case Some((param11, (param12, param13))) =>
                          List(param11, param12, param13).toSet should have size(3)
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
              // instances
              insts should be ('empty)
              selectConstructInsts should be ('empty)
          }
      }
    }
    
    it should "transform the string with the select-expression" is (pending)
    
    it should "transform the string with the instances" is (pending)
    
    it should "transform the string for the non-recursive combinators with the local instances" is (pending)
    
    it should "transform the string for the recursive combinators with the local instances" is (pending)
    
    it should "transform the string with the instances of the other tree" is (pending)
    
    it should "transform the string of the term with the instantiation" is (pending)
    
    it should "complain on the already defined instances" is (pending)
    
    it should "complain on the ambiguous instances" is (pending)
    
    it should "complain on the not found instances" is (pending)
  }
  
  "An Instantiator" should behave like instantiator(SymbolInstantiationEnvironment.empty[parser.LambdaInfo, parser.TypeLambdaInfo], SymbolTypeEnvironment.empty[TypeLambdaInfo[parser.TypeLambdaInfo, LocalSymbol]])(_ => ().successNel)(Instantiator.statefullyTransformToSymbolTree3)(Instantiator.statefullyMakeSymbolTypeInferenceEnvironment3)
}