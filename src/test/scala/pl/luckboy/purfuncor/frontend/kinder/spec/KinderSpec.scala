/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.kinder.spec
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import org.scalatest.FlatSpec
import org.scalatest.Inside
import org.scalatest.matchers.ShouldMatchers
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.kinder._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol
import pl.luckboy.purfuncor.frontend.resolver.NameTree
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbolTabular
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbolTabular
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.common.Arrow

class KinderSpec extends FlatSpec with ShouldMatchers with Inside
{
  def kinder[T, U, V, W, X, Y[_, _], Z, TT, TU, TV, E, D](emptyEnv: E, initData: D)(makeData: String => ValidationNel[AbstractError, D])(f2: D => Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, lmbdindexer.TypeLambdaInfo[X]]], Y[lmbdindexer.TypeLambdaInfo[X], Z]]])(g: InferredKindTable[TT] => E)(h2: D => Term[TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]] => ValidationNel[AbstractError, Term[TypeSimpleTerm[W, lmbdindexer.TypeLambdaInfo[X]]]])(implicit init: Initializer[NoKind, TT, AbstractTypeCombinator[W, lmbdindexer.TypeLambdaInfo[X]], E], inferrer: Inferrer[TypeSimpleTerm[W, lmbdindexer.TypeLambdaInfo[X]], E, Kind], envSt: KindInferenceEnvironmentState[E, TT], enval: KindInferenceEnvironmental[E, TT, TU], treeInfoTransformer: TreeInfoTransformer[Y, W, TT, TU], treeInfoExtractor: TreeInfoExtractor[Y[lmbdindexer.TypeLambdaInfo[X], Z], Tree[TT, AbstractTypeCombinator[W, lmbdindexer.TypeLambdaInfo[X]], Z]], treeInfoExtractor2: TreeInfoExtractor[Y[TypeLambdaInfo[X, TU], TypeTreeInfo[Z, TT]], Tree[TT, AbstractTypeCombinator[W, TypeLambdaInfo[X, TU]], TypeTreeInfo[Z, TT]]], instTreeInfoExtractor2: InstantiationTreeInfoExtractor[Y[TypeLambdaInfo[X, TU], TypeTreeInfo[Z, TT]], T, Instance[T], SelectConstructInstance[W, TypeLambdaInfo[X, TU]]], globalSymTabular: GlobalSymbolTabular[Y[TypeLambdaInfo[X, TU], TypeTreeInfo[Z, TT]], T], typeGlobalSymTabular: GlobalSymbolTabular[Z, TT], localSymTabular: LocalSymbolTabular[V, TV], typeLocalSymTabular: LocalSymbolTabular[X, TU])
  {
    //TODO: add a test for the global type contains the lambda-expression with the reference to itself.
    //TODO: add tests for the construct-expression and the select-expression and the extract-expression.
    //TODO: add a test for the type lambda-expression has the type arguments with the inferred kinds and returns a type with the inferred kind.
    //TODO: add a test for the mismatched kinds of the recursive dependent type combinators.
    val f = f2(initData)
    val h = h2(initData)
    
    it should "infer the kinds from the string" in {
      val (env, res) = Kinder.inferKindsFromTreeString("""
type T t = t
type U t u = t u
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      // T
      inside(enval.globalTypeVarKindFromEnvironment(env)(GlobalSymbol(NonEmptyList("T")))) {
        case InferredKind(Arrow(arg1, ret1, _)) =>
          // k1 -> k1
          inside(arg1) {
            case Star(KindParam(param1), _) =>
              inside(ret1) {
                case Star(KindParam(param2), _) =>
                  List(param1, param2).toSet should have size(1)
              }
          }
      }
      inside(enval.localKindTablesFromEnvironment(env)(some(GlobalSymbol(NonEmptyList("T")))).get(0)) {
        case Some(KindTable(kinds)) =>
          kinds should have size(1)
          inside(kinds.get(LocalSymbol("t"))) {
            case Some(InferredKind(Star(KindParam(_), _))) => ()
          }
      }
      // U
      inside(enval.globalTypeVarKindFromEnvironment(env)(GlobalSymbol(NonEmptyList("U")))) {
        case InferredKind(Arrow(arg1, Arrow(arg2, ret2, _), _)) =>
          // (k1 -> k2) -> k1 -> k2
          inside(arg1) {
            case Arrow(arg11, ret11, _) =>
              inside(arg11) {
                case Star(KindParam(param11), _) =>
                  inside(ret11) {
                    case Star(KindParam(param12), _) =>
                      inside(arg2) {
                        case Star(KindParam(param2), _) =>
                          inside(ret2) {
                            case Star(KindParam(param3), _) =>
                              List(param11, param2).toSet should have size(1)
                              List(param12, param3).toSet should have size(1)
                              List(param11, param12, param2, param3).toSet should have size(2)
                          }
                     }
                  }
              }
          }
      }
      inside(enval.localKindTablesFromEnvironment(env)(some(GlobalSymbol(NonEmptyList("U")))).get(0)) {
        case Some(KindTable(kinds)) =>
          kinds should have size(2)
          inside(kinds.get(LocalSymbol("t"))) {
            case Some(InferredKind(Arrow(arg1, ret1, _))) =>
              // k1 -> k2
              inside(arg1) {
                case Star(KindParam(param1), _) =>
                  inside(ret1) {
                    case Star(KindParam(param2), _) =>
                      List(param1, param2).toSet should have size(2)
                  }
              }
          }
          inside(kinds.get(LocalSymbol("u"))) {
            case Some(InferredKind(Star(KindParam(_), _))) => ()
          }
      }
    }

    it should "infer the kind from the string with the lambda-expression" in {
      val (env, res) = Kinder.inferKindsFromTreeString("type T t = (\\u v => v u) t")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalTypeVarKindFromEnvironment(env)(GlobalSymbol(NonEmptyList("T")))) {
        case InferredKind(Arrow(arg1, ret1, _)) =>
          // k1 -> (k1 -> k2) -> k2
          inside(arg1) {
            case Star(KindParam(param1), _) =>
              inside(ret1) {
                case Arrow(arg2, ret2, _) =>
                  inside(arg2) {
                    case Arrow(arg21, ret21, _) =>
                      inside(arg21) {
                        case Star(KindParam(param21), _) =>
                          inside(ret21) {
                            case Star(KindParam(param22), _) =>
                              inside(ret2) {
                                case Star(KindParam(param3), _) =>
                                  List(param1, param21).toSet should have size(1)
                                  List(param22, param3).toSet should have size(1)
                                  List(param1, param21, param22, param3).toSet should have size(2)
                              }
                          }
                      }
                  }
              }
          }
      }
      inside(enval.localKindTablesFromEnvironment(env)(some(GlobalSymbol(NonEmptyList("T")))).get(0)) {
        case Some(KindTable(kinds)) =>
          kinds should have size(1)
          inside(kinds.get(LocalSymbol("t"))) {
            case Some(InferredKind(Star(KindParam(_), _))) => ()
          }          
      }
      inside(enval.localKindTablesFromEnvironment(env)(some(GlobalSymbol(NonEmptyList("T")))).get(1)) {
        case Some(KindTable(kinds)) =>
          kinds should have size(2)
          inside(kinds.get(LocalSymbol("u"))) {
            case Some(InferredKind(Star(KindParam(_), _))) => ()
          }        
          inside(kinds.get(LocalSymbol("v"))) {
            case Some(InferredKind(Arrow(arg1, ret1, _))) =>
              // k1 -> k2
              inside(arg1) {
                case Star(KindParam(param1), _) =>
                  inside(ret1) {
                    case Star(KindParam(param2), _) =>
                      List(param1, param2).toSet should have size(2)
                  }
              }
          }
      }
    }
    
    it should "infer the kinds from the string with the built-in types" in {
      val (env, res) = Kinder.inferKindsFromTreeString("""
type T t u = u (#Array t)
type U t u v = ##-> t (##-> u v)
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      // T
      inside(enval.globalTypeVarKindFromEnvironment(env)(GlobalSymbol(NonEmptyList("T")))) {
        case InferredKind(Arrow(Star(KindType, _), ret1, _)) =>
          // * -> (* -> k1) -> k1
          inside(ret1) {
            case Arrow(arg11, Star(KindParam(param1), _), _) =>
              inside(arg11) {
                case Arrow(Star(KindType, _), Star(KindParam(param2), _), _) =>
                  List(param1, param2).toSet should have size(1)
              }
          }
      }
      // U
      inside(enval.globalTypeVarKindFromEnvironment(env)(GlobalSymbol(NonEmptyList("U")))) {
        case InferredKind(Arrow(Star(KindType, _), ret1, _)) =>
          // * -> * -> * -> *
          inside(ret1) {
            case Arrow(Star(KindType, _), ret2, _) =>
              inside(ret2) {
                case Arrow(Star(KindType, _), Star(KindType, _), _) => ()
              }
          }
      }
    }
    
    it should "infer the kind from the string with the nested lambda-expression" in {
      val (env, res) = Kinder.inferKindsFromTreeString("""
type T t1 = \t2 t3 => \t4 => ##& t1 (##| t2 (t4 t3))
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalTypeVarKindFromEnvironment(env)(GlobalSymbol(NonEmptyList("T")))) {
        case InferredKind(Arrow(Star(KindType, _), ret1, _)) =>
          // * -> * -> k1 -> (k1 -> *) -> *
          inside(ret1) {
            case Arrow(Star(KindType, _), ret2, _) =>
              inside(ret2) {
                case Arrow(Star(KindParam(param3), _), ret3, _) =>
                  inside(ret3) {
                    case Arrow(arg31, Star(KindType, _), _) =>
                      inside(arg31) {
                        case Arrow(Star(KindParam(param31), _), Star(KindType, _), _) =>
                          List(param3, param31).toSet should have size(1)
                      }
                  }
              }
          }
      }      
    }
    
    it should "infer the kind from the string with the covered local type variables" in {
      val (env, res) = Kinder.inferKindsFromTreeString("""
type T t1 (t2: *) = (\t3 t2 => t2 t3 t1) t2
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalTypeVarKindFromEnvironment(env)(GlobalSymbol(NonEmptyList("T")))) {
        case InferredKind(Arrow(Star(KindParam(param1), _), ret1, _)) =>
          // k1 -> * -> (* -> k1 -> k2) -> k2
          inside(ret1) {
            case Arrow(Star(KindType, _), ret2, _) =>
              inside(ret2) {
                case Arrow(arg31, Star(KindParam(param3), _), _) =>
                  inside(arg31) {
                    case Arrow(Star(KindType, _), ret31, _) =>
                      inside(ret31) {
                        case Arrow(Star(KindParam(param32), _), Star(KindParam(param33), _), _) =>
                          List(param1, param32).toSet should have size(1)
                          List(param33, param3).toSet should have size(1)
                          List(param1, param32, param33, param3).toSet should have size(2)
                      }
                  }
              }
          }
      }
    }
    
    it should "infer the kind for the inferred kind of the returned type" in {
      val (env, res) = Kinder.inferKindsFromTreeString("""
type T t1 t2 t3 = t3 t1 t2
type U t1 t2 = T (##& t1 #Int) t2
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalTypeVarKindFromEnvironment(env)(GlobalSymbol(NonEmptyList("U")))) {
        case InferredKind(Arrow(Star(KindType, _), ret1, _)) =>
          // * -> k1 -> (* -> k1 -> k2) -> k2
          inside(ret1) {
            case Arrow(Star(KindParam(param2), _), ret2, _) =>
              inside(ret2) {
                case Arrow(arg31, Star(KindParam(param3), _), _) =>
                  inside(arg31) {
                    case Arrow(Star(KindType, _), ret31, _) =>
                      inside(ret31) {
                        case Arrow(Star(KindParam(param31), _), Star(KindParam(param32), _), _) =>
                          List(param2, param31).toSet should have size(1)
                          List(param32, param3).toSet should have size(1)
                          List(param2, param31, param32, param3).toSet should have size(2)
                      }
                  }
              }
          }
      }
    }
    
    it should "initialize all kinds of the non-recursive dependent type combinators" in {
      val (env, res) = Kinder.inferKindsFromTreeString("""
type T t1 t2 = ##& (##| (U t1 t2) (V #NonZero)) t1
type U t1 t2 = t2 t1
type V t1 = ##& W t1
type W = #Int
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      // T
      inside(enval.globalTypeVarKindFromEnvironment(env)(GlobalSymbol(NonEmptyList("T")))) {
        case InferredKind(Arrow(Star(KindType, _), ret1, _)) =>
          // * -> (* -> *) -> *
          inside(ret1) {
            case Arrow(arg21, Star(KindType, _), _) =>
              inside(arg21) {
                case Arrow(Star(KindType, _), Star(KindType, _), _) => ()
              }
          }
      }
      // U
      inside(enval.globalTypeVarKindFromEnvironment(env)(GlobalSymbol(NonEmptyList("U")))) {
        case InferredKind(Arrow(Star(KindParam(param1), _), ret1, _)) =>
          // k1 -> (k1 -> k2) -> k2
          inside(ret1) {
            case Arrow(arg21, Star(KindParam(param2), _), _) =>
              inside(arg21) {
                case Arrow(Star(KindParam(param21), _), Star(KindParam(param22), _), _) =>
                  List(param1, param21).toSet should have size(1)
                  List(param22, param2).toSet should have size(1)
                  List(param1, param21, param22, param2).toSet should have size(2)
              }
          }
      }
      // V
      inside(enval.globalTypeVarKindFromEnvironment(env)(GlobalSymbol(NonEmptyList("V")))) {
        case InferredKind(Arrow(Star(KindType, _), Star(KindType, _), _)) =>
          // * -> *
          ()
      }
      // W
      inside(enval.globalTypeVarKindFromEnvironment(env)(GlobalSymbol(NonEmptyList("W")))) {
        case InferredKind(Star(KindType, _)) =>
          // *
          ()
      }
    }
    
    it should "initialize all kinds of the recursive dependent type combinators" in {
      val (env, res) = Kinder.inferKindsFromTreeString("""
type T t = T2 #Int t
type U t1 t2 = tuple 3 t1 t2 (V t1 t2)
type V t1 t2 = ##| (W t2) (##| (X t1 t2) (Y #Double Z))
type W t = tuple 2 #Char (T t)
type X t1 t2 = tuple 2 #Boolean (U t1 t2)
type Y t1 t2 = tuple 3 #Int (t2 t1) (Y t1 t2)
type Z t = #Int
type T2 = U
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalTypeVarKindFromEnvironment(env)(GlobalSymbol(NonEmptyList("T")))) {
        case InferredKind(Arrow(Star(KindType, _), Star(KindType, _), _)) =>
          // * -> *
          ()
      }
      inside(enval.globalTypeVarKindFromEnvironment(env)(GlobalSymbol(NonEmptyList("U")))) {
        case InferredKind(Arrow(Star(KindType, _), ret1, _)) =>
          // * -> * -> *
          inside(ret1) { case Arrow(Star(KindType, _), Star(KindType, _), _) => () }
      }
      inside(enval.globalTypeVarKindFromEnvironment(env)(GlobalSymbol(NonEmptyList("V")))) {
        case InferredKind(Arrow(Star(KindType, _), ret1, _)) =>
          // * -> * -> *
          inside(ret1) { case Arrow(Star(KindType, _), Star(KindType, _), _) => () }
      }
      inside(enval.globalTypeVarKindFromEnvironment(env)(GlobalSymbol(NonEmptyList("W")))) {
        case InferredKind(Arrow(Star(KindType, _), Star(KindType, _), _)) =>
          // * -> *
          ()
      }
      inside(enval.globalTypeVarKindFromEnvironment(env)(GlobalSymbol(NonEmptyList("X")))) {
        case InferredKind(Arrow(Star(KindType, _), ret1, _)) =>
          // * -> * -> *
          inside(ret1) { case Arrow(Star(KindType, _), Star(KindType, _), _) => () }
      }
      inside(enval.globalTypeVarKindFromEnvironment(env)(GlobalSymbol(NonEmptyList("Y")))) {
        case InferredKind(Arrow(Star(KindParam(param1), _), ret1, _)) =>
          // k1 -> (k1 -> *) -> *
          inside(ret1) {
            case Arrow(arg21, Star(KindType, _), _) =>
              inside(arg21) {
                case Arrow(Star(KindParam(param21), _), Star(KindType, _), _) =>
                  List(param1, param21).toSet should have size(1)
              }
          }
      }
      inside(enval.globalTypeVarKindFromEnvironment(env)(GlobalSymbol(NonEmptyList("Z")))) {
        case InferredKind(Arrow(Star(KindParam(_), _), Star(KindType, _), _)) =>
          // k1 -> *
          ()
      }
      inside(enval.globalTypeVarKindFromEnvironment(env)(GlobalSymbol(NonEmptyList("T2")))) {
        case InferredKind(Arrow(Star(KindType, _), ret1, _)) =>
          // * -> * -> *
          inside(ret1) { case Arrow(Star(KindType, _), Star(KindType, _), _) => () }
      }
    }
    
    it should "infer the kind for the defined kind of the type combinator" in {
      val (env, res) = Kinder.inferKindsFromTreeString("type (T: * -> (* -> *) -> *) t1 t2 = t2 t1")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalTypeVarKindFromEnvironment(env)(GlobalSymbol(NonEmptyList("T")))) {
        case InferredKind(Arrow(Star(KindType, _), ret1, _)) =>
          // * -> (* -> *) -> *
          inside(ret1) { 
            case Arrow(arg21, Star(KindType, _), _) =>
              inside(arg21) { case Arrow(Star(KindType, _), Star(KindType, _), _) => () }
          }
      }
    }
    
    it should "infer the kind for the defined kinds of the type arguments" in {
      val (env, res) = Kinder.inferKindsFromTreeString("""
type T (t1: k1 -> k2) t2 (t3: (k1 -> k2) -> * -> k3) = t3 t1 t2
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalTypeVarKindFromEnvironment(env)(GlobalSymbol(NonEmptyList("T")))) {
        case InferredKind(Arrow(arg11, ret1, _)) =>
          // (k1 -> k2) -> * -> ((k1 -> k2) -> * -> k3) -> k3
          inside(arg11) {
            case Arrow(Star(KindParam(param11), _), Star(KindParam(param12), _), _) =>
              inside(ret1) {
                case Arrow(Star(KindType, _), ret2, _) =>
                  inside(ret2) {
                    case Arrow(arg31, Star(KindParam(param3), _), _) =>
                      inside(arg31) {
                        case Arrow(arg41, ret31, _) =>
                          inside(arg41) {
                            case Arrow(Star(KindParam(param41), _), Star(KindParam(param42), _), _) =>
                              inside(ret31) {
                                case Arrow(Star(KindType, _), Star(KindParam(param31), _), _) =>
                                  List(param11, param41).toSet should have size(1)
                                  List(param12, param42).toSet should have size(1)
                                  List(param31, param3).toSet should have size(1)
                                  List(param11, param12, param41, param42, param31, param3).toSet should have size(3)
                              }
                          }
                      }
                  }
              }
          }
      }
    }
    
    it should "infer the kind for the defined kind of the type expression" in {
      val (env, res) = Kinder.inferKindsFromTreeString("type T t1 t2 = (t2: * -> k1) t1")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalTypeVarKindFromEnvironment(env)(GlobalSymbol(NonEmptyList("T")))) {
        case InferredKind(Arrow(Star(KindType, _), ret1, _)) =>
          // * -> (* -> k1) -> k1
          inside(ret1) {
            case Arrow(arg21, Star(KindParam(param2), _), _) =>
              inside(arg21) {
                case Arrow(Star(KindType, _), Star(KindParam(param21), _), _) =>
                  List(param2, param21).toSet should have size(1)
              }
          }
      }
    }
    
    it should "complain on the mismatched kinds" in {
      val (env, res) = Kinder.inferKindsFromTreeString("""
type T t1 t2 = tuple 2 (t2 t1) #Int
type U t1 t2 = ##& (T t1 t2) t2
type V t1 = T t1 ##->
""")(NameTree.empty)(f).run(emptyEnv)
      inside(res) {
        case Success(Failure(noKind)) =>
          noKind.errs.map { _.msg } should be ===(List(
              "couldn't match kind * with kind k1 -> *",
              "couldn't match kind k1 -> * with kind * -> * -> *"))
      }
    }
    
    it should "complain on the infinity kinds" in {
      val (env, res) = Kinder.inferKindsFromTreeString("""
type T t1 t2 t3 = tuple 3 (t1 t2) (t3 t1) (t3 t2)
""")(NameTree.empty)(f).run(emptyEnv)
      inside(res) {
        case Success(Failure(noKind)) =>
          noKind.errs.map { _.msg } should be ===(List("couldn't match kind k1 -> k2 with kind k1"))
      }      
    }
    
    it should "complain on the instantiation of the parameters of the defined kinds" in {
      val (env, res) = Kinder.inferKindsFromTreeString("""
type T t1 t2 t3 (t4: k1 -> * -> *) = tuple 3 t2 (t4 t1 t3) (t4 t2 t3)
""")(NameTree.empty)(f).run(emptyEnv)
      inside(res) {
        case Success(Failure(noKind)) =>
          noKind.errs.map { _.msg } should be ===(List("couldn't instantiate parameter at defined kind k1 -> * -> *"))
      }      
    }

    it should "complain on the distinct parameters in the defined kinds" in {
      val (env, res) = Kinder.inferKindsFromTreeString("""
type T t1 t2 (t3: k1 -> k2 -> *) (t4: k1 -> k2 -> k3 -> *) = tuple 3 (t3 t1 t1) (t3 t2 t2) (t4 t1 t2 t1)
""")(NameTree.empty)(f).run(emptyEnv)
      inside(res) {
        case Success(Failure(noKind)) =>
          noKind.errs.map { _.msg } should be ===(List(
              "parameters are distinct at defined kind k1 -> k2 -> *",
              "parameters are distinct at defined kind k1 -> k2 -> k3 -> *"))
      }
    }
    
    it should "complain on the uninferred kinds of the global type variables" in {
      val (env, res) = Kinder.inferKindsFromTreeString("""
type T t1 t2 = tuple 3 t1 (t2 t1) (t2 ##->)
type U = T
type V = U #Int
""")(NameTree.empty)(f).run(emptyEnv)      
      inside(res) {
        case Success(Failure(noKind)) =>
          noKind.errs.map { _.msg } should be ===(List(
              "couldn't match kind * with kind * -> * -> *",
              "uninferred kind of global type variable #.T",
              "uninferred kind of global type variable #.U"))
      }
    }
    
    it should "transform the string" in {
      val res = Kinder.transformString("type T = #Int; f x = #iAdd (x: T) x")(NameTree.empty, InferredKindTable.empty)(f)(g)
      inside(res) {
        case Success(Tree(combs, treeInfo)) =>
          val typeTree = treeInfoExtractor2.typeTreeFromTreeInfo(treeInfo)
          val typeCombs = typeTree.combs
          val typeTreeInfo = typeTree.treeInfo
          val combSyms = Set(GlobalSymbol(NonEmptyList("f")))
          val combLocs = combSyms.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfo))
          combLocs should have size(combSyms.size)
          combs.keySet should be ===(combLocs)
          val typeCombSyms = Set(GlobalSymbol(NonEmptyList("T")))
          val typeCombLocs = typeCombSyms.flatMap(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo))
          typeCombLocs should have size(typeCombSyms.size)
          typeCombs.keySet should be ===(typeCombLocs)
          typeTreeInfo.kindTable.kinds.keySet should be ===(typeCombLocs)
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo)(GlobalSymbol(NonEmptyList("f"))).flatMap(combs.get)) {
            case Some(Combinator(None, args, body, lmbdindexer.LambdaInfo(lambdaInfo, _), _)) =>
              inside(body) {
                case App(fun1, args1, _) =>
                  inside(fun1) { case Simple(Literal(BuiltinFunValue(BuiltinFunction.IAdd)), _) => () }
                  inside(args1) {
                    case NonEmptyList(arg11, arg12) =>
                      inside(arg11) { 
                        case Simple(TypedTerm(term11, typ11), _) =>
                          inside(term11) {
                            case Simple(Var(loc11, _), _) =>
                              some(loc11) should be ===(localSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("x")))
                          }
                          inside(typ11) {
                            case Simple(TypeVar(typLoc11), _) =>
                              some(typLoc11) should be ===(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("T"))))
                          }
                      }
                      inside(arg12) { 
                        case Simple(Var(loc12, _), _) =>
                          some(loc12) should be ===(localSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("x")))
                      }
                  }
              }
          }
          inside(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("T"))).flatMap(typeCombs.get)) {
            case Some(TypeCombinator(None, args, body, TypeLambdaInfo(lambdaInfo, 0, kindTable), _)) =>
              kindTable should be ===(InferredKindTable.empty)
              inside(body) {
                case Simple(TypeLiteral(TypeBuiltinFunValue(TypeBuiltinFunction.Int)), _) => ()
              }
          }
          inside(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("T"))).flatMap(typeTreeInfo.kindTable.kinds.get)) {
            case Some(InferredKind(Star(KindType, _))) => ()
          }
      }
    }
    
    it should "transform inferred kinds to global kind table" in {
      val res = Kinder.transformString("""
type T t1 t2 = t2 t1
type U = ##-> #Int
""")(NameTree.empty, InferredKindTable.empty)(f)(g)
      inside(res) {
        case Success(Tree(combs, treeInfo)) =>
          val typeTree = treeInfoExtractor2.typeTreeFromTreeInfo(treeInfo)
          val typeCombs = typeTree.combs
          val typeTreeInfo = typeTree.treeInfo
          combs.keySet should be ('empty)
          val typeCombSyms = Set(GlobalSymbol(NonEmptyList("T")), GlobalSymbol(NonEmptyList("U")))
          val typeCombLocs = typeCombSyms.flatMap(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo))
          typeCombLocs should have size(typeCombSyms.size)
          typeCombs.keySet should be ===(typeCombLocs)
          typeTreeInfo.kindTable.kinds.keySet should be ===(typeCombLocs)
          inside(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("T"))).flatMap(typeTreeInfo.kindTable.kinds.get)) {
            case Some(InferredKind(Arrow(Star(KindParam(param1), _), ret1, _))) =>
              // k1 -> (k1 -> k2) -> k2
              inside(ret1) {
                case Arrow(arg21, Star(KindParam(param2), _), _) =>
                  inside(arg21) {
                    case Arrow(Star(KindParam(param11), _), Star(KindParam(param12), _), _) =>
                      List(param1, param11).toSet should have size(1)
                      List(param12, param2).toSet should have size(1)
                      List(param1, param11, param12, param2).toSet should have size(2)
                  }
              }
          }
          inside(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("U"))).flatMap(typeTreeInfo.kindTable.kinds.get)) {
            case Some(InferredKind(Arrow(Star(KindType, _), Star(KindType, _), _))) =>
              // * -> *
              ()
          }
      }
    }
    
    it should "transform inferred kinds to local kind tables" in {
      val res = Kinder.transformString("""
type T t1 t2 = t2 t1
type U t1 = \t2 t3 => ##-> t1 (t3 t2)
""")(NameTree.empty, InferredKindTable.empty)(f)(g)
      inside(res) {
        case Success(Tree(combs, treeInfo)) =>
          val typeTree = treeInfoExtractor2.typeTreeFromTreeInfo(treeInfo)
          val typeCombs = typeTree.combs
          val typeTreeInfo = typeTree.treeInfo
          combs.keySet should be ('empty)
          val typeCombSyms = Set(GlobalSymbol(NonEmptyList("T")), GlobalSymbol(NonEmptyList("U")))
          val typeCombLocs = typeCombSyms.flatMap(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo))
          typeCombLocs should have size(typeCombSyms.size)
          typeCombs.keySet should be ===(typeCombLocs)
          typeTreeInfo.kindTable.kinds.keySet should be ===(typeCombLocs)
          inside(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("T"))).flatMap(typeCombs.get)) {
            case Some(TypeCombinator(None, _, _, TypeLambdaInfo(lambdaInfo, 0, kindTable), _)) =>
              val syms = Set(LocalSymbol("t1"), LocalSymbol("t2"))
              val locs = syms.flatMap(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo))
              locs should have size(syms.size)
              kindTable.kinds.keySet should be ===(locs)
              inside(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("t1")).flatMap(kindTable.kinds.get)) {
                case Some(InferredKind(Star(KindParam(_), _))) =>
                  // k1
                  ()
              }
              inside(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("t2")).flatMap(kindTable.kinds.get)) {
                case Some(InferredKind(Arrow(Star(KindParam(param1), _), Star(KindParam(param2), _), _))) =>
                  // k1 -> k2
                  List(param1, param2) should have size(2)
              }
          }
          inside(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("U"))).flatMap(typeCombs.get)) {
            case Some(TypeCombinator(None, _, body, TypeLambdaInfo(lambdaInfo, 0, kindTable), _)) =>
              val syms = Set(LocalSymbol("t1"))
              val locs = syms.flatMap(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo))
              inside(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("t1")).flatMap(kindTable.kinds.get)) {
                case Some(InferredKind(Star(KindType, _))) =>
                  // *
                  ()
              }
              inside(body) {
                case Simple(TypeLambda(_, _, TypeLambdaInfo(lambdaInfo1, 1, kindTable1)), _) =>
                  val syms1 = Set(LocalSymbol("t2"), LocalSymbol("t3"))
                  val locs1 = syms.flatMap(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo1))
                  inside(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo1)(LocalSymbol("t2")).flatMap(kindTable1.kinds.get)) {
                    case Some(InferredKind(Star(KindParam(_), _))) =>
                      // k1
                      ()
                  }
                  inside(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo1)(LocalSymbol("t3")).flatMap(kindTable1.kinds.get)) {
                    case Some(InferredKind(Arrow(Star(KindParam(_), _), Star(KindType, _), _))) =>
                      // k1 -> *
                      ()
                  }
              }
          }
      }      
    }
    
    it should "transform the string with the defined type of the combinator" in {
      val res = Kinder.transformString("""
type T = tuple 2
(f: \t1 t2 => ##-> t1 (##-> t2 (T t1 t2))) x y = tuple 2 x y
""")(NameTree.empty, InferredKindTable.empty)(f)(g)
      inside(res) {
        case Success(Tree(combs, treeInfo)) =>
          val typeTree = treeInfoExtractor2.typeTreeFromTreeInfo(treeInfo)
          val typeCombs = typeTree.combs
          val typeTreeInfo = typeTree.treeInfo
          val combSyms = Set(GlobalSymbol(NonEmptyList("f")))
          val combLocs = combSyms.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfo))
          combLocs should have size(combSyms.size)
          combs.keySet should be ===(combLocs)
          val typeCombSyms = Set(GlobalSymbol(NonEmptyList("T")))
          val typeCombLocs = typeCombSyms.flatMap(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo))
          typeCombLocs should have size(typeCombSyms.size)
          typeCombs.keySet should be ===(typeCombLocs)
          typeTreeInfo.kindTable.kinds.keySet should be ===(typeCombLocs)
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo)(GlobalSymbol(NonEmptyList("f"))).flatMap(combs.get)) {
            case Some(Combinator(Some(typ), _, _, _, _)) =>
              inside(typ) {
                case Simple(TypeLambda(typArgs, typBody, TypeLambdaInfo(lambdaInfo, 0, kindTable)), _) =>
                  inside(typArgs) { case NonEmptyList(TypeArg(Some("t1"), None, _), TypeArg(Some("t2"), None, _)) => () }
                  val typSyms = Set(LocalSymbol("t1"), LocalSymbol("t2"))
                  val typLocs = typSyms.flatMap(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo))
                  typLocs should have size(typSyms.size)
                  kindTable.kinds.keySet should be ===(typLocs)
                  inside(typBody) {
                    case App(typFun1, typArgs1, _) =>
                      inside(typFun1) { case Simple(TypeLiteral(TypeBuiltinFunValue(TypeBuiltinFunction.Fun)), _) => () }
                      inside(typArgs1) {
                        case NonEmptyList(typArg11, typArg12) =>
                          inside(typArg11) { 
                            case Simple(TypeVar(typLoc11), _) =>
                              some(typLoc11) should be ===(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("t1")))
                          }
                          inside(typArg12) {
                            case App(typFun2, typArgs2, _) =>
                              inside(typFun1) { case Simple(TypeLiteral(TypeBuiltinFunValue(TypeBuiltinFunction.Fun)), _) => () }
                              inside(typArgs2) {
                                case NonEmptyList(typArg21, typArg22) =>
                                  inside(typArg21) {
                                    case Simple(TypeVar(typLoc21), _) =>
                                      some(typLoc21) should be ===(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("t2")))
                                  }
                                  inside(typArg22) {
                                    case App(typFun3, typArgs3, _) =>
                                      inside(typFun3) { 
                                      	case Simple(TypeVar(typLoc3), _) =>
                                          some(typLoc3) should be ===(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("T"))))
                                      }
                                      inside(typArgs3) {
                                        case NonEmptyList(typArg31, typArg32) =>
                                          inside(typArg31) {
                                            case Simple(TypeVar(typLoc31), _) =>
                                              some(typLoc31) should be ===(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("t1")))
                                          }
                                          inside(typArg32) {
                                            case Simple(TypeVar(typLoc32), _) =>
                                              some(typLoc32) should be ===(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("t2")))
                                          }
                                      }
                                  }
                              }
                          }
                      }
                  }
                  inside(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("t1")).flatMap(kindTable.kinds.get)) {
                    case Some(InferredKind(Star(KindType, _))) =>
                      // *
                      ()
                  }
                  inside(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("t2")).flatMap(kindTable.kinds.get)) {
                    case Some(InferredKind(Star(KindType, _))) =>
                      // *
                      ()
                  }
              }
          }
      }
    }
    
    it should "transform the string with the defined type of the argument" in {
      val res = Kinder.transformString("""
type T = #Int
f x (y: \t => T) = #iSub x y
""")(NameTree.empty, InferredKindTable.empty)(f)(g)
      inside(res) {
        case Success(Tree(combs, treeInfo)) =>
          val typeTree = treeInfoExtractor2.typeTreeFromTreeInfo(treeInfo)
          val typeCombs = typeTree.combs
          val typeTreeInfo = typeTree.treeInfo
          val combSyms = Set(GlobalSymbol(NonEmptyList("f")))
          val combLocs = combSyms.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfo))
          combLocs should have size(combSyms.size)
          combs.keySet should be ===(combLocs)
          val typeCombSyms = Set(GlobalSymbol(NonEmptyList("T")))
          val typeCombLocs = typeCombSyms.flatMap(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo))
          typeCombLocs should have size(typeCombSyms.size)
          typeCombs.keySet should be ===(typeCombLocs)
          typeTreeInfo.kindTable.kinds.keySet should be ===(typeCombLocs)
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo)(GlobalSymbol(NonEmptyList("f"))).flatMap(combs.get)) {
            case Some(Combinator(None, args, _, _, _)) =>
              inside(args) {
                case List(_, Arg(_, Some(typ), _)) =>
                  inside(typ) {
                    case Simple(TypeLambda(typArgs, typBody, TypeLambdaInfo(lambdaInfo, 0, kindTable)), _) =>
                      inside(typArgs) { case NonEmptyList(TypeArg(Some("t"), None, _)) => () }
                      val typSyms = Set(LocalSymbol("t"))
                      val typLocs = typSyms.flatMap(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo))
                      typLocs should have size(typSyms.size)
                      kindTable.kinds.keySet should be ===(typLocs)
                      inside(typBody) {
                        case Simple(TypeVar(typLoc1), _) =>
                          some(typLoc1) should be ===(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("T"))))
                      }
                      inside(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("t")).flatMap(kindTable.kinds.get)) {
                        case Some(InferredKind(Star(KindParam(_), _))) =>
                          // k1
                          ()
                      }
                  }
              }
          }
      } 
    }
    
    it should "transform the string with the defined type of the expression" in {
      val res = Kinder.transformString("""
type T = #Int
f x y = tuple 2 (x: \t u => tuple 2 (u t) T) y
""")(NameTree.empty, InferredKindTable.empty)(f)(g)
      inside(res) {
        case Success(Tree(combs, treeInfo)) =>
          val typeTree = treeInfoExtractor2.typeTreeFromTreeInfo(treeInfo)
          val typeCombs = typeTree.combs
          val typeTreeInfo = typeTree.treeInfo
          val combSyms = Set(GlobalSymbol(NonEmptyList("f")))
          val combLocs = combSyms.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfo))
          combLocs should have size(combSyms.size)
          combs.keySet should be ===(combLocs)
          val typeCombSyms = Set(GlobalSymbol(NonEmptyList("T")))
          val typeCombLocs = typeCombSyms.flatMap(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo))
          typeCombLocs should have size(typeCombSyms.size)
          typeCombs.keySet should be ===(typeCombLocs)
          typeTreeInfo.kindTable.kinds.keySet should be ===(typeCombLocs)
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo)(GlobalSymbol(NonEmptyList("f"))).flatMap(combs.get)) {
            case Some(Combinator(None, _, body, _, _)) =>
              inside(body) {
                case App(_, args1, _) =>
                  inside(args1) {
                    case NonEmptyList(arg11, _) =>
                      inside(arg11) {
                        case Simple(TypedTerm(_, typ), _) =>
                          inside(typ) {
                            case Simple(TypeLambda(typArgs, typBody, TypeLambdaInfo(lambdaInfo, 0, kindTable)), _) =>
                              inside(typArgs) { case NonEmptyList(TypeArg(Some("t"), None, _), TypeArg(Some("u"), None, _)) => () }
                              val typSyms = Set(LocalSymbol("t"), LocalSymbol("u"))
                    		  val typLocs = typSyms.flatMap(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo))
                    		  typLocs should have size(typSyms.size)
                              kindTable.kinds.keySet should be ===(typLocs)
                              inside(typBody) {
                                case App(typFun1, typArgs1, _) =>
                                  inside(typFun1) { case Simple(TypeLiteral(TupleTypeFunValue(2)), _) => () }
                                  inside(typArgs1) {
                                    case NonEmptyList(typArg11, typArg12) =>
                                      inside(typArg11) {
                                        case App(typFun2, typArgs2, _) =>
                                          inside(typFun2) {
                                            case Simple(TypeVar(typLoc2), _) =>
                                              some(typLoc2) should be ===(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("u")))
                                          }
                                          inside(typArgs2) {
                                            case NonEmptyList(typArg21) =>
                                              inside(typArg21) {
                                                case Simple(TypeVar(typLoc21), _) =>
                                                  some(typLoc21) should be ===(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("t")))
                                              }
                                          }
                                      }
                                      inside(typArg12) {
                                        case Simple(TypeVar(typLoc12), _) =>
                                          some(typLoc12) should be ===(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("T"))))
                                      }
                                  }
                              }
                              inside(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("t")).flatMap(kindTable.kinds.get)) {
                                case Some(InferredKind(Star(KindParam(_), _))) =>
                                  // k1
                                  ()
                              }
                              inside(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("u")).flatMap(kindTable.kinds.get)) {
                                case Some(InferredKind(Arrow(Star(KindParam(_), _), Star(KindType, _), _))) =>
                                  // k1 -> *
                                  ()
                              }
                          }
                      }
                  }
              }
          }
      }      
    }
    
    it should "complain on transformation of the incorrect string for the error of the global type variable" in {
      val res = Kinder.transformString("""
type T t = tuple 2 t (t #Int)
f x = (x: T)
""")(NameTree.empty, InferredKindTable.empty)(f)(g)
      inside(res) {
        case Failure(errs) =>
          errs.map { _.msg } should be ===(NonEmptyList("couldn't match kind * with kind * -> k1"))
      }
    }

    it should "complain on transformation of the incorrect string for the errors of the defined types" in {
      val res = Kinder.transformString("""
f x = (x: \t => tuple 2 t (t #Int #Int))
g (x: \t => tuple 2 t (t #Int)) = x
""")(NameTree.empty, InferredKindTable.empty)(f)(g)      
      inside(res) {
        case Failure(errs) =>
          errs.map { _.msg } should be ===(NonEmptyList(
              "couldn't match kind * with kind * -> * -> k1",
              "couldn't match kind * with kind * -> k1"))
      }
    }
    
    it should "transform the string with the type references of the other tree" in {
      val s = "type T = #Int"
      val res = Kinder.transformString(s)(NameTree.empty, InferredKindTable.empty)(f)(g)
      val res2 = makeData(s)
      val nameTree = NameTree.empty |+| NameTree.fromTypeGlobalSymbol(GlobalSymbol(NonEmptyList("T")))
      inside((res |@| res2) { (t, d) => (t, d) }) {
        case Success((tree @ Tree(_, treeInfo), data)) =>
          val typeTree = treeInfoExtractor2.typeTreeFromTreeInfo(treeInfo)
          val res3 = Kinder.transformString("type U = T; f (x: T) = x")(nameTree, typeTree.treeInfo.kindTable)(f2(data))(g)
          inside(res3) {
            case Success(Tree(combs2, treeInfo2)) =>
              val typeTree2 = treeInfoExtractor2.typeTreeFromTreeInfo(treeInfo2)
              val typeCombs2 = typeTree2.combs
              val typeTreeInfo2 = typeTree2.treeInfo
              val combSyms2 = Set(GlobalSymbol(NonEmptyList("f")))
              val combLocs2 = combSyms2.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfo2))
              combLocs2 should have size(combSyms2.size)
              combs2.keySet should be ===(combLocs2)
              val typeCombSyms2 = Set(GlobalSymbol(NonEmptyList("U")))
              val typeCombLocs2 = typeCombSyms2.flatMap(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo2.treeInfo))
              typeCombLocs2 should have size(typeCombSyms2.size)
              typeCombs2.keySet should be ===(typeCombLocs2)
              typeTreeInfo2.kindTable.kinds.keySet should be ===(typeCombLocs2)
              inside(globalSymTabular.getGlobalLocationFromTable(treeInfo)(GlobalSymbol(NonEmptyList("f"))).flatMap(combs2.get)) {
                case Some(Combinator(None, args, body, lmbdindexer.LambdaInfo(lambdaInfo, _), _)) =>
                  inside(args) { 
                    case List(Arg(Some("x"), Some(typ), _)) =>
                      inside(typ) {
                        case Simple(TypeVar(typLoc1), _) =>
                          some(typLoc1) should be ===(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo2.treeInfo)(GlobalSymbol(NonEmptyList("T"))))
                      }
                  }
              }
              inside(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo2.treeInfo)(GlobalSymbol(NonEmptyList("U"))).flatMap(typeCombs2.get)) {
                case Some(TypeCombinator(None, Nil, body, TypeLambdaInfo(lambdaInfo, 0, kindTable), _)) =>
                  kindTable.kinds.keySet should be ('empty)
                  inside(body) {
                    case Simple(TypeVar(loc1), _) =>
                      some(loc1) should be ===(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo2.treeInfo)(GlobalSymbol(NonEmptyList("T"))))
                  }
              }
              inside(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo2.treeInfo)(GlobalSymbol(NonEmptyList("U"))).flatMap(typeTreeInfo2.kindTable.kinds.get)) {
                case Some(InferredKind(Star(KindType, _))) =>
                  // *
                  ()
              }
          }
      }
    }

    it should "transform the string with the unit type" in {
      val res = Kinder.transformString("unittype 3 T")(NameTree.empty, InferredKindTable.empty)(f)(g)
      inside(res) {
        case Success(Tree(combs, treeInfo)) =>
          val typeTree = treeInfoExtractor2.typeTreeFromTreeInfo(treeInfo)
          val typeCombs = typeTree.combs
          val typeTreeInfo = typeTree.treeInfo
          combs.keySet should be ('empty)
          val typeCombSyms = Set(GlobalSymbol(NonEmptyList("T")))
          val typeCombLocs = typeCombSyms.flatMap(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo))
          typeCombLocs should have size(typeCombSyms.size)
          typeCombs.keySet should be ===(typeCombLocs)
          typeTreeInfo.kindTable.kinds.keySet should be ===(typeCombLocs)
          inside(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("T"))).flatMap(typeCombs.get)) {
            case Some(UnittypeCombinator(3, None, _)) => ()
          }
          inside(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("T"))).flatMap(typeTreeInfo.kindTable.kinds.get)) {
            case Some(InferredKind(Arrow(Star(KindType, _), ret1, _))) => 
              // * -> * -> * -> *
              inside(ret1) {
                case Arrow(Star(KindType, _), ret2, _) =>
                  inside(ret2) {
                    case Arrow(Star(KindType, _), Star(KindType, _), _) => ()
                  }
              }
          }
      }
    }
    
    it should "transform the string of the type term with the kind inference" in {
      val res = Kinder.transformTypeTermStringWithKindInference("(\\t u => ##& t u) #Int")(NameTree.empty, emptyEnv)(h)
      inside(res) {
        case Success((typeTerm, kind)) =>
          inside(typeTerm) {
            case App(fun1, args1, _) =>
              inside(fun1) {
                case Simple(TypeLambda(args2, body2, TypeLambdaInfo(lambdaInfo2, 0, kindTable2)), _) =>
                  inside(args2) { case NonEmptyList(TypeArg(Some("t"), None, _), TypeArg(Some("u"), None, _)) => () }
                  val syms2 = Set(LocalSymbol("t"), LocalSymbol("u"))
                  val locs2 = syms2.flatMap(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo2))
                  locs2 should have size(syms2.size)
                  kindTable2.kinds.keySet should be ===(locs2)
                  inside(body2) {
                    case App(fun3, args3, _) =>
                      inside(fun3) { case Simple(TypeLiteral(TypeBuiltinFunValue(TypeBuiltinFunction.Conj)), _) => () }
                      inside(args3) {
                        case NonEmptyList(arg31, arg32) =>
                          inside(arg31) {
                            case Simple(TypeVar(loc31), _) =>
                    	      some(loc31) should be ===(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo2)(LocalSymbol("t")))
                          }
                          inside(arg32) {
                            case Simple(TypeVar(loc32), _) =>
                    	      some(loc32) should be ===(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo2)(LocalSymbol("u")))
                          }
                      }
                  }
              }
              inside(args1) {
                case NonEmptyList(arg11) =>
                  inside(arg11) { case Simple(TypeLiteral(TypeBuiltinFunValue(TypeBuiltinFunction.Int)), _) => () }
              }
          }
          inside(kind) {
            case InferredKind(Arrow(Star(KindType, _), Star(KindType, _), _)) =>
              // * -> *
              ()
          }
      }
    }
    
    it should "transform the string of the type term with the kind inference and the global type variables" in {
      val s = "type T = #Int; type U = #NonZero"
      val res = Kinder.transformString(s)(NameTree.empty, InferredKindTable.empty)(f)(g)
      val res2 = makeData(s)
      inside((res |@| res2) { (t, d) => (t, d) }) {
        case Success((Tree(_, treeInfo), data)) =>
          val typeTree = treeInfoExtractor2.typeTreeFromTreeInfo(treeInfo)
          val typeTreeInfo = typeTree.treeInfo
          val env = g(typeTree.treeInfo.kindTable)
          val nameTree = NameTree.empty |+| NameTree.fromTypeGlobalSymbols(Seq(GlobalSymbol(NonEmptyList("T")), GlobalSymbol(NonEmptyList("U"))))
          val res3 = Kinder.transformTypeTermStringWithKindInference("##& T U")(nameTree, env)(h2(data))
          inside(res3) {
            case Success((typeTerm, kind)) =>
              inside(typeTerm) {
                case App(fun, args, _) =>
                  inside(fun) { case Simple(TypeLiteral(TypeBuiltinFunValue(TypeBuiltinFunction.Conj)), _) => () }
                  inside(args) {
                    case NonEmptyList(arg1, arg2) =>
                      inside(arg1) { 
                        case Simple(TypeVar(loc1), _) =>
                          some(loc1) should be ===(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("T"))))
                      }
                      inside(arg2) { 
                        case Simple(TypeVar(loc2), _) =>
                          some(loc2) should be ===(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("U"))))
                      }
                  }
              }
          }
      }
    }
    
    it should "transform the string with the instances" in {
      val res = Kinder.transformString("""
poly (f: \t1 t2 => tuple 2 t1 t2)
instance f => g
g = tuple 2 1 2L
unittype 0 T
unittype 2 U
unittype 1 V
instance select \t1 t2 => ##| (##| (##& T tuple 0) (##& (U t1 t2) (tuple 2 t1 t2))) (##& (V t1) (tuple 1 t1)) construct {
  ##& T tuple 0
  \t1 t2 => ##& (U t1 t2) (tuple 2 t1 t2)
  \t1 => ##& (V t1) (tuple 1 t1)
}
""")(NameTree.empty, InferredKindTable.empty)(f)(g)
      inside(res) {
        case Success(Tree(combs, treeInfo)) =>
          val typeTree = treeInfoExtractor2.typeTreeFromTreeInfo(treeInfo)
          val typeCombs = typeTree.combs
          val typeTreeInfo = typeTree.treeInfo
          val insts = instTreeInfoExtractor2.instancesFromTreeInfo(treeInfo)
          val selectConstructInsts = instTreeInfoExtractor2.selectConstructInstancesFromTreeInfo(treeInfo)
          val combSyms = Set(GlobalSymbol(NonEmptyList("f")), GlobalSymbol(NonEmptyList("g")))
          val combLocs = combSyms.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfo))
          combLocs should have size(combSyms.size)
          combs.keySet should be ===(combLocs)
          val typeCombSyms = Set(
              GlobalSymbol(NonEmptyList("T")),
              GlobalSymbol(NonEmptyList("U")),
              GlobalSymbol(NonEmptyList("V")))
          val typeCombLocs = typeCombSyms.flatMap(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo))
          typeCombLocs should have size(typeCombSyms.size)
          typeCombs.keySet should be ===(typeCombLocs)
          typeTreeInfo.kindTable.kinds.keySet should be ===(typeCombLocs)
          val instSyms = Set(GlobalSymbol(NonEmptyList("f")))
          val instLocs = instSyms.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfo))
          instLocs should have size(instSyms.size)
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo)(GlobalSymbol(NonEmptyList("f"))).flatMap(combs.get)) {
            case Some(PolyCombinator(Some(typ), _)) =>
              inside(typ) {
                case Simple(TypeLambda(typArgs, typBody, TypeLambdaInfo(lambdaInfo, 0, kindTable)), _) =>
                  inside(typArgs) { case NonEmptyList(TypeArg(Some("t1"), None, _), TypeArg(Some("t2"), None, _)) => () }
                  val typSyms = Set(LocalSymbol("t1"), LocalSymbol("t2"))
                  val typLocs = typSyms.flatMap(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo))
                  typLocs should have size(typSyms.size)
                  inside(typBody) {
                    case App(typFun1, typArgs1, _) =>
                      inside(typFun1) { case Simple(TypeLiteral(TupleTypeFunValue(2)), _) => () }
                      inside(typArgs1) {
                        case NonEmptyList(arg11, arg12) =>
                          inside(arg11) { 
                            case Simple(TypeVar(typLoc11), _) =>
                              some(typLoc11) should be ===(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("t1")))
                          }
                          inside(arg12) {
                            case Simple(TypeVar(typLoc12), _) =>
                              some(typLoc12) should be ===(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("t2")))
                          }
                      }
                  }
                  inside(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("t1")).flatMap(kindTable.kinds.get)) {
                    case Some(InferredKind(Star(KindType, _))) =>
                      // *
                      ()
                  }
                  inside(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("t2")).flatMap(kindTable.kinds.get)) {
                    case Some(InferredKind(Star(KindType, _))) =>
                      // *
                      ()
                  }
              }
          }
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo)(GlobalSymbol(NonEmptyList("g"))).flatMap(combs.get)) {
            case Some(Combinator(None, Nil, body, lambdaInfo, _)) =>
              inside(body) {
                case App(fun1, args1, _) =>
                  inside(fun1) { case Simple(Literal(TupleFunValue(2)), _) => () }
                  inside(args1) {
                    case NonEmptyList(arg11, arg12) =>
                      inside(arg11) { case Simple(Literal(IntValue(1)), _) => () }
                      inside(arg12) { case Simple(Literal(LongValue(2L)), _) => () }
                  }
              }
          }
          inside(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("T"))).flatMap(typeCombs.get)) {
            case Some(UnittypeCombinator(0, None, _)) => ()
          }
          inside(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("U"))).flatMap(typeCombs.get)) {
            case Some(UnittypeCombinator(2, None, _)) => ()
          }
          inside(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("V"))).flatMap(typeCombs.get)) {
            case Some(UnittypeCombinator(1, None, _)) => ()
          }
          inside(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("T"))).flatMap(typeTreeInfo.kindTable.kinds.get)) {
            case Some(InferredKind(Star(KindType, _))) => 
              // *
              ()
          }
          inside(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("U"))).flatMap(typeTreeInfo.kindTable.kinds.get)) {
            case Some(InferredKind(Arrow(Star(KindType, _), ret1, _))) => 
              // * -> * -> *
              inside(ret1) {
                case Arrow(Star(KindType, _), Star(KindType, _), _) =>
                  ()
              }
          }
          inside(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("V"))).flatMap(typeTreeInfo.kindTable.kinds.get)) {
            case Some(InferredKind(Arrow(Star(KindType, _), Star(KindType, _), _))) => 
              // * -> *
              ()
          }
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo)(GlobalSymbol(NonEmptyList("f"))).flatMap(insts.get)) {
            case Some(List(Instance(instCombLoc, _, _))) =>
              some(instCombLoc) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo)(GlobalSymbol(NonEmptyList("g"))))
          }
          inside(selectConstructInsts) {
            case List(SelectConstructInstance(supertype, types, _)) =>
              inside(supertype) {
                case Simple(TypeLambda(supertypeArgs, supertypeBody, TypeLambdaInfo(lambdaInfo, 0, kindTable)), _) =>
                  inside(supertypeArgs) { case NonEmptyList(TypeArg(Some("t1"), None, _), TypeArg(Some("t2"), None, _)) => () }
                  val supertypeSyms = Set(LocalSymbol("t1"), LocalSymbol("t2"))
                  val supertypeLocs = supertypeSyms.flatMap(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo))
                  supertypeLocs should have size(supertypeSyms.size)
                  inside(supertypeBody) {
                    case App(supertypeFun1, supertypeArgs1, _) =>
                      inside(supertypeFun1) { case Simple(TypeLiteral(TypeBuiltinFunValue(TypeBuiltinFunction.Disj)), _) => () }
                      inside(supertypeArgs1) {
                        case NonEmptyList(supertypeArg11, supertypeArg12) =>
                          inside(supertypeArg11) {
                            case App(supertypeFun2, supertypeArgs2, _) =>
                              inside(supertypeFun2) { case Simple(TypeLiteral(TypeBuiltinFunValue(TypeBuiltinFunction.Disj)), _) => () }
                              inside(supertypeArgs2) {
                                case NonEmptyList(supertypeArgs21, supertypeArgs22) =>
                                  inside(supertypeArgs21) {
                                    case App(supertypeFun3, supertypeArgs3, _) =>
                                      inside(supertypeFun3) { case Simple(TypeLiteral(TypeBuiltinFunValue(TypeBuiltinFunction.Conj)), _) => () }
                                      inside(supertypeArgs3) {
                                        case NonEmptyList(supertypeArg31, supertypeArg32) =>
                                          inside(supertypeArg31) {
                                           case Simple(TypeVar(supertypeLoc31), _) =>
                                             some(supertypeLoc31) should be ===(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("T"))))
                                          }
                                          inside(supertypeArg32) { case Simple(TypeLiteral(TupleTypeFunValue(0)), _) => () }
                                      }
                                  }
                                  inside(supertypeArgs22) {
                                    case App(supertypeFun4, supertypeArgs4, _) =>
                                      inside(supertypeFun4) { case Simple(TypeLiteral(TypeBuiltinFunValue(TypeBuiltinFunction.Conj)), _) => () }
                                      inside(supertypeArgs4) {
                                        case NonEmptyList(supertypeArg41, supertypeArg42) =>
                                          inside(supertypeArg41) {
                                            case App(supertypeFun5, supertypeArgs5, _) =>
                                              inside(supertypeFun5) {
                                                case Simple(TypeVar(supertypeLoc5), _) =>
                                                  some(supertypeLoc5) should be ===(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("U"))))
                                              }
                                              inside(supertypeArgs5) {
                                                case NonEmptyList(supertypeArg51, supertypeArg52) =>
                                                  inside(supertypeArg51) {
                                                    case Simple(TypeVar(supertypeLoc51), _) =>
                                                      some(supertypeLoc51) should be ===(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("t1")))
                                                  }
                                                  inside(supertypeArg52) {
                                                    case Simple(TypeVar(supertypeLoc52), _) =>
                                                      some(supertypeLoc52) should be ===(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("t2")))
                                                  }
                                              }
                                          }
                                          inside(supertypeArg42) {
                                            case App(supertypeFun6, supertypeArgs6, _) =>
                                              inside(supertypeFun6) { case Simple(TypeLiteral(TupleTypeFunValue(2)), _) => () }
                                              inside(supertypeArgs6) {
                                                case NonEmptyList(supertypeArg61, supertypeArg62) =>
                                                  inside(supertypeArg61) {
                                                    case Simple(TypeVar(supertypeLoc61), _) =>
                                                      some(supertypeLoc61) should be ===(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("t1")))
                                                  }
                                                  inside(supertypeArg62) {
                                                    case Simple(TypeVar(supertypeLoc62), _) =>
                                                      some(supertypeLoc62) should be ===(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("t2")))
                                                  }
                                              }
                                          }
                                      }
                                  }
                              }
                          }
                          inside(supertypeArg12) {
                            case App(supertypeFun7, supertypeArgs7, _) =>
                              inside(supertypeFun7) { case Simple(TypeLiteral(TypeBuiltinFunValue(TypeBuiltinFunction.Conj)), _) => () }
                              inside(supertypeArgs7) {
                                case NonEmptyList(supertypeArg71, supertypeArg72) =>
                                  inside(supertypeArg71) {
                                    case App(supertypeFun8, supertypeArgs8, _) =>
                                      inside(supertypeFun8) {
                                        case Simple(TypeVar(supertypeLoc8), _) =>
                                          some(supertypeLoc8) should be ===(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("V"))))
                                      }
                                      inside(supertypeArgs8) {
                                        case NonEmptyList(supertypeArg81) =>
                                          inside(supertypeArg81) {
                                            case Simple(TypeVar(supertypeLoc81), _) =>
                                              some(supertypeLoc81) should be ===(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("t1")))
                                          }
                                      }
                                  }
                                  inside(supertypeArg72) {
                                    case App(supertypeFun9, supertypeArgs9, _) =>
                                      inside(supertypeFun9) { case Simple(TypeLiteral(TupleTypeFunValue(1)), _) => () }
                                      inside(supertypeArgs9) {
                                        case NonEmptyList(supertypeArg91) =>
                                          inside(supertypeArg91) {
                                            case Simple(TypeVar(supertypeLoc91), _) =>
                                              some(supertypeLoc91) should be ===(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("t1")))
                                          }
                                      }
                                  }
                              }
                          }
                      }
                  }
                  inside(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("t1")).flatMap(kindTable.kinds.get)) {
                    case Some(InferredKind(Star(KindType, _))) =>
                      // *
                      ()
                  }
                  inside(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("t2")).flatMap(kindTable.kinds.get)) {
                    case Some(InferredKind(Star(KindType, _))) =>
                      // *
                      ()
                  }
              }
              inside(types) {
                case NonEmptyList(typ1, typ2, typ3) =>
                  inside(typ1) {
                    case App(typFun11, typArgs11, _) =>
                      inside(typFun11) { case Simple(TypeLiteral(TypeBuiltinFunValue(TypeBuiltinFunction.Conj)), _) => () }
                      inside(typArgs11) {
                        case NonEmptyList(typArg111, typArg112) =>
                          inside(typArg111) {
                            case Simple(TypeVar(typLoc1), _) =>
                              some(typLoc1) should be ===(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("T"))))
                          }
                          inside(typArg112) { case Simple(TypeLiteral(TupleTypeFunValue(0)), _) => () }
                      }
                  }
                  inside(typ2) {
                    case Simple(TypeLambda(typArgs2, typBody2, TypeLambdaInfo(lambdaInfo2, 1, kindTable2)), _) =>
                      val typSyms2 = Set(LocalSymbol("t1"), LocalSymbol("t2"))
                      val typLocs2 = typSyms2.flatMap(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo2))
                      typLocs2 should have size(typSyms2.size)
                      inside(typBody2) {
                        case App(typFun21, typArgs21, _) =>
                          inside(typFun21) { case Simple(TypeLiteral(TypeBuiltinFunValue(TypeBuiltinFunction.Conj)), _) => () }
                          inside(typArgs21) {
                            case NonEmptyList(typArg211, typArg212) =>
                              inside(typArg211) {
                                case App(typFun22, typArgs22, _) =>
                                  inside(typFun22) {
                                    case Simple(TypeVar(typLoc22), _) =>
                                      some(typLoc22) should be ===(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("U"))))
                                  }
                                  inside(typArgs22) {
                                    case NonEmptyList(typArg221, typArg222) =>
                                      inside(typArg221) {
                                        case Simple(TypeVar(typLoc221), _) =>
                                          some(typLoc221) should be ===(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo2)(LocalSymbol("t1")))
                                      }
                                      inside(typArg222) {
                                        case Simple(TypeVar(typLoc222), _) =>
                                          some(typLoc222) should be ===(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo2)(LocalSymbol("t2")))
                                      }
                                  }
                              }
                              inside(typArg212) {
                                case App(typFun23, typArgs23, _) =>
                                  inside(typFun23) { case Simple(TypeLiteral(TupleTypeFunValue(2)), _) => () }
                                  inside(typArgs23) {
                                    case NonEmptyList(typArg231, typArg232) =>
                                      inside(typArg231) {
                                        case Simple(TypeVar(typLoc231), _) =>
                                          some(typLoc231) should be ===(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo2)(LocalSymbol("t1")))
                                      }
                                      inside(typArg232) {
                                        case Simple(TypeVar(typLoc232), _) =>
                                          some(typLoc232) should be ===(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo2)(LocalSymbol("t2")))
                                      }
                                  }
                              }
                          }
                      }
                      inside(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo2)(LocalSymbol("t1")).flatMap(kindTable2.kinds.get)) {
                        case Some(InferredKind(Star(KindType, _))) =>
                          // *
                          ()
                      }
                      inside(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo2)(LocalSymbol("t2")).flatMap(kindTable2.kinds.get)) {
                        case Some(InferredKind(Star(KindType, _))) =>
                          // *
                          ()
                      }
                  }
                  inside(typ3) {
                    case Simple(TypeLambda(typArgs3, typBody3, TypeLambdaInfo(lambdaInfo3, 2, kindTable3)), _) =>
                      val typSyms3 = Set(LocalSymbol("t1"))
                      val typLocs3 = typSyms3.flatMap(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo3))
                      typLocs3 should have size(typSyms3.size)
                      inside(typBody3) {
                        case App(typFun31, typArgs31, _) =>
                          inside(typFun31) { case Simple(TypeLiteral(TypeBuiltinFunValue(TypeBuiltinFunction.Conj)), _) => () }
                          inside(typArgs31) {
                            case NonEmptyList(typArg311, typArg312) =>
                              inside(typArg311) {
                                case App(typFun32, typArgs32, _) =>
                                  inside(typFun32) {
                                    case Simple(TypeVar(typLoc32), _) =>
                                      some(typLoc32) should be ===(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("V"))))
                                  }
                                  inside(typArgs32) {
                                    case NonEmptyList(typArg321) =>
                                      inside(typArg321) {
                                        case Simple(TypeVar(typLoc321), _) =>
                                          some(typLoc321) should be ===(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo3)(LocalSymbol("t1")))
                                      }
                                  }
                              }
                              inside(typArg312) {
                                case App(typFun33, typArgs33, _) =>
                                  inside(typFun33) { case Simple(TypeLiteral(TupleTypeFunValue(1)), _) => () }
                                  inside(typArgs33) {
                                    case NonEmptyList(typArg331) =>
                                      inside(typArg331) {
                                        case Simple(TypeVar(typLoc331), _) =>
                                          some(typLoc331) should be ===(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo3)(LocalSymbol("t1")))
                                      }
                                  }
                              }
                          }
                      }
                      inside(typeLocalSymTabular.getLocalLocationFromTable(lambdaInfo3)(LocalSymbol("t1")).flatMap(kindTable3.kinds.get)) {
                        case Some(InferredKind(Star(KindType, _))) =>
                          // *
                          ()
                      }
                  }
              }
          }
      }
    }
  }
  
  "A Kinder" should behave like kinder(SymbolKindInferenceEnvironment.empty[parser.TypeLambdaInfo], ())(_ => ().successNel)(_ => Kinder.transformToSymbolTree)(SymbolKindInferenceEnvironment.fromInferredKindTable)(_ => Kinder.transformToSymbolTypeTerm)
}
