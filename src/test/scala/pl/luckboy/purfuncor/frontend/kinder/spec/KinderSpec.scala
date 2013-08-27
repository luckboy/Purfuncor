package pl.luckboy.purfuncor.frontend.kinder.spec
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
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.common.Arrow

class KinderSpec extends FlatSpec with ShouldMatchers with Inside
{
  def kinder[T, U, V, W[_, _], X, Y, E](emptyEnv: E)(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, lmbdindexer.LambdaInfo, TypeSimpleTerm[Symbol, lmbdindexer.TypeLambdaInfo]], resolver.TreeInfo[lmbdindexer.TypeLambdaInfo, resolver.TypeTreeInfo]] => ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, lmbdindexer.LambdaInfo, TypeSimpleTerm[V, lmbdindexer.TypeLambdaInfo]], W[lmbdindexer.TypeLambdaInfo, resolver.TypeTreeInfo]]])(g: InferredKindTable[T] => E)(implicit init: Initializer[NoKind, X, AbstractTypeCombinator[V, lmbdindexer.TypeLambdaInfo], E], inferrer: Inferrer[TypeSimpleTerm[V, lmbdindexer.TypeLambdaInfo], E, Kind], envSt: KindInferenceEnvironmentState[E, X], enval: KindInferenceEnvironmental[E, X, Y], treeInfoTransformer: TreeInfoTransformer[W, X, Y], treeInfoExtractor: TreeInfoExtractor[W[lmbdindexer.TypeLambdaInfo, resolver.TypeTreeInfo], V, X])
  {
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
    
    it should "infer the kind from the string with the convered local type variables" in {
      val (env, res) = Kinder.inferKindsFromTreeString("""
type T t1 t2 = (\t3 t2 => t2 t3 t1) t2
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalTypeVarKindFromEnvironment(env)(GlobalSymbol(NonEmptyList("T")))) {
        case InferredKind(Arrow(Star(KindParam(param1), _), ret1, _)) =>
          // k1 -> k2 -> (k2 -> k1 -> k3) -> k3
          inside(ret1) {
            case Arrow(Star(KindParam(param2), _), ret2, _) =>
              inside(ret2) {
                case Arrow(arg31, Star(KindParam(param3), _), _) =>
                  inside(arg31) {
                    case Arrow(Star(KindParam(param31), _), ret31, _) =>
                      inside(ret31) {
                        case Arrow(Star(KindParam(param32), _), Star(KindParam(param33), _), _) =>
                          List(param1, param32).toSet should have size(1)
                          List(param2, param31).toSet should have size(1)
                          List(param33, param3).toSet should have size(1)
                          List(param1, param2, param31, param32, param33, param3).toSet should have size(3)
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
    
    it should "initialize all kinds of the recursive dependent type combinators" is (pending)
    
    it should "infer the kind for the defined kind of the type combinator" is (pending)
    
    it should "infer the kind for the defined kind of the type arguments" is (pending)
    
    it should "infer the kind for the defined kind of the type expression" is (pending)
  }
  
  "A Kinder" should behave like kinder(SymbolKindInferenceEnvironment.empty)(_.successNel)(SymbolKindInferenceEnvironment.fromInferredKindTable)
}