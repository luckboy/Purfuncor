package pl.luckboy.purfuncor.frontend.lmbdindexer.spec
import scalaz._
import scalaz.Scalaz._
import org.scalatest.FlatSpec
import org.scalatest.Inside
import org.scalatest.matchers.ShouldMatchers
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.lmbdindexer._
import pl.luckboy.purfuncor.frontend.resolver._
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.frontend.Bind

class LambdaIndexerSpec extends FlatSpec with ShouldMatchers with Inside
{
  //TODO: add tests for the construct-expression and the select-expression and the extract-expression.
  
  "A LambdaIndexer" should "transform the string" in {
    val res = LambdaIndexer.transformString("""
f x y = \z => let a = 1 in #iAdd a z
type T t1 = \t2 => \t3 => ##& (##| t1 t2) t3 
""")(NameTree.empty)
    inside(res) {
      case Success(Tree(combs, resolver.TreeInfo(Tree(typeCombs, typeTreeInfo)))) =>
        combs.keySet should be ===(Set(GlobalSymbol(NonEmptyList("f"))))
        typeCombs.keySet should be ===(Set(GlobalSymbol(NonEmptyList("T"))))
        inside(combs.get(GlobalSymbol(NonEmptyList("f")))) {
          case Some(Combinator(None, args, body, LambdaInfo(parser.LambdaInfo, idx), _)) =>
            inside(args) { case List(Arg(Some("x"), None, _), Arg(Some("y"), None, _)) => () }
            inside(body) {
              case Simple(Lambda(args1, body1, LambdaInfo(parser.LambdaInfo, idx1)), _) =>
                inside(args1) { case NonEmptyList(Arg(Some("z"), None, _)) => () }
                inside(body1) {
                  case Simple(Let(binds2, body2, LambdaInfo(parser.LambdaInfo, idx2)), _) =>
                    inside(binds2) { case NonEmptyList(Bind("a", Simple(Literal(IntValue(1)), _), _)) => () }
                    inside(body2) {
                      case App(fun3, args3, _) =>
                        inside(fun3) { case Simple(Literal(BuiltinFunValue(BuiltinFunction.IAdd)), _) => () }
                        inside(args3) { 
                          case NonEmptyList(Simple(Var(LocalSymbol("a"), LambdaInfo(parser.LambdaInfo, idx31)), _), Simple(Var(LocalSymbol("z"), LambdaInfo(parser.LambdaInfo, idx32)), _)) => ()
                            List(idx, idx1, idx2, idx31, idx32).toSet should be ===((0 to 4).toSet)
                        }
                    }
                }
            }
        }
        inside(typeCombs.get(GlobalSymbol(NonEmptyList("T")))) {
          case Some(TypeCombinator(None, args, body, TypeLambdaInfo(parser.TypeLambdaInfo, idx), _)) =>
            inside(args) { case List(TypeArg(Some("t1"), None, _)) => () }
            inside(body) {
              case Simple(TypeLambda(args1, body1, TypeLambdaInfo(parser.TypeLambdaInfo, idx1)), _) =>
                inside(args1) { case NonEmptyList(TypeArg(Some("t2"), None, _)) => () }
                inside(body1) {
                  case Simple(TypeLambda(args2, body2, TypeLambdaInfo(parser.TypeLambdaInfo, idx2)), _) =>
                    inside(args2) { case NonEmptyList(TypeArg(Some("t3"), None, _)) => ()}
                    inside(body2) {
                      case App(fun2, args2, _) =>
                        inside(fun2) { case Simple(TypeLiteral(TypeBuiltinFunValue(TypeBuiltinFunction.Conj)), _) => () }
                        inside(args2) {
                          case NonEmptyList(arg21, arg22) =>
                            inside(arg21) {
                              case App(fun3, args3, _) =>
                                inside(fun3) { case Simple(TypeLiteral(TypeBuiltinFunValue(TypeBuiltinFunction.Disj)), _) => () }
                                inside(args3) { case NonEmptyList(Simple(TypeVar(LocalSymbol("t1")), _), Simple(TypeVar(LocalSymbol("t2")), _)) => () }
                            }
                            inside(arg22) { case Simple(TypeVar(LocalSymbol("t3")), _) => ()}
                        }
                    }
                    List(idx, idx1, idx2).toSet should be ===((0 to 2).toSet)
                }
            }
        }
    }
  }
  
  it should "index let-expression and lambda-expressions" in {
    val res = LambdaIndexer.transformString("""
f x y = (let
    a = \z => z
  in
    (\x y => x 10) (\x => #iAdd x 10)) (\x => x)
""")(NameTree.empty)
    inside(res) {
      case Success(Tree(combs, treeInfo)) =>
        inside(combs.get(GlobalSymbol(NonEmptyList("f")))) {
          case Some(Combinator(None, _, App(fun1, NonEmptyList(arg11), _), LambdaInfo(parser.LambdaInfo, idx), _)) =>
            inside(fun1) {
              case Simple(Let(NonEmptyList(Bind(_, body2, _)), body3, LambdaInfo(parser.LambdaInfo, idx1)), _) =>
                inside(body2) {
                  case Simple(Lambda(_, body4, LambdaInfo(parser.LambdaInfo, idx2)), _) =>
                    inside(body3) {
                      case App(fun5, NonEmptyList(arg51), _) =>
                        inside(fun5) {
                          case Simple(Lambda(_, body5, LambdaInfo(parser.LambdaInfo, idx5)), _) =>
                            inside(arg51) {
                              case Simple(Lambda(_, body51, LambdaInfo(parser.LambdaInfo, idx51)), _) =>
                                inside(arg11) {
                                  case Simple(Lambda(_, body11, LambdaInfo(parser.LambdaInfo, idx11)), _) =>
                                    inside(body4) {
                                      case Simple(Var(_, LambdaInfo(parser.LambdaInfo, idx4)), _) =>
                                        inside(body5) {
                                          case App(fun6, _, _) =>
                                            inside(fun6) {
                                              case Simple(Var(_, LambdaInfo(parser.LambdaInfo, idx6)), _) =>
                                                inside(body51) {
                                                  case App(_, NonEmptyList(Simple(Var(_, LambdaInfo(parser.LambdaInfo, idx511)), _), _), _) =>
                                                    List(idx, idx1, idx2, idx4, idx5, idx51, idx6, idx511, idx11).toSet should be ===((0 to 8).toSet)
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
    }
  }
  
  it should "index type lambda-expressions" in {
    val res = LambdaIndexer.transformString("""
type T t1 = (\t2 => (\t3 => t3) (\t3 => ##& (##& t1 (t2 t1)) t3)) (\t2 => t2)
""")(NameTree.empty)
    inside(res) {
      case Success(Tree(combs, resolver.TreeInfo(Tree(typeCombs, typeTreeInfo)))) =>
        inside(typeCombs.get(GlobalSymbol(NonEmptyList("T")))) {
          case Some(TypeCombinator(None, _, App(fun1, NonEmptyList(arg11), _), TypeLambdaInfo(parser.TypeLambdaInfo, idx), _)) =>
            inside(fun1) {
              case Simple(TypeLambda(_, App(fun2, NonEmptyList(arg21), _), TypeLambdaInfo(parser.TypeLambdaInfo, idx1)), _) =>
                inside(fun2) {
                  case Simple(TypeLambda(_, _, TypeLambdaInfo(parser.TypeLambdaInfo, idx2)), _) =>
                    inside(arg21) {
                      case Simple(TypeLambda(_, _, TypeLambdaInfo(parser.TypeLambdaInfo, idx21)), _) =>
                        inside(arg11) {
                          case Simple(TypeLambda(_, _, TypeLambdaInfo(parser.TypeLambdaInfo, idx11)), _) =>
                            List(idx, idx1, idx2, idx21, idx11).toSet should be ===((0 to 4).toSet)
                        }
                    }
                }
            }
        }
    }    
  }
  
  it should "index type lambda-expressions in the typed terms" in {
    val res = LambdaIndexer.transformString("""
f = #iAdd (10: (\t1 => \t2 => t1) #Int #Any) (20: (\t => t) #Int)
""")(NameTree.empty)
    inside(res) {
      case Success(Tree(combs, treeInfo)) =>
        inside(combs.get(GlobalSymbol(NonEmptyList("f")))) {
          case Some(Combinator(None, _, App(_, NonEmptyList(arg11, arg12), _), _, _)) =>
            inside(arg11) {
              case Simple(TypedTerm(_, App(typeFun1, _, _)), _) =>
                inside(typeFun1) {
                  case Simple(TypeLambda(_, typeBody2, TypeLambdaInfo(parser.TypeLambdaInfo, idx1)), _) =>
                    inside(typeBody2) {
                      case Simple(TypeLambda(_, _, TypeLambdaInfo(parser.TypeLambdaInfo, idx2)), _) =>
                        List(idx1, idx2).toSet should be ===(Set(0, 1))
                    }
                }
            }
            inside(arg12) {
              case Simple(TypedTerm(_, App(typeFun3, _, _)), _) =>
                inside(typeFun3) {
                  case Simple(TypeLambda(_, _, TypeLambdaInfo(parser.TypeLambdaInfo, idx3)), _) =>
                    idx3 should be ===(0)
                }
            }
        }
    }
  }
}