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
          case Some(Combinator(None, args, body, LambdaInfo(idx), _)) =>
            inside(args) { case List(Arg(Some("x"), None, _), Arg(Some("y"), None, _)) => () }
            inside(body) {
              case Simple(Lambda(args1, body1, LambdaInfo(idx1)), _) =>
                inside(args1) { case NonEmptyList(Arg(Some("z"), None, _)) => () }
                inside(body1) {
                  case Simple(Let(binds2, body2, LambdaInfo(idx2)), _) =>
                    inside(binds2) { case NonEmptyList(Bind("a", Simple(Literal(IntValue(1)), _), _)) => () }
                    inside(body2) {
                      case App(fun3, args3, _) =>
                        inside(fun3) { case Simple(Literal(BuiltinFunValue(BuiltinFunction.IAdd)), _) => () }
                        inside(args3) { case NonEmptyList(Simple(Var(LocalSymbol("a")), _), Simple(Var(LocalSymbol("z")), _)) => () }
                        List(idx, idx1, idx2).toSet should be ===((0 to 2).toSet)
                    }
                }
            }
        }
        inside(typeCombs.get(GlobalSymbol(NonEmptyList("T")))) {
          case Some(TypeCombinator(None, args, body, TypeLambdaInfo(idx), _)) =>
            inside(args) { case List(TypeArg(Some("t1"), None, _)) => () }
            inside(body) {
              case Simple(TypeLambda(args1, body1, TypeLambdaInfo(idx1)), _) =>
                inside(args1) { case NonEmptyList(TypeArg(Some("t2"), None, _)) => () }
                inside(body1) {
                  case Simple(TypeLambda(args2, body2, TypeLambdaInfo(idx2)), _) =>
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
          case Some(Combinator(None, _, App(fun1, NonEmptyList(arg11), _), LambdaInfo(idx), _)) =>
            inside(fun1) {
              case Simple(Let(NonEmptyList(Bind(_, body2, _)), body3, LambdaInfo(idx1)), _) =>
                inside(body2) {
                  case Simple(Lambda(_, _, LambdaInfo(idx2)), _) =>
                    inside(body3) {
                      case App(fun4, NonEmptyList(arg41), _) =>
                        inside(fun4) {
                          case Simple(Lambda(_, _, LambdaInfo(idx4)), _) =>
                            inside(arg41) {
                              case Simple(Lambda(_, _, LambdaInfo(idx41)), _) =>
                                inside(arg11) {
                                  case Simple(Lambda(_, _, LambdaInfo(idx11)), _) =>
                                    List(idx, idx1, idx2, idx4, idx41, idx11).toSet should be ===((0 to 5).toSet)
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
          case Some(TypeCombinator(None, _, App(fun1, NonEmptyList(arg11), _), TypeLambdaInfo(idx), _)) =>
            inside(fun1) {
              case Simple(TypeLambda(_, App(fun2, NonEmptyList(arg21), _), TypeLambdaInfo(idx1)), _) =>
                inside(fun2) {
                  case Simple(TypeLambda(_, _, TypeLambdaInfo(idx2)), _) =>
                    inside(arg21) {
                      case Simple(TypeLambda(_, _, TypeLambdaInfo(idx21)), _) =>
                        inside(arg11) {
                          case Simple(TypeLambda(_, _, TypeLambdaInfo(idx11)), _) =>
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
                  case Simple(TypeLambda(_, typeBody2, TypeLambdaInfo(idx1)), _) =>
                    inside(typeBody2) {
                      case Simple(TypeLambda(_, _, TypeLambdaInfo(idx2)), _) =>
                        List(idx1, idx2).toSet should be ===(Set(0, 1))
                    }
                }
            }
            inside(arg12) {
              case Simple(TypedTerm(_, App(typeFun3, _, _)), _) =>
                inside(typeFun3) {
                  case Simple(TypeLambda(_, _, TypeLambdaInfo(idx3)), _) =>
                    idx3 should be ===(0)
                }
            }
        }
    }
  }
}