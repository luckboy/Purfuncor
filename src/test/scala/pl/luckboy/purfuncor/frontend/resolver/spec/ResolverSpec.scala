package pl.luckboy.purfuncor.frontend.resolver.spec
import scalaz._
import scalaz.Scalaz._
import org.scalatest.FlatSpec
import org.scalatest.Inside
import org.scalatest.matchers.ShouldMatchers
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver._
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.frontend.Bind

class ResolverSpec extends FlatSpec with ShouldMatchers with Inside
{
  "A Resolver" should "transform the string to a tree" in {
    val res = Resolver.transformString("f x = #iAdd x 1; g = #iMul (f 10) 5")(NameTree.empty) 
    inside(res) {
      case Success(Tree(combs, treeInfo)) =>
        combs.keySet should be ===(Set(GlobalSymbol(NonEmptyList("f")), GlobalSymbol(NonEmptyList("g"))))
        // f
        inside(combs.get(GlobalSymbol(NonEmptyList("f")))) {
          case Some(Combinator(args, body, parser.LetInfo, None)) =>
            inside(args) { case List(Arg(Some("x"), _)) => () } 
            inside(body) { 
              case App(fun1, args1, _) =>
                inside(fun1) { case Simple(Literal(BuiltinFunValue(BuiltinFunction.IAdd)), _) => () }
                inside(args1) { case NonEmptyList(Simple(Var(LocalSymbol("x")), _), Simple(Literal(IntValue(1)), _)) => () }
            }
        }
        // g
        inside(combs.get(GlobalSymbol(NonEmptyList("g")))) {
          case Some(Combinator(Nil, body, parser.LetInfo, None)) =>
            inside(body) {
              case App(fun1, args1, _) =>
                inside(fun1) { case Simple(Literal(BuiltinFunValue(BuiltinFunction.IMul)), _) => () }
                inside(args1) {
                  case NonEmptyList(arg11, arg12) =>
                    inside(arg11) { 
                      case App(fun2, args2, _) =>
                        inside(fun2) { case Simple(Var(GlobalSymbol(NonEmptyList("f"))), _) => () }
                        inside(args2) { case NonEmptyList(Simple(Literal(IntValue(10)), _)) => () }
                    }
                    inside(arg12) { case Simple(Literal(IntValue(5)), _) => () }
                }
            }
        }
    }
  }
  
  it should "transform the string to a tree for modules" in {
    val res = Resolver.transformString("""
module m1 {
  module m2 {
    f x y = #iDiv x (#.m2.f y)
  }
  module m3 {
    g = #iDiv (m2.f 10 2) (h 10)
  }
  h x = x
}
module m2 {
  f x = #iAdd (m1.h x) x
}
""")(NameTree.empty)
    inside(res) {
      case Success(Tree(combs, treeInfo)) =>
        combs.keySet should be ===(Set(
            GlobalSymbol(NonEmptyList("m1", "m2", "f")),
            GlobalSymbol(NonEmptyList("m1", "m3", "g")),
            GlobalSymbol(NonEmptyList("m1", "h")),
            GlobalSymbol(NonEmptyList("m2", "f"))))
        // m1.m2.f
        inside(combs.get(GlobalSymbol(NonEmptyList("m1", "m2", "f")))) {
          case Some(Combinator(args, body, parser.LetInfo, None)) =>
            inside(args) { case List(Arg(Some("x"), _), Arg(Some("y"), _)) => () }
            inside(body) {
              case App(fun1, args1, _) =>
                inside(fun1) { case Simple(Literal(BuiltinFunValue(BuiltinFunction.IDiv)), _) => () }
                inside(args1) {
                  case NonEmptyList(arg11, arg12) =>
                    inside(arg11) { case Simple(Var(LocalSymbol("x")), _) => () } 
                    inside(arg12) {
                      case App(fun2, args2, _) =>
                        inside(fun2) { case Simple(Var(GlobalSymbol(NonEmptyList("m2", "f"))), _) => () }
                        inside(args2) { case NonEmptyList(Simple(Var(LocalSymbol("y")), _)) => () }
                    }
                }
            }
        }
        // m1.m3.g
        inside(combs.get(GlobalSymbol(NonEmptyList("m1", "m3", "g")))) {
          case Some(Combinator(Nil, body, parser.LetInfo, None)) =>
            inside(body) {
              case App(fun1, args1, _) =>
                inside(fun1) { case Simple(Literal(BuiltinFunValue(BuiltinFunction.IDiv)), _) => () }
                inside(args1) {
                  case NonEmptyList(arg11, arg12) =>
                    inside(arg11) {
                      case App(fun2, args2, _) =>
                        inside(fun2) { case Simple(Var(GlobalSymbol(NonEmptyList("m1", "m2", "f"))), _) => () }
                        inside(args2) { case NonEmptyList(Simple(Literal(IntValue(10)), _), Simple(Literal(IntValue(2)), _)) => () }
                    }
                    inside(arg12) {
                      case App(fun3, args3, _) =>
                        inside(fun3) { case Simple(Var(GlobalSymbol(NonEmptyList("m1", "h"))), _) => () }
                        inside(args3) { case NonEmptyList(Simple(Literal(IntValue(10)), _)) => () }
                    }
                }
            }
        }
        // m1.h
        inside(combs.get(GlobalSymbol(NonEmptyList("m1", "h")))) {
          case Some(Combinator(args, body, parser.LetInfo, None)) =>
            inside(args) { case List(Arg(Some("x"), _)) => () }
            inside(body) { case Simple(Var(LocalSymbol("x")), _) => () }
        }
        // m2.f
        inside(combs.get(GlobalSymbol(NonEmptyList("m2", "f")))) {
          case Some(Combinator(args, body, parser.LetInfo, None)) =>
            inside(args) { case List(Arg(Some("x"), _)) => () }
            inside(body) {
              case App(fun1, args1, _) =>
                inside(fun1) { case Simple(Literal(BuiltinFunValue(BuiltinFunction.IAdd)), _) => () }
                inside(args1) {
                  case NonEmptyList(arg11, arg12) =>
                    inside(arg11) { 
                      case App(fun2, args2, _) =>
                        inside(fun2) { case Simple(Var(GlobalSymbol(NonEmptyList("m1", "h"))), _) => () }
                        inside(args2) { case NonEmptyList(Simple(Var(LocalSymbol("x")), _)) => () } 
                    }
                    inside(arg12) { case Simple(Var(LocalSymbol("x")), _) => () }
                }
            }
        }
    }
  }
  
  it should "transform the string to a tree for imports" in {
    val res = Resolver.transformString("""
module a {
  import b.c.d.e.f
  import b.d
  
  f = g 10 (h 5)
  g x y = #iMod x i
}
module b.c.d.e.f {
  g x y = #iSub x y
  h x = #iNeg x
}
module b.d {
  i = 10
}
""")(NameTree.empty)
    inside(res) {
      case Success(Tree(combs, treeInfo)) =>
        combs.keySet should be ===(Set(
            GlobalSymbol(NonEmptyList("a", "f")),
            GlobalSymbol(NonEmptyList("a", "g")),
            GlobalSymbol(NonEmptyList("b", "c", "d", "e", "f", "g")),
            GlobalSymbol(NonEmptyList("b", "c", "d", "e", "f", "h")),
            GlobalSymbol(NonEmptyList("b", "d", "i"))
            ))
        // a.f
        inside(combs.get(GlobalSymbol(NonEmptyList("a", "f")))) {
          case Some(Combinator(Nil, body, parser.LetInfo, None)) =>
            inside(body) {
              case App(fun1, args1, _) =>
                inside(fun1) { case Simple(Var(GlobalSymbol(NonEmptyList("a", "g"))), _) => () }
                inside(args1) {
                  case NonEmptyList(arg11, arg12) =>
                    inside(arg11) { case Simple(Literal(IntValue(10)), _) => () }
                    inside(arg12) { 
                      case App(fun2, args2, _) =>
                        inside(fun2) { case Simple(Var(GlobalSymbol(NonEmptyList("b", "c", "d", "e", "f", "h"))), _) => () }
                        inside(args2) { case NonEmptyList(Simple(Literal(IntValue(5)), _)) => () }
                    }
                }
            }
        }
        // a.g
        inside(combs.get(GlobalSymbol(NonEmptyList("a", "g")))) {
          case Some(Combinator(args, body, parser.LetInfo, None)) =>
            inside(args) { case List(Arg(Some("x"), _), Arg(Some("y"), _)) => () }
            inside(body) {
              case App(fun1, args1, _) =>
                inside(fun1) { case Simple(Literal(BuiltinFunValue(BuiltinFunction.IMod)), _) => () }
                inside(args1) { case NonEmptyList(Simple(Var(LocalSymbol("x")), _), Simple(Var(GlobalSymbol(NonEmptyList("b", "d", "i"))), _)) => () }
            }
        }
        // b.c.d.e.f.g
        inside(combs.get(GlobalSymbol(NonEmptyList("b", "c", "d", "e", "f", "g")))) {
          case Some(Combinator(args, body, parser.LetInfo, None)) =>
            inside(args) { case List(Arg(Some("x"), _), Arg(Some("y"), _)) => () }
            inside(body) {
              case App(fun1, args1, _) =>
                inside(fun1) { case Simple(Literal(BuiltinFunValue(BuiltinFunction.ISub)), _) => () }
                inside(args1) { case NonEmptyList(Simple(Var(LocalSymbol("x")), _), Simple(Var(LocalSymbol("y")), _)) => () }
            }
        }
        // b.c.d.e.f.h
        inside(combs.get(GlobalSymbol(NonEmptyList("b", "c", "d", "e", "f", "h")))) {
          case Some(Combinator(args, body, parser.LetInfo, None)) =>
            inside(args) { case List(Arg(Some("x"), _)) => () }
            inside(body) {
              case App(fun1, args1, _) =>
                inside(fun1) { case Simple(Literal(BuiltinFunValue(BuiltinFunction.INeg)), _) => () }
                inside(args1) { case NonEmptyList(Simple(Var(LocalSymbol("x")), _)) => () }
            }
        }
        // b.d.i
        inside(combs.get(GlobalSymbol(NonEmptyList("b", "d", "i")))) {
          case Some(Combinator(Nil, body, parser.LetInfo, None)) =>
            inside(body) { case Simple(Literal(IntValue(10)), _) => () }
        }
    }
  }
  
  it should "resolve the symbols of the combinator definitions" in {
    val res = Resolver.transformString("""
f = 1
module m1 {
  #.m10.g = 2
  module m2 {
    h = 3
    m3.m4.i = 4
    m5.j = 5
  }
  k = 6
  #.l = 7
}
module m2.m3 {
  m = 8
  #.n = 9
  #.m1.m2.o = 10
}
""")(NameTree.empty)
    inside(res) {
      case Success(Tree(combs, treeInfo)) =>
        combs.keySet should be ===(Set(
            GlobalSymbol(NonEmptyList("f")),
            GlobalSymbol(NonEmptyList("m10", "g")),
            GlobalSymbol(NonEmptyList("m1", "m2", "h")),
            GlobalSymbol(NonEmptyList("m1", "m2", "m3", "m4", "i")),
            GlobalSymbol(NonEmptyList("m1", "m2", "m5", "j")), 
            GlobalSymbol(NonEmptyList("m1", "k")),
            GlobalSymbol(NonEmptyList("l")),
            GlobalSymbol(NonEmptyList("m2", "m3", "m")),
            GlobalSymbol(NonEmptyList("n")),
            GlobalSymbol(NonEmptyList("m1", "m2", "o"))
            ))
    }
  }
}