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
}