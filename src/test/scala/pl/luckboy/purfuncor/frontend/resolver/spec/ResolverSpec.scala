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

  it should "transform let-expression and lambda-expression" in {
    val res = Resolver.transformString("""
f = let a = 1; b = 2 in let c = g 2 3 in #iAdd a c
g = \x _ y => #iDiv x y
""")(NameTree.empty)    
    inside(res) {
      case Success(Tree(combs, treeInfo)) =>
        combs.keySet should be ===(Set(GlobalSymbol(NonEmptyList("f")), GlobalSymbol(NonEmptyList("g"))))
        // f
        inside(combs.get(GlobalSymbol(NonEmptyList("f")))) {
          case Some(Combinator(Nil, body, parser.LetInfo, None)) =>
            inside(body) {
              case Simple(Let(binds1, body1, parser.LetInfo), _) =>
                inside(binds1) {
                  case NonEmptyList(bind11, bind12) =>
                    inside(bind11) { case Bind("a", Simple(Literal(IntValue(1)), _), _) => () }
                    inside(bind12) { case Bind("b", Simple(Literal(IntValue(2)), _), _) => () }
                }
                inside(body1) {
                  case Simple(Let(binds2, body2, parser.LetInfo), _) =>
                    inside(binds2) {
                      case NonEmptyList(bind21) =>
                        inside(bind21) {
                          case Bind("c", App(fun3, args3, _), _) =>
                            inside(fun3) { case Simple(Var(GlobalSymbol(NonEmptyList("g"))), _) => () }
                            inside(args3) { case NonEmptyList(Simple(Literal(IntValue(2)), _), Simple(Literal(IntValue(3)), _)) => () }
                        }
                    }
                    inside(body2) {
                      case App(fun4, args4, _) =>
                        inside(fun4) { case Simple(Literal(BuiltinFunValue(BuiltinFunction.IAdd)), _) => () }
                        inside(args4) { case NonEmptyList(Simple(Var(LocalSymbol("a")), _), Simple(Var(LocalSymbol("c")), _)) => () }
                    }
                }
            }
        }
        // g
        inside(combs.get(GlobalSymbol(NonEmptyList("g")))) {
          case Some(Combinator(Nil, body, parser.LetInfo, None)) =>
            inside(body) {
              case Simple(Lambda(args1, body1, parser.LetInfo), _) =>
                inside(args1) { case NonEmptyList(Arg(Some("x"), _), Arg(None, _), Arg(Some("y"), _)) => () }
                inside(body1) {
                  case App(fun2, args2, _) =>
                    inside(fun2) { case Simple(Literal(BuiltinFunValue(BuiltinFunction.IDiv)), _) => () }
                    inside(args2) { case NonEmptyList(Simple(Var(LocalSymbol("x")), _), Simple(Var(LocalSymbol("y")), _)) => () }
                }
            }
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
            GlobalSymbol(NonEmptyList("m1", "m2", "o"))))
    }
  }
  
  it should "resolve the symbols of the covering local variables" in {
    val res = Resolver.transformString("""
z1 = 1
z2 = 2
f x y = let
    z1 = 3
  in
    let 
      z2 = 4
    in
      #iAdd z1 z2
g z1 x = #iMul z1 x
h x y = \z1 => \z2 => #iSub z1 z2
""")(NameTree.empty)
	inside(res) {
      case Success(Tree(combs, treeInfo)) =>
        inside(combs.get(GlobalSymbol(NonEmptyList("f")))) {
          case Some(Combinator(_, body, _, None)) =>
            inside(body) {
              case Simple(Let(_, body1, _), _) =>
                inside(body1) {
                  case Simple(Let(_, App(_, args2, _), _), _) =>
                    inside(args2) { case NonEmptyList(Simple(Var(LocalSymbol("z1")), _), Simple(Var(LocalSymbol("z2")), _)) => () }
                }
            }
        }
        inside(combs.get(GlobalSymbol(NonEmptyList("g")))) {
          case Some(Combinator(_, App(_, args1, _), _, None)) =>
            inside(args1) { case NonEmptyList(Simple(Var(LocalSymbol("z1")), _), Simple(Var(LocalSymbol("x")), _)) => () }
        }
        inside(combs.get(GlobalSymbol(NonEmptyList("h")))) {
          case Some(Combinator(_, body, _, None)) =>
            inside(body) {
              case Simple(Lambda(NonEmptyList(Arg(Some("z1"), _)), body1, _), _) =>
                inside(body1) {
                  case Simple(Lambda(NonEmptyList(Arg(Some("z2"), _)), App(_, args3, _), _), _) =>
                    inside(args3) { case NonEmptyList(Simple(Var(LocalSymbol("z1")), _), Simple(Var(LocalSymbol("z2")), _)) => () }
                }
            }
        }
    }
  }
  
  it should "resolve the symbols of the covering global variables" in {
    val res = Resolver.transformString("""
module m1 {
  z1 = 1
  module m2 {
    z1 = 2
    f = #iAdd z1 z2
    g = #iMul z3 z10
    z3 = 3
  }
  z10 = 10
  h = z2
  z2 = 4
}
z2 = 5
z3 = 6
""")(NameTree.empty)
	inside(res) {
      case Success(Tree(combs, treeInfo)) =>
        inside(combs.get(GlobalSymbol(NonEmptyList("m1", "m2", "f")))) {
          case Some(Combinator(_, App(_, args1, _), _, None)) =>
            inside(args1) { case NonEmptyList(Simple(Var(GlobalSymbol(NonEmptyList("m1", "m2", "z1"))), _), Simple(Var(GlobalSymbol(NonEmptyList("m1", "z2"))), _)) => () }
        }
        inside(combs.get(GlobalSymbol(NonEmptyList("m1", "m2", "g")))) {
          case Some(Combinator(_, App(_, args1, _), _, None)) =>
            inside(args1) { case NonEmptyList(Simple(Var(GlobalSymbol(NonEmptyList("m1", "m2", "z3"))), _), Simple(Var(GlobalSymbol(NonEmptyList("m1", "z10"))), _)) => () }
        }
        inside(combs.get(GlobalSymbol(NonEmptyList("m1", "h")))) {
          case Some(Combinator(_, body, _, None)) =>
            inside(body) { case Simple(Var(GlobalSymbol(NonEmptyList("m1", "z2"))), _) => () }
        }
    }
  }
  
  it should "resolve the symbols of the covering modules" in {
    val res = Resolver.transformString("""
module m1 {
  module m2 {
    z1 = 10
  }
  module m3 {
    f = m2.z1
    m2.z1 = 20
  }
  z2 = 30
}
module m4 {
  module m5 {
    g = m1.m2.z1
  }
  h = m1.z2
  module m1 {
    module m2 {
      z1 = 40
    }
    m3.m2.z1 = 50
    z2 = 60
  }
}
""")(NameTree.empty)
	inside(res) {
      case Success(Tree(combs, treeInfo)) =>
        inside(combs.get(GlobalSymbol(NonEmptyList("m1", "m3", "f")))) {
          case Some(Combinator(_, Simple(Var(GlobalSymbol(NonEmptyList("m1", "m3", "m2", "z1"))), _), _, None)) => ()
        }
        inside(combs.get(GlobalSymbol(NonEmptyList("m4", "m5", "g")))) {
          case Some(Combinator(_, Simple(Var(GlobalSymbol(NonEmptyList("m4", "m1", "m2", "z1"))), _), _, None)) => ()          
        }
        inside(combs.get(GlobalSymbol(NonEmptyList("m4", "h")))) {
          case Some(Combinator(_, Simple(Var(GlobalSymbol(NonEmptyList("m4", "m1", "z2"))), _), _, None)) => ()          
        }
    }
  }
  
  it should "complain on undefined variables" in {
    val res = Resolver.transformString("""
f x y = z
g x y = m1.i
h = #.m2.j
i = m2.k
j = m2.m1.l
m2.i = 10
""")(NameTree.empty)
    inside(res) {
      case Failure(errs) =>
        errs.map { _.msg } should be ===(NonEmptyList(
            "undefined variable z",
            "undefined module m1",
            "undefined global variable #.m2.j",
            "undefined global variable #.m2.k",
            "undefined global variable #.m2.m1.l"))
    }
  }
  
  it should "complain on already defined variables" in {
    val res = Resolver.transformString("""
z1 = 10
m1.z2 = 20      
f = let a = 1; a = 2 in a
g = let
    a = 1
  in
    let a = 2; b = 3; c = 4; b = 5 in a
z1 = 30
#.m1.z2 = 40
""")(NameTree.empty)
    inside(res) {
      case Failure(errs) =>
        errs.map { _.msg } should be ===(NonEmptyList(
            "already defined global variable #.z1",
            "already defined global variable #.m1.z2",
            "already defined local variable a",
            "already defined local variable b"))
    }
  }

  it should "complain on already defined arguments" in {
    val res = Resolver.transformString("""
f x y z x = 10
g = \x y y => #iAdd x y
""")(NameTree.empty)
    inside(res) {
      case Failure(errs) =>
        errs.map { _.msg } should be ===(NonEmptyList(
            "already defined argument x",
            "already defined argument y"))
    }
  }

  it should "complain on ambiguous references" in {
    val res = Resolver.transformString("""
module m1.m2 {
  f = 10
  g = 20
  m3.h = 40
}
module m4.m2 {
  f = 50
  m3.h = 60
}
module m5 {
  import m1.m2
  import m4.m2
  i = f
  j = #iAdd m3.h g
}
""")(NameTree.empty)
    inside(res) {
      case Failure(errs) =>
        errs.map { _.msg } should be ===(NonEmptyList(
            "reference to f is ambiguous",
            "reference to m3 is ambiguous"))
    }
  }
  
  it should "resolve the symbols which are defined at other tree" in {
    val res = Resolver.transformString("""
f2 = m1.m2.f
module m1 {
  g2 = g
}
m3.h2 = #.m2.h
""")(
    NameTree.empty |+|
    NameTree.fromGlobalSymbol(GlobalSymbol(NonEmptyList("m1", "m2", "f"))) |+|
    NameTree.fromGlobalSymbol(GlobalSymbol(NonEmptyList("m1", "g"))) |+|
    NameTree.fromGlobalSymbol(GlobalSymbol(NonEmptyList("m2", "h"))))
    inside(res) {
      case Success(Tree(combs, treeInfo)) =>
        combs.keySet should be ===(Set(
            GlobalSymbol(NonEmptyList("f2")),
            GlobalSymbol(NonEmptyList("m1", "g2")),
            GlobalSymbol(NonEmptyList("m3", "h2"))))
        inside(combs.get(GlobalSymbol(NonEmptyList("f2")))) {
          case Some(Combinator(Nil, body, parser.LetInfo, None)) =>
            inside(body) { case Simple(Var(GlobalSymbol(NonEmptyList("m1", "m2", "f"))), _) => () }
        }
        inside(combs.get(GlobalSymbol(NonEmptyList("m1", "g2")))) {
          case Some(Combinator(Nil, body, parser.LetInfo, None)) =>
            inside(body) { case Simple(Var(GlobalSymbol(NonEmptyList("m1", "g"))), _) => () }
        }
        inside(combs.get(GlobalSymbol(NonEmptyList("m3", "h2")))) {
          case Some(Combinator(Nil, body, parser.LetInfo, None)) =>
            inside(body) { case Simple(Var(GlobalSymbol(NonEmptyList("m2", "h"))), _) => () }
        }
    }
  }
}