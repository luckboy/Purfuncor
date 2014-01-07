package pl.luckboy.purfuncor.backend.interp.spec
import scala.util.parsing.input.OffsetPosition
import scalaz._
import scalaz.Scalaz._
import org.scalatest.FlatSpec
import org.scalatest.Inside
import org.scalatest.matchers.ShouldMatchers
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend.kinder
import pl.luckboy.purfuncor.frontend.instant
import pl.luckboy.purfuncor.frontend.SimpleTerm
import pl.luckboy.purfuncor.frontend.TypeSimpleTerm
import pl.luckboy.purfuncor.frontend.AbstractCombinator
import pl.luckboy.purfuncor.frontend.parser
import pl.luckboy.purfuncor.frontend.resolver
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol
import pl.luckboy.purfuncor.backend.interp._
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.backend.interp.Value

class InterpreterSpec extends FlatSpec with ShouldMatchers with Inside
{
  def interpreter[T, U, V, W, X, C, E, D](emptyEnv: E, initData: D)(makeData: String => ValidationNel[AbstractError, D])(f2: D => Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => State[E, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, V, W], X]]])(g4: D => (Term[SimpleTerm[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]]], E) => ValidationNel[AbstractError, Term[SimpleTerm[U, V, W]]])(implicit init: Initializer[NoValue[U, V, W, C], T, AbstractCombinator[U, V, W], E], eval: Evaluator[SimpleTerm[U, V, W], E, Value[U, V, W, C]], envSt: EnvironmentState[E, T, Value[U, V, W, C], InstanceValue[U, V, W, C]], enval: Environmental[E, Value[U, V, W, C]])
  {
    //TODO: add a test for the global variable contains the lambda-expression with the reference to itself
    //TODO: add a test for the global variable contains the let-expression with the reference to itself
    val f = f2(initData)
    val g3 = g4(initData)
    
    it should "interpret the term string" in {
      val (env, res) = Interpreter.interpretTermString("#iAdd 2 (#iMul 3 4)")(g3).run(emptyEnv)
      res should be ===(IntValue(14).success)
    }
    
    it should "interpret the tree string" in {
      val (env, res) = Interpreter.interpretTreeString("""
f = #iAdd g h
g = #iMul h 2
h = #iSub 7 4
""")(f).run(emptyEnv)
      res should be ===(().success.success)
      enval.globalVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("f"))) should be ===(IntValue(9))
      enval.globalVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("g"))) should be ===(IntValue(6))
      enval.globalVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("h"))) should be ===(IntValue(3))
    }
    
    it should "initialize all independent variables" in {
      val (env, res) = Interpreter.interpretTreeString("f = 10; g = 20; h = 30")(f).run(emptyEnv)
      res should be ===(().success.success)
      enval.globalVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("f"))) should be ===(IntValue(10))
      enval.globalVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("g"))) should be ===(IntValue(20))
      enval.globalVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("h"))) should be ===(IntValue(30))
    }
    
    it should "initialize all dependent variables" in {
      val (env, res) = Interpreter.interpretTreeString("""
f = #iAdd (#iAdd k (g 1 2)) (j 3 4)
g x y = #iMul (h x) y
h x = #iAdd (#iNeg x) i
i = 5
j x y = #intFromDouble (#dDiv (#doubleFromInt (#iAdd 6 x)) (#doubleFromInt (#iSub y k)))
k = #iAdd (l 7) (h 8)
l x = #iAdd x 3
""")(f).run(emptyEnv)
      res should be ===(().success.success)
      enval.globalVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("f"))) should be ===(IntValue(12))
      inside(enval.globalVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("g")))) { case CombinatorValue(_, _, _) => () }
      inside(enval.globalVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("h")))) { case CombinatorValue(_, _, _) => () }
      enval.globalVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("i"))) should be ===(IntValue(5))
      inside(enval.globalVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("j")))) { case CombinatorValue(_, _, _) => () }
      enval.globalVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("k"))) should be ===(IntValue(7))
      inside(enval.globalVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("l")))) { case CombinatorValue(_, _, _) => () }
    }
    
    it should "interpret the term string with the global variables" in {
      val s = """
f = 1
g = 2
h x = #iMul x 3 
"""
      val (env, res) = Interpreter.interpretTreeString(s)(f).run(emptyEnv)
      val res2 = makeData(s)
      inside(res2) {
        case Success(data) =>
          val (env2, res3) = Interpreter.interpretTermString("#iAdd f (#iSub (h 10) g)")(g4(data)).run(env)
          res3 should be ===(IntValue(29).success)
      }
    }
    
    it should "interpret the let-expressions" in {
      val (env, res) = Interpreter.interpretTreeString("""
f = let
    a = 10
    b = 20
  in
    #iMul (let
      c = 30
    in
      #iAdd (#iAdd a b) c) a
""")(f).run(emptyEnv)
      res should be ===(().success.success)
      enval.globalVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("f"))) should be ===(IntValue(600))
    }
    
    it should "interpret the applications of the lambda expressions" in {
      val (env, res) = Interpreter.interpretTreeString("""
f = let
      a = 1
    in
      let
        b = \x => #iAdd x a
        c = 3
        d = 4
      in
        (\x y => #iAdd (#iMul (b x) c) (#iMul x y)) d (#iAdd d 5)
""")(f).run(emptyEnv)
      res should be ===(().success.success)
      enval.globalVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("f"))) should be ===(IntValue(51))
    }

    it should "interpret the partial applications" in {
      val (env, res) = Interpreter.interpretTreeString("""
f x y z = #iAdd (#iMul x y) z
g = let
      a = f 3
      b = \x y => #iSub x y
    in
      let
        c = b 5
        d = a 6
      in
        #iAdd (#iAdd (a 7 8) (a 9 10)) (#iMul (c 11) (d 12))
""")(f).run(emptyEnv)
      res should be ===(().success.success)
      enval.globalVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("g"))) should be ===(IntValue(-114))
    }
    
    it should "interpret the term with the covered local variables" in {
      val (env, res) = Interpreter.interpretTermString("""
let
  a = 14
  b = 11
in
  #iMul (let
    a = 3
    b = 4
  in
    #iAdd a b) (#iSub a b)
""")(g3).run(emptyEnv)
      res should be ===(IntValue(21).success)
    }
    
    ignore should "complain at the term" in {
      val (env, res) = Interpreter.interpretTermString("#iAdd (#iDiv 1 0) 2")(g3).run(emptyEnv)
      inside(res) {
        case Success(noValue: NoValue[U, V, W, C]) =>
          noValue.msg should be ===("divided by zero")
          inside(noValue.stackTrace) { case List(StackTraceElement(None, None, OffsetPosition(_, _))) => () }
      }
    }
    
    ignore should "complain at the combinator without the arguments" in {
      val (env, res) = Interpreter.interpretTreeString("f = #iSub 1 (#iDiv 2 0)")(f).run(emptyEnv)
      inside(res) {
        case Success(Failure(noValue)) =>
          noValue.msg should be ===("divided by zero")
          inside(noValue.stackTrace) { case List(StackTraceElement(None, Some(GlobalSymbol(NonEmptyList("f"))), OffsetPosition(_, _))) => () }
      }
    }
    
    ignore should "complain at the combinator with the two arguments" in {
      val (env, res) = Interpreter.interpretTreeString("f x y = #iAdd (#iDiv x y) y")(f).run(emptyEnv)
      val (env2, res2) = Interpreter.interpretTermString("f 10 0")(g3).run(env)
      inside(res2) {
        case Success(noValue: NoValue[U, V, W, C]) =>
          noValue.msg should be ===("divided by zero")
          inside(noValue.stackTrace) { 
            case List(stackTraceElem1, stackTraceElem2) =>
              inside(stackTraceElem1) { case StackTraceElement(None, Some(GlobalSymbol(NonEmptyList("f"))), OffsetPosition(_, _)) => () }
              inside(stackTraceElem2) { case StackTraceElement(None, None, OffsetPosition(_, _)) => () }              
          }
      }
    }
    
    ignore should "complain at the combinator that is applied at the other combinator" in {
      val (env, res) = Interpreter.interpretTreeString("""
f x y = #iAdd (#iDiv x y) y
g x = f x 0
""")(f).run(emptyEnv)
      val (env2, res2) = Interpreter.interpretTermString("g 10")(g3).run(env)
      inside(res2) {
        case Success(noValue: NoValue[U, V, W, C]) =>
          noValue.msg should be ===("divided by zero")
          inside(noValue.stackTrace) { 
            case List(stackTraceElem1, stackTraceElem2, stackTraceElem3) =>
              inside(stackTraceElem1) { case StackTraceElement(None, Some(GlobalSymbol(NonEmptyList("f"))), OffsetPosition(_, _)) => () }
              inside(stackTraceElem2) { case StackTraceElement(None, Some(GlobalSymbol(NonEmptyList("g"))), OffsetPosition(_, _)) => () }
              inside(stackTraceElem3) { case StackTraceElement(None, None, OffsetPosition(_, _)) => () }              
          }
      }
    }
    
    ignore should "complain at the lambda expression" in {
      val (env, res) = Interpreter.interpretTermString("(\\x => #iDiv 1 x) 0")(g3).run(emptyEnv)
      inside(res) {
        case Success(noValue: NoValue[U, V, W, C]) =>
          noValue.msg should be ===("divided by zero")
          inside(noValue.stackTrace) { 
            case List(stackTraceElem1, stackTraceElem2) =>
              inside(stackTraceElem1) { case StackTraceElement(None, None, OffsetPosition(_, _)) => () }
              inside(stackTraceElem2) { case StackTraceElement(None, None, OffsetPosition(_, _)) => () }
          }
      }
    }
    
    it should "interpret the string of the typed term" in {
      val (env, res) = Interpreter.interpretTermString("#iAdd 2 ((#iMul 3 4): ##& (##| #Zero #NonZero) #Int)")(g3).run(emptyEnv)
      res should be ===(IntValue(14).success)      
    }
    
    it should "interpret the string with the construct-expressions" in {
      val (env, res) = Interpreter.interpretTreeString("""
unittype 2 T
unittype 0 U
instance select \t1 t2 => ##| (##& (T t1 t2) (tuple 2 t1 t2)) (##& U tuple 0) construct {
  \t1 t2 => ##& (T t1 t2) (tuple 2 t1 t2)
  ##& U tuple 0
}
""")(f).run(emptyEnv)
      val (env2, res2) = Interpreter.interpretTermString("""
tuple 2 (construct 2 'a' 'b': ##& (T #Char #Char) (tuple 2 #Char #Char)) (construct 0: ##& U tuple 0)
""")(g3).run(env)
      res2 should be ===(TupleValue(Vector(
          ConstructValue(0, Vector(CharValue('a'), CharValue('b'))),
          ConstructValue(1, Vector()))).success)
    }
    
    it should "interpret the string with the select-expression" in {
      val (env, res) = Interpreter.interpretTreeString("""
unittype 2 T
unittype 0 U
unittype 0 V
instance select \t1 t2 => ##| (##| (##& (T t1 t2) (tuple 2 t1 t2)) (##& U tuple 0)) (##& V tuple 0) construct {
  \t1 t2 => ##& (T t1 t2) (tuple 2 t1 t2)
  ##& U tuple 0
  ##& V tuple 0
}
U = (construct 0: ##& U tuple 0): \t1 t2 => ##| (##| (##& (T t1 t2) (tuple 2 t1 t2)) (##& U tuple 0)) (##& V tuple 0)
""")(f).run(emptyEnv)
      val (env2, res2) = Interpreter.interpretTermString("""
U select {
  (x: \t1 t2 => ##& (T t1 t2) (tuple 2 t1 t2)) => 1
  (x: ##& U tuple 0)                           => 2
  (x: ##& V tuple 0)                           => 3
}
""")(g3).run(env)
      res2 should be ===(IntValue(2).success)
    }
    
    it should "interpret the string with the applications of the ad-hoc polymorphic combinators" in {
      val (env, res) = Interpreter.interpretTreeString("""
poly f
poly g
instance f => h
instance f => i
instance g => j
h = #iAdd
i = #lSub
j = 'a'
(k: ##& (##| #Zero #NonZero) #Int) = 1
(l: ##& (##| #Zero #NonZero) #Int) = 2
(m: ##& (##| #Zero #NonZero) #Long) = 4L
(n: ##& (##| #Zero #NonZero) #Long) = 3L
""")(f).run(emptyEnv)
      val (env2, res2) = Interpreter.interpretTermString("""
tuple 3 ((f k l): ##& (##| #Zero #NonZero) #Int) ((f m n): ##& (##| #Zero #NonZero) #Long) (g: #Char)
""")(g3).run(env)
      res2 should be ===(TupleValue(Vector(IntValue(3), LongValue(1L), CharValue('a'))).success)
    }
    
    it should "interpret the string with the applications of the combinators with the instance arguments" in {
      val (env, res) = Interpreter.interpretTreeString("""
f g x y = g (\z => i z x) (j y)
h g x = g (k x) (l g x)
poly i
j x = x select {
    (y: ##& T tuple 0)                               => 1: ##& (##| #Zero #NonZero) #Int
    (y: \t1 t2 t3 => ##& (t1 t2 t3) (tuple 2 t2 t3)) => 2: ##& (##| #Zero #NonZero) #Int
  }
poly k
l g x = g x m
poly m
instance i => n
instance k => o
instance m => p
n = #iMul
o = #zNot
p = true
unittype 0 T
unittype 2 U
instance select ##| (##& T tuple 0) (##& (U #Char #Char) (tuple 2 #Char #Char)) construct {
  ##& T tuple 0
  ##& (U #Char #Char) (tuple 2 #Char #Char)
}
U x y = (construct 2 x y: ##& (U #Char #Char) (tuple 2 #Char #Char)): ##| (##& T tuple 0) (##& (U #Char #Char) (tuple 2 #Char #Char))
""")(f).run(emptyEnv)
      val (env2, res2) = Interpreter.interpretTermString("""
tuple 2 (f (\g => #iAdd (g (3: ##& (##| #Zero #NonZero) #Int))) 2 (U 'a' 'b')) (h #zXor true)
""")(g3).run(env)
      res2 should be ===(TupleValue(Vector(IntValue(8), BooleanValue(false))).success)
    }
    
    it should "interpret the string with the select-expressions for integers" in {
      val (env, res) = Interpreter.interpretTermString("""
(#iAdd 1 2) select {
  (x: ##& #Zero #Int)    => 1
  (x: ##& #NonZero #Int) => 
    (#iSub (#iDiv 9 x) 3) select {
      (y: ##& #Zero #Int)    => 2
      (y: ##& #NonZero #Int) => 3
    }
}
""")(g3).run(emptyEnv)
      res should be ===(IntValue(2).success)
    }
    
    it should "interpret the string with the select-expressions for arrays" in {
      val (env, res) = Interpreter.interpretTermString("""
(#array 0L 'a') select {
  (x: ##& #Empty (#Array #Char))    =>
    (#array 2L 'b') select {
      (y: ##& #Empty (#Array #Char))    => 1
      (y: ##& #NonEmpty (#Array #Char)) => 2
    }
  (x: ##& #NonEmpty (#Array #Char)) => 3
}
""")(g3).run(emptyEnv)
      res should be ===(IntValue(2).success)
    }
  }
  
  "An Interpreter" should behave like interpreter(SymbolEnvironment.empty[instant.LambdaInfo[parser.LambdaInfo, LocalSymbol, GlobalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, kinder.TypeLambdaInfo[parser.TypeLambdaInfo, LocalSymbol]], kinder.TypeLambdaInfo[parser.TypeLambdaInfo, LocalSymbol]], ())(_ => ().successNel)(_ => Interpreter.statefullyTransformToSymbolTree)(_ => Interpreter.transformToSymbolTerm3)
}