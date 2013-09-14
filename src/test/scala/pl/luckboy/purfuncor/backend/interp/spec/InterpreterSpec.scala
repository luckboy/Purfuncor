package pl.luckboy.purfuncor.backend.interp.spec
import scala.util.parsing.input.OffsetPosition
import scalaz._
import scalaz.Scalaz._
import org.scalatest.FlatSpec
import org.scalatest.Inside
import org.scalatest.matchers.ShouldMatchers
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend.SimpleTerm
import pl.luckboy.purfuncor.frontend.TypeSimpleTerm
import pl.luckboy.purfuncor.frontend.AbstractCombinator
import pl.luckboy.purfuncor.frontend.parser
import pl.luckboy.purfuncor.frontend.resolver
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.backend.interp._
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.backend.interp.Value

class InterpreterSpec extends FlatSpec with ShouldMatchers with Inside
{
  def interpreter[T, U, V, W, X, C, E](emptyEnv: E)(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => State[E, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, V, W], X]]])(g: Term[SimpleTerm[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]]] => ValidationNel[AbstractError, Term[SimpleTerm[U, V, W]]])(implicit init: Initializer[NoValue[U, V, W, C], T, AbstractCombinator[U, V, W], E], eval: Evaluator[SimpleTerm[U, V, W], E, Value[U, V, W, C]], envSt: EnvironmentState[E], enval: Environmental[E, Value[U, V, W, C]])
  {
    it should "interpret the term string" in {
      val (env, res) = Interpreter.interpretTermString("#iAdd 2 (#iMul 3 4)")(g).run(emptyEnv)
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
j x y = #iDiv (#iAdd 6 x) (#iSub y k)
k = #iAdd (l 7) (h 8)
l x = #iAdd x 3
""")(f).run(emptyEnv)
      res should be ===(().success.success)
      enval.globalVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("f"))) should be ===(IntValue(12))
      inside(enval.globalVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("g")))) { case CombinatorValue(_, _) => () }
      inside(enval.globalVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("h")))) { case CombinatorValue(_, _) => () }
      enval.globalVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("i"))) should be ===(IntValue(5))
      inside(enval.globalVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("j")))) { case CombinatorValue(_, _) => () }
      enval.globalVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("k"))) should be ===(IntValue(7))
      inside(enval.globalVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("l")))) { case CombinatorValue(_, _) => () }
    }
    
    it should "interpret the term string with the global variables" in {
      val (env, res) = Interpreter.interpretTreeString("""
f = 1
g = 2
h x = #iMul x 3 
""")(f).run(emptyEnv)
      val (env2, res2) = Interpreter.interpretTermString("#iAdd f (#iSub (h 10) g)")(g).run(env)
      res2 should be ===(IntValue(29).success)
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
  a = 1
  b = 2
in
  #iMul (let
    a = 3
    b = 4
  in
    #iAdd a b) (#iAdd a b)
""")(g).run(emptyEnv)
      res should be ===(IntValue(21).success)
    }
    
    it should "complain at the term" in {
      val (env, res) = Interpreter.interpretTermString("#iAdd (#iDiv 1 0) 2")(g).run(emptyEnv)
      inside(res) {
        case Success(noValue: NoValue[U, V, W, C]) =>
          noValue.msg should be ===("divided by zero")
          inside(noValue.stackTrace) { case List(StackTraceElement(None, None, OffsetPosition(_, _))) => () }
      }
    }
    
    it should "complain at the combinator without the arguments" in {
      val (env, res) = Interpreter.interpretTreeString("f = #iSub 1 (#iDiv 2 0)")(f).run(emptyEnv)
      inside(res) {
        case Success(Failure(noValue)) =>
          noValue.msg should be ===("divided by zero")
          inside(noValue.stackTrace) { case List(StackTraceElement(None, Some(GlobalSymbol(NonEmptyList("f"))), OffsetPosition(_, _))) => () }
      }
    }
    
    it should "complain at the combinator with the two arguments" in {
      val (env, res) = Interpreter.interpretTreeString("f x y = #iAdd (#iDiv x y) y")(f).run(emptyEnv)
      val (env2, res2) = Interpreter.interpretTermString("f 10 0")(g).run(env)
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
    
    it should "complain at the combinator that is applied at the other combinator" in {
      val (env, res) = Interpreter.interpretTreeString("""
f x y = #iAdd (#iDiv x y) y
g x = f x 0
""")(f).run(emptyEnv)
      val (env2, res2) = Interpreter.interpretTermString("g 10")(g).run(env)
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
    
    it should "complain at the lambda expression" in {
      val (env, res) = Interpreter.interpretTermString("(\\x => #iDiv 1 x) 0")(g).run(emptyEnv)
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
      val (env, res) = Interpreter.interpretTermString("#iAdd 2 ((#iMul 3 4): #Int)")(g).run(emptyEnv)
      res should be ===(IntValue(14).success)      
    }
  }
  
  "An Interpreter" should behave like interpreter(SymbolEnvironment.empty[parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]])(Interpreter.statefullyTransformToSymbolTree)(Interpreter.transformToSymbolTerm)
}