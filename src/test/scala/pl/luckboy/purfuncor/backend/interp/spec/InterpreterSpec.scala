package pl.luckboy.purfuncor.backend.interp.spec
import scalaz._
import scalaz.Scalaz._
import org.scalatest.FlatSpec
import org.scalatest.Inside
import org.scalatest.matchers.ShouldMatchers
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend.Combinator
import pl.luckboy.purfuncor.frontend.SimpleTerm
import pl.luckboy.purfuncor.frontend.parser
import pl.luckboy.purfuncor.frontend.resolver
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.backend.interp._
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.backend.interp.Value

class InterpreterSpec extends FlatSpec with ShouldMatchers with Inside
{
  def interpreter[T, U, V, W, C, E](emptyEnv: E)(f: Tree[GlobalSymbol, Combinator[Symbol, parser.LetInfo], resolver.TreeInfo] => ValidationNel[AbstractError, Tree[T, Combinator[U, V], W]], g: Term[SimpleTerm[Symbol, parser.LetInfo]] => ValidationNel[AbstractError, Term[SimpleTerm[U, V]]])(implicit init: Initializer[NoValue[U, V, C], T, Combinator[U, V], E], eval: Evaluator[SimpleTerm[U, V], E, Value[U, V, C]], enval: Environmental[E, Value[U, V, C]])
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
  }
  
  "A Interpreter" should behave like interpreter(SymbolEnvironment.empty[parser.LetInfo])(_.successNel, _.successNel)
}