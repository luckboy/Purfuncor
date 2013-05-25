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
      case Success(Tree(combs, TreeInfo)) =>
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
}