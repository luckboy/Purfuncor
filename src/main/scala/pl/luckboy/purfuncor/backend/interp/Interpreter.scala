package pl.luckboy.purfuncor.backend.interp
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.frontend.SimpleTerm
import pl.luckboy.purfuncor.frontend.Combinator
import pl.luckboy.purfuncor.frontend.parser
import pl.luckboy.purfuncor.frontend.resolver
import pl.luckboy.purfuncor.frontend.resolver.Scope
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import Initializer._
import Evaluator._

object Interpreter
{
  def interpretTreeS[E, L, C, I, F](tree: Tree[L, C, I])(env: F)(implicit init: Initializer[E, L, C, F]) =
    initializeS(tree)(env)
  
  def interpretTree[E, L, C, I, F](tree: Tree[L, C, I])(implicit init: Initializer[E, L, C, F]) =
    State(interpretTreeS[E, L, C, I, F](tree))
    
  def interpretTermS[T, E, V](term: Term[T])(env: E)(implicit eval: Evaluator[T, E, V]) =
    evaluateS(term)(env)

  def interpretTerm[T, E, V](term: Term[T])(implicit eval: Evaluator[T, E, V]) =
    State(interpretTermS[T, E, V](term))
  
  def interpretTreeStringS[T, U, V, W, C, E](s: String)(f: Tree[GlobalSymbol, Combinator[Symbol, parser.LetInfo], resolver.TreeInfo] => ValidationNel[AbstractError, Tree[T, Combinator[U, V], W]])(env: E)(implicit init: Initializer[NoValue[U, V, C], T, Combinator[U, V], E], enval: Environmental[E, Value[U, V, C]]) =
    (for {
      tree <- resolver.Resolver.transformString(s)(enval.nameTreeFromEnvironment(env))
      tree2 <- f(tree)
    } yield {
      val (env2, res) = interpretTreeS(tree2)(env)
      (env2, res.success)
    }).valueOr {
      noValue => (env, noValue.failure)      
    }
    
  def interpretTreeString[T, U, V, W, C, E](s: String)(f: Tree[GlobalSymbol, Combinator[Symbol, parser.LetInfo], resolver.TreeInfo] => ValidationNel[AbstractError, Tree[T, Combinator[U, V], W]])(implicit init: Initializer[NoValue[U, V, C], T, Combinator[U, V], E], enval: Environmental[E, Value[U, V, C]]) =
    State(interpretTreeStringS[T, U, V, W, C, E](s)(f))
    
  def interpretTermStringS[T, U, C, E](s: String)(f: Term[SimpleTerm[Symbol, parser.LetInfo]] => ValidationNel[AbstractError, Term[SimpleTerm[T, U]]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U], E, Value[T, U, C]], enval: Environmental[E, Value[T, U, C]]) =
    (for {
      term <- resolver.Resolver.transformTermString(s)(Scope.fromNameTree(enval.nameTreeFromEnvironment(env)))
      term2 <- f(term)
    } yield {
      val (env2, value) = interpretTermS(term2)(env)
      (env2, value.success)
    }).valueOr { 
      noValue => (env, noValue.failure)
    }
    
  def interpretTermString[T, U, C, E](s: String)(f: Term[SimpleTerm[Symbol, parser.LetInfo]] => ValidationNel[AbstractError, Term[SimpleTerm[T, U]]])(implicit eval: Evaluator[SimpleTerm[T, U], E, Value[T, U, C]], enval: Environmental[E, Value[T, U, C]]) =
    State(interpretTermStringS[T, U, C, E](s)(f))  
}