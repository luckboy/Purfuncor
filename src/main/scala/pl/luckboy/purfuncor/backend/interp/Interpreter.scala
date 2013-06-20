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
  
  def interpretTreeStringS[T, U, V, C, E](s: String)(f: Tree[GlobalSymbol, Combinator[Symbol, parser.LetInfo], resolver.TreeInfo] => ValidationNel[AbstractError, Tree[T, U, V]])(env: E)(implicit init: Initializer[NoValue[Symbol, parser.LetInfo, C], T, U, E], enval: Environmental[E]) =
    (for {
      tree <- resolver.Resolver.transformString(s)(enval.nameTreeFromEnvironment(env))
      tree2 <- f(tree)
    } yield {
      val (env2, value) = interpretTreeS(tree2)(env)
      (env2, value.success)
    }).valueOr {
      noValue => (env, noValue.failure)      
    }
    
  def interpretTreeString[T, U, V, C, E](s: String)(f: Tree[GlobalSymbol, Combinator[Symbol, parser.LetInfo], resolver.TreeInfo] => ValidationNel[AbstractError, Tree[T, U, V]])(implicit init: Initializer[NoValue[Symbol, parser.LetInfo, C], T, U, E], enval: Environmental[E]) =
    State(interpretTreeStringS[T, U, V, C, E](s)(f))
    
  def interpretTermStringS[T, C, E](s: String)(f: Term[SimpleTerm[Symbol, parser.LetInfo]] => ValidationNel[AbstractError, Term[T]])(env: E)(implicit eval: Evaluator[T, E, Value[Symbol, parser.LetInfo, C]], enval: Environmental[E]) =
    (for {
      term <- resolver.Resolver.transformTermString(s)(Scope.fromNameTree(enval.nameTreeFromEnvironment(env)))
      term2 <- f(term)
    } yield {
      val (env2, value) = interpretTermS(term2)(env)
      (env2, value.success)
    }).valueOr { 
      noValue => (env, noValue.failure)
    }
    
  def interpretTermString[T, C, E](s: String)(f: Term[SimpleTerm[Symbol, parser.LetInfo]] => ValidationNel[AbstractError, Term[T]])(implicit eval: Evaluator[T, E, Value[Symbol, parser.LetInfo, C]], enval: Environmental[E]) =
    State(interpretTermStringS[T, C, E](s)(f))  
}