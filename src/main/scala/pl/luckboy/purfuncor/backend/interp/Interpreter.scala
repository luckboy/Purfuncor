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
  
  def interpretTreeStringS[T, E](s: String)(env: E)(implicit init: Initializer[NoValue[Symbol, parser.LetInfo, T], GlobalSymbol, Combinator[Symbol, parser.LetInfo], E], enval: Environmental[E]) =
    resolver.Resolver.transformString(s)(enval.nameTreeFromEnvironment(env)).map {
      tree =>
        val (env2, value) = interpretTreeS(tree)(env)
        (env2, value.success)
    }.valueOr {
      noValue => (env, noValue.failure)      
    }
    
  def interpretTreeString[T, E](s: String)(implicit init: Initializer[NoValue[Symbol, parser.LetInfo, T], GlobalSymbol, Combinator[Symbol, parser.LetInfo], E], enval: Environmental[E]) =
    State(interpretTreeStringS[T, E](s))
    
  def interpretTermStringS[T, E](s: String)(env: E)(implicit eval: Evaluator[SimpleTerm[Symbol, parser.LetInfo], E, Value[Symbol, parser.LetInfo, T]], enval: Environmental[E]) =
    resolver.Resolver.transformTermString(s)(Scope.fromNameTree(enval.nameTreeFromEnvironment(env))).map {
      term =>
        val (env2, value) = interpretTermS(term)(env)
        (env2, value.success)
    }.valueOr { 
      noValue => (env, noValue.failure)
    }
    
  def interpretTermString[T, E](s: String)(implicit eval: Evaluator[SimpleTerm[Symbol, parser.LetInfo], E, Value[Symbol, parser.LetInfo, T]], enval: Environmental[E]) =
    State(interpretTermStringS[T, E](s))  
}