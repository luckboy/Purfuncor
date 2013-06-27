package pl.luckboy.purfuncor.backend.interp
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.frontend.SimpleTerm
import pl.luckboy.purfuncor.frontend.TypeSimpleTerm
import pl.luckboy.purfuncor.frontend.AbstractCombinator
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
  
  def interpretTreeStringS[T, U, V, W, X, C, E](s: String)(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => State[E, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, V, W], X]]])(env: E)(implicit init: Initializer[NoValue[U, V, W, C], T, AbstractCombinator[U, V, W], E], enval: Environmental[E, Value[U, V, W, C]]) =
    resolver.Resolver.transformString(s)(enval.nameTreeFromEnvironment(env)).map {
      tree =>
        (for {
          res <- f(tree)
          res2 <- res.map { tree2 => interpretTree(tree2) }.getOrElse {
            State.state(NoValue.fromString[U, V, W, C]("result is failure").failure[Unit])
          }
        } yield { res.map { _ => res2 } }).run(env)
    }.valueOr {
      errs => (env, errs.failure)      
    }
    
  def interpretTreeString[T, U, V, W, X, C, E](s: String)(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => State[E, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, V, W], X]]])(implicit init: Initializer[NoValue[U, V, W, C], T, AbstractCombinator[U, V, W], E], enval: Environmental[E, Value[U, V, W, C]]) =
    State(interpretTreeStringS[T, U, V, W, X, C, E](s)(f))

  def interpretTreeFilesS[T, U, V, W, X, C, E](files: List[java.io.File])(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => State[E, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, V, W], X]]])(env: E)(implicit init: Initializer[NoValue[U, V, W, C], T, AbstractCombinator[U, V, W], E], enval: Environmental[E, Value[U, V, W, C]]) =
    resolver.Resolver.transformFiles(files)(enval.nameTreeFromEnvironment(env)).map {
      tree =>
        (for {
          res <- f(tree)
          res2 <- res.map { tree2 => interpretTree(tree2) }.getOrElse {
            State.state(NoValue.fromString[U, V, W, C]("result is failure").failure[Unit])
          }
        } yield { res.map { _ => res2 } }).run(env)
    }.valueOr {
      errs => (env, errs.failure)      
    }
  
  def interpretTreeFiles[T, U, V, W, X, C, E](files: List[java.io.File])(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => State[E, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, V, W], X]]])(implicit init: Initializer[NoValue[U, V, W, C], T, AbstractCombinator[U, V, W], E], enval: Environmental[E, Value[U, V, W, C]]) =
    State(interpretTreeFilesS[T, U, V, W, X, C, E](files)(f))
    
  def interpretTermStringS[T, U, W, C, E](s: String)(f: Term[SimpleTerm[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]]] => ValidationNel[AbstractError, Term[SimpleTerm[T, U, W]]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, W], E, Value[T, U, W, C]], enval: Environmental[E, Value[T, U, W, C]]) =
    (for {
      term <- resolver.Resolver.transformTermString(s)(Scope.fromNameTree(enval.nameTreeFromEnvironment(env)))
      term2 <- f(term)
    } yield {
      val (env2, value) = interpretTermS(term2)(env)
      (env2, value.success)
    }).valueOr { 
      noValue => (env, noValue.failure)
    }
  
  def interpretTermString[T, U, W, C, E](s: String)(f: Term[SimpleTerm[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]]] => ValidationNel[AbstractError, Term[SimpleTerm[T, U, W]]])(implicit eval: Evaluator[SimpleTerm[T, U, W], E, Value[T, U, W, C]], enval: Environmental[E, Value[T, U, W, C]]) =
    State(interpretTermStringS[T, U, W, C, E](s)(f))  
}