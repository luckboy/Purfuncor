package pl.luckboy.purfuncor.typer
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.NameTree
import pl.luckboy.purfuncor.frontend.resolver.Scope
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.common.Evaluator._
import pl.luckboy.purfuncor.common.Initializer._

object Typer
{
  def interpretTypeTermS[T, E, V](term: Term[T])(env: E)(implicit eval: Evaluator[T, E, V]) =
    evaluateS(term)(env)
  
  def interpretTypeTerm[T, E, V](term: Term[T])(implicit eval: Evaluator[T, E, V]) =
    State(interpretTypeTermS[T, E, V](term))
      
  def interpretTypeTreeS[E, L, C, I, F](tree: Tree[L, C, I])(env: F)(implicit init: Initializer[E, L, C, F]) =
    initializeS(tree)(env)
  
  def interpretTypeTree[E, L, C, I, F](tree: Tree[L, C, I])(implicit init: Initializer[E, L, C, F]) =
    State(interpretTypeTreeS[E, L, C, I, F](tree))

  def interpretTypeTreeFromTreeS[T, U, V, E, L, C, I, F](tree: Tree[T, U, V])(env: F)(implicit  init: Initializer[E, L, C, F], treeInfoExtractor: TreeInfoExtractor[V, Tree[L, C, I]]) =
    interpretTypeTreeS(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo))(env)
    
  def interpretTypeTreeFromTree[T, U, V, E, L, C, I, F](tree: Tree[T, U, V])(implicit  init: Initializer[E, L, C, F], treeInfoExtractor: TreeInfoExtractor[V, Tree[L, C, I]]) =
    State(interpretTypeTreeFromTreeS[T, U, V, E, L, C, I, F](tree))
    
  def interpretTypeTreeFromTreeStringS[T, U, V, W, X, Y, Z, C, E](s: String)(nameTree: NameTree)(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => State[E, ValidationNel[AbstractError, Tree[T, U, V]]])(env: E)(implicit init: Initializer[NoTypeValue[W, X, Y, C], W, AbstractTypeCombinator[X, Y], E], treeInfoExtractor: TreeInfoExtractor[V, Tree[W, AbstractTypeCombinator[X, Y], Z]]) =
    resolver.Resolver.transformString(s)(nameTree).map {
      tree =>
        (for {
          res <- f(tree)
          res2 <- res.map { interpretTypeTreeFromTree(_)(init, treeInfoExtractor) }.getOrElse {
            State.state(NoTypeValue.fromError[W, X, Y, C](FatalError("result is failure", none, NoPosition)).failure[Unit])
          }
        } yield { res.map { _ => res2 } }).run(env)
    }.valueOr { errs => (env, errs.failure) }
    
  def interpretTypeTreeFromTreeString[T, U, V, W, X, Y, Z, C, E](s: String)(nameTree: NameTree)(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => State[E, ValidationNel[AbstractError, Tree[T, U, V]]])(implicit init: Initializer[NoTypeValue[W, X, Y, C], W, AbstractTypeCombinator[X, Y], E], treeInfoExtractor: TreeInfoExtractor[V, Tree[W, AbstractTypeCombinator[X, Y], Z]]) =
    State(interpretTypeTreeFromTreeStringS[T, U, V, W, X, Y, Z, C, E](s)(nameTree)(f))
    
  def interpretTypeTermStringS[T, E, V](s: String)(nameTree: NameTree)(f: Term[TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]] => ValidationNel[AbstractError, Term[T]])(env: E)(implicit eval: Evaluator[T, E, V]) =
    (for {
      term <- resolver.Resolver.transformTypeTermString(s)(Scope.fromNameTree(nameTree))
      term2 <- f(term)
    } yield {
      val (env2, value) = interpretTypeTermS(term2)(env)
      (env2, value.success)
    }).valueOr { errs => (env, errs.failure) }
  
  def interpretTypeTermStrong[T, E, V](s: String)(nameTree: NameTree)(f: Term[TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]] => ValidationNel[AbstractError, Term[T]])(implicit eval: Evaluator[T, E, V]) =
    State(interpretTypeTermStringS[T, E, V](s)(nameTree)(f))
}