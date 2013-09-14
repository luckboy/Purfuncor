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
  
  def interpretTreeStringS[T, U, V, W, X, C, E](s: String)(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => State[E, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, V, W], X]]])(env: E)(implicit init: Initializer[NoValue[U, V, W, C], T, AbstractCombinator[U, V, W], E], envSt: EnvironmentState[E]) = {
    val (env2, nameTree) = envSt.nameTreeFromEnvironment(env)
    resolver.Resolver.transformString(s)(nameTree).map {
      tree =>
        (for {
          res <- f(tree)
          res2 <- res.map { tree2 => interpretTree(tree2) }.getOrElse {
            State.state(NoValue.fromString[U, V, W, C]("result is failure").failure[Unit])
          }
        } yield { res.map { _ => res2 } }).run(env2)
    }.valueOr {
      errs => (env2, errs.failure)      
    }
  }
    
  def interpretTreeString[T, U, V, W, X, C, E](s: String)(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => State[E, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, V, W], X]]])(implicit init: Initializer[NoValue[U, V, W, C], T, AbstractCombinator[U, V, W], E], envSt: EnvironmentState[E]) =
    State(interpretTreeStringS[T, U, V, W, X, C, E](s)(f))

  def interpretTreeFilesS[T, U, V, W, X, C, E](files: List[java.io.File])(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => State[E, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, V, W], X]]])(env: E)(implicit init: Initializer[NoValue[U, V, W, C], T, AbstractCombinator[U, V, W], E], envSt: EnvironmentState[E]) = {
    val (env2, nameTree) = envSt.nameTreeFromEnvironment(env)
    resolver.Resolver.transformFiles(files)(nameTree).map {
      tree =>
        (for {
          res <- f(tree)
          res2 <- res.map { tree2 => interpretTree(tree2) }.getOrElse {
            State.state(NoValue.fromString[U, V, W, C]("result is failure").failure[Unit])
          }
        } yield { res.map { _ => res2 } }).run(env2)
    }.valueOr {
      errs => (env2, errs.failure)      
    }
  }
  
  def interpretTreeFiles[T, U, V, W, X, C, E](files: List[java.io.File])(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => State[E, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, V, W], X]]])(implicit init: Initializer[NoValue[U, V, W, C], T, AbstractCombinator[U, V, W], E], envSt: EnvironmentState[E]) =
    State(interpretTreeFilesS[T, U, V, W, X, C, E](files)(f))
    
  def interpretTermStringS[T, U, W, C, E](s: String)(f: Term[SimpleTerm[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]]] => ValidationNel[AbstractError, Term[SimpleTerm[T, U, W]]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, W], E, Value[T, U, W, C]], envSt: EnvironmentState[E]) = {
    val (env2, nameTree) = envSt.nameTreeFromEnvironment(env)
    (for {
      term <- resolver.Resolver.transformTermString(s)(Scope.fromNameTree(nameTree))
      term2 <- f(term)
    } yield {
      val (env3, value) = interpretTermS(term2)(env2)
      (env3, value.success)
    }).valueOr { 
      noValue => (env2, noValue.failure)
    }
  }
  
  def interpretTermString[T, U, W, C, E](s: String)(f: Term[SimpleTerm[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]]] => ValidationNel[AbstractError, Term[SimpleTerm[T, U, W]]])(implicit eval: Evaluator[SimpleTerm[T, U, W], E, Value[T, U, W, C]], envSt: EnvironmentState[E]) =
    State(interpretTermStringS[T, U, W, C, E](s)(f))
    
  val statefullyTransformToSymbolTree = {
    (tree: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]]) =>
      State((env: SymbolEnvironment[parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]]) =>
        (env.copy(typeCombSyms = tree.treeInfo.typeTree.combs.keySet), tree.successNel[AbstractError]))
  }
  
  val transformToSymbolTerm = (_: Term[SimpleTerm[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]]]).successNel[AbstractError]
}