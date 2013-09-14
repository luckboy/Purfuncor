package pl.luckboy.purfuncor.frontend.typer
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol
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
    
  def interpretTypeTreeFromTreeStringS[T, U, V, W, X, Y, Z, TT, C, E](s: String)(nameTree: NameTree)(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => State[E, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, V, TypeSimpleTerm[W, X]], Y]]])(env: E)(implicit init: Initializer[NoTypeValue[Z, W, X, C], Z, AbstractTypeCombinator[W, X], E], treeInfoExtractor: TreeInfoExtractor[Y, Tree[Z, AbstractTypeCombinator[W, X], TT]]) =
    resolver.Resolver.transformString(s)(nameTree).map {
      tree =>
        (for {
          res <- f(tree)
          res2 <- res.map { tree2 => interpretTypeTreeFromTree(tree2)(init, treeInfoExtractor) }.getOrElse {
            State.state(NoTypeValue.fromError[Z, W, X, C](FatalError("result is failure", none, NoPosition)).failure[Unit])
          }
        } yield { res.map { _ => res2 } }).run(env)
    }.valueOr { errs => (env, errs.failure) }
    
  def interpretTypeTreeFromTreeString[T, U, V, W, X, Y, Z, TT, C, E](s: String)(nameTree: NameTree)(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => State[E, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, V, TypeSimpleTerm[W, X]], Y]]])(implicit init: Initializer[NoTypeValue[Z, W, X, C], Z, AbstractTypeCombinator[W, X], E], treeInfoExtractor: TreeInfoExtractor[Y, Tree[Z, AbstractTypeCombinator[W, X], TT]]) =
    State(interpretTypeTreeFromTreeStringS[T, U, V, W, X, Y, Z, TT, C, E](s)(nameTree)(f))
    
  def interpretTypeTermStringS[T, U, V, C, E](s: String)(nameTree: NameTree)(f: Term[TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]] => ValidationNel[AbstractError, Term[TypeSimpleTerm[T, U]]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[T, U], E, TypeValue[V, T, U, C]]) =
    (for {
      term <- resolver.Resolver.transformTypeTermString(s)(Scope.fromNameTree(nameTree))
      term2 <- f(term)
    } yield {
      val (env2, value) = interpretTypeTermS(term2)(env)
      (env2, value.success)
    }).valueOr { errs => (env, errs.failure) }
  
  def interpretTypeTermString[T, U, V, C, E](s: String)(nameTree: NameTree)(f: Term[TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]] => ValidationNel[AbstractError, Term[TypeSimpleTerm[T, U]]])(implicit eval: Evaluator[TypeSimpleTerm[T, U], E, TypeValue[V, T, U, C]]) =
    State(interpretTypeTermStringS[T, U, V, C, E](s)(nameTree)(f))
    
  val statefullyTransformToSymbolTree = {
    (tree: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]]) =>
      State((e: SymbolTypeEnvironment[parser.TypeLambdaInfo]) => (e, tree.successNel[AbstractError]))
  }
  
  def statefullyTransformToSymbolTree2(kindTable: kinder.InferredKindTable[GlobalSymbol]) = {
    (tree: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]]) =>
      State({
        (e: SymbolTypeEnvironment[kinder.TypeLambdaInfo[parser.TypeLambdaInfo, LocalSymbol]]) =>
        val tree4 = (for {
          tree2 <- lmbdindexer.LambdaIndexer.transform(tree)
          tree3 <- kinder.Kinder.transform(tree2)(kindTable) { (kt: kinder.InferredKindTable[GlobalSymbol]) => kinder.SymbolKindInferenceEnvironment.fromInferredKindTable[parser.TypeLambdaInfo](kt) }
        } yield (tree3))
        (e, tree4)
      })
  }
  
  val transformToSymbolTypeTerm = {
    (term: Term[TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]]) => term.successNel[AbstractError]
  }
  
  def transformToSymbolTypeTerm2(kindTable: kinder.InferredKindTable[GlobalSymbol]) = {
    (term: Term[TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]]) =>
      (for {
        term2 <- lmbdindexer.LambdaIndexer.transformTypeTerm(term)
        term3 <- kinder.Kinder.transformTypeTermWithKindInference(term2)(kinder.SymbolKindInferenceEnvironment.fromInferredKindTable[parser.TypeLambdaInfo](kindTable))
      } yield term3).map { _._1 }
  }
}