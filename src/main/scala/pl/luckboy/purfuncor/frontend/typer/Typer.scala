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
import pl.luckboy.purfuncor.frontend.kinder.InferredKindTable
import pl.luckboy.purfuncor.frontend.kinder.TypeLambdaInfo
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.common.Evaluator._
import pl.luckboy.purfuncor.common.Initializer._

object Typer
{
  //
  // A type interpreter.
  //
  
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
  
  //
  // A type inferrer.
  //
  
  def transfromTerm[T, U, V, W, X, Y, Z, TT, E](term: Term[SimpleTerm[T, lmbdindexer.LambdaInfo[U], TypeSimpleTerm[V, TypeLambdaInfo[W, X]]]])(env: E)(implicit enval: TypeInferenceEnvironmental[E, Y, Z, TT]): ValidationNel[AbstractError, Term[SimpleTerm[T, LambdaInfo[U, Z, Y], TypeSimpleTerm[V, W]]]] =
    throw new UnsupportedOperationException
  
  def transfromTree[T, U, V, W, X, Y, Z, TT, TU, E](tree: Tree[T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], Z])(implicit enval: TypeInferenceEnvironmental[E, T, TT, TU]): ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, LambdaInfo[V, T, TT], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], TreeInfo[Z, T, TT]]] =
    throw new UnsupportedOperationException
  
  def inferTermTypesS[T, U, V, W, X, Y, E](term: Term[SimpleTerm[T, lmbdindexer.LambdaInfo[U], TypeSimpleTerm[V, TypeLambdaInfo[W, X]]]])(env: E)(implicit inferrer: Inferrer[SimpleTerm[T, lmbdindexer.LambdaInfo[U], TypeSimpleTerm[V, TypeLambdaInfo[W, X]]], E, Type[Y]], envSt: TypeInferenceEnvironmentState[E, Y]): (E, Type[Y]) =
    throw new UnsupportedOperationException
    
  def inferTermTypes[T, U, V, W, X, Y, E](term: Term[SimpleTerm[T, lmbdindexer.LambdaInfo[U], TypeSimpleTerm[V, TypeLambdaInfo[W, X]]]])(implicit inferrer: Inferrer[SimpleTerm[T, lmbdindexer.LambdaInfo[U], TypeSimpleTerm[V, TypeLambdaInfo[W, X]]], E, Type[Y]], envSt: TypeInferenceEnvironmentState[E, Y]) =
    State(inferTermTypesS[T, U, V, W, X, Y, E](term))
    
  def inferTreeTypesS[E, L, C, I, F](tree: Tree[L, C, I])(env: F)(implicit init: Initializer[E, L, C, F]): (F, Validation[E, Unit]) =
    throw new UnsupportedOperationException
  
  def inferTreeTypes[E, L, C, I, F](tree: Tree[L, C, I])(implicit init: Initializer[E, L, C, F]) =
    State(inferTreeTypesS[E, L, C, I, F](tree))
  
  def transformS[T, U, V, W, X, Y, Z, TT, TU, E, TC, TE](tree: Tree[T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], Z])(kindTable: InferredKindTable[TT], typeTable: InferredTypeTable[T, TT])(f: (InferredKindTable[TT], InferredTypeTable[T, TT]) => State[TE, E])(typeEnv: TE)(implicit init: Initializer[NoType[TT], T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], E], inferrer: Inferrer[SimpleTerm[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], E, Type[TT]], typeInit: Initializer[NoTypeValue[TT, W, TypeLambdaInfo[X, Y], TC], TT, AbstractTypeCombinator[W, TypeLambdaInfo[X, Y]], TE], envSt: TypeInferenceEnvironmentState[E, TT], enval: TypeInferenceEnvironmental[E, T, TU, TT]): (TE, Validation[NoType[TT], ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, LambdaInfo[V, T, TU], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], TreeInfo[Z, T, TU]]]]) =
    throw new UnsupportedOperationException
  
  def transform[T, U, V, W, X, Y, Z, TT, TU, E, TC, TE](tree: Tree[T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], Z])(kindTable: InferredKindTable[TT], typeTable: InferredTypeTable[T, TT])(f: (InferredKindTable[TT], InferredTypeTable[T, TT]) => State[TE, E])(implicit init: Initializer[NoType[TT], T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], E], inferrer: Inferrer[SimpleTerm[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], E, Type[TT]], typeInit: Initializer[NoTypeValue[TT, W, TypeLambdaInfo[X, Y], TC], TT, AbstractTypeCombinator[W, TypeLambdaInfo[X, Y]], TE], envSt: TypeInferenceEnvironmentState[E, TT], enval: TypeInferenceEnvironmental[E, T, TU, TT]) =
    State(transformS[T, U, V, W, X, Y, Z, TT, TU, E, TC, TE](tree)(kindTable, typeTable)(f))
}