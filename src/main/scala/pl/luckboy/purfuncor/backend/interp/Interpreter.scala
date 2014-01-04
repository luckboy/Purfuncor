package pl.luckboy.purfuncor.backend.interp
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.frontend.SimpleTerm
import pl.luckboy.purfuncor.frontend.TypeSimpleTerm
import pl.luckboy.purfuncor.frontend.AbstractCombinator
import pl.luckboy.purfuncor.frontend.parser
import pl.luckboy.purfuncor.frontend.kinder
import pl.luckboy.purfuncor.frontend.instant
import pl.luckboy.purfuncor.frontend.resolver
import pl.luckboy.purfuncor.frontend.resolver.Scope
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol
import pl.luckboy.purfuncor.frontend.typer.SymbolTypeEnvironment
import pl.luckboy.purfuncor.frontend.instant.SymbolInstantiationEnvironment
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
  
  def interpretTreeStringS[T, U, V, W, X, C, E](s: String)(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => State[E, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, V, W], X]]])(env: E)(implicit init: Initializer[NoValue[U, V, W, C], T, AbstractCombinator[U, V, W], E], envSt: EnvironmentState[E, T, Value[U, V, W, C], InstanceValue[U, V, W, C]]) = {
    val (env2, nameTree) = envSt.nameTreeFromEnvironmentS(env)
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
    
  def interpretTreeString[T, U, V, W, X, C, E](s: String)(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => State[E, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, V, W], X]]])(implicit init: Initializer[NoValue[U, V, W, C], T, AbstractCombinator[U, V, W], E], envSt: EnvironmentState[E, T, Value[U, V, W, C], InstanceValue[U, V, W, C]]) =
    State(interpretTreeStringS[T, U, V, W, X, C, E](s)(f))

  def interpretTreeFilesS[T, U, V, W, X, C, E](files: List[java.io.File])(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => State[E, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, V, W], X]]])(env: E)(implicit init: Initializer[NoValue[U, V, W, C], T, AbstractCombinator[U, V, W], E], envSt: EnvironmentState[E, T, Value[U, V, W, C], InstanceValue[U, V, W, C]]) = {
    val (env2, nameTree) = envSt.nameTreeFromEnvironmentS(env)
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
  
  def interpretTreeFiles[T, U, V, W, X, C, E](files: List[java.io.File])(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => State[E, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, V, W], X]]])(implicit init: Initializer[NoValue[U, V, W, C], T, AbstractCombinator[U, V, W], E], envSt: EnvironmentState[E, T, Value[U, V, W, C], InstanceValue[U, V, W, C]]) =
    State(interpretTreeFilesS[T, U, V, W, X, C, E](files)(f))
    
  def interpretTermStringS[T, U, V, W, C, E](s: String)(f: (Term[SimpleTerm[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]]], E) => ValidationNel[AbstractError, Term[SimpleTerm[T, U, V]]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, C]], envSt: EnvironmentState[E, W, Value[T, U, V, C], InstanceValue[T, U, V, C]]) = {
    val (env2, nameTree) = envSt.nameTreeFromEnvironmentS(env)
    (for {
      term <- resolver.Resolver.transformTermString(s)(Scope.fromNameTree(nameTree))
      term2 <- f(term, env)
    } yield {
      val (env3, value) = interpretTermS(term2)(env2)
      (env3, value.success)
    }).valueOr { 
      noValue => (env2, noValue.failure)
    }
  }
  
  def interpretTermString[T, U, V, W, C, E](s: String)(f: (Term[SimpleTerm[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]]], E) => ValidationNel[AbstractError, Term[SimpleTerm[T, U, V]]])(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, C]], envSt: EnvironmentState[E, W, Value[T, U, V, C], InstanceValue[T, U, V, C]]) =
    State(interpretTermStringS[T, U, V, W, C, E](s)(f))
    
  val statefullyTransformToSymbolTree = {
    (tree: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]]) =>
      State({
        (env: SymbolEnvironment[instant.LambdaInfo[parser.LambdaInfo, LocalSymbol, GlobalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, kinder.TypeLambdaInfo[parser.TypeLambdaInfo, LocalSymbol]], kinder.TypeLambdaInfo[parser.TypeLambdaInfo, LocalSymbol]]) => 
          val (typeEnv, res3) = (for {
            res <- instant.Instantiator.statefullyTransformToSymbolTree3(tree, env.kindTable, env.typeTable)
            res2 <- res.map {
              instant.Instantiator.transform(_)(env.kindTable, env.typeTable, env.instTree, env.instArgTable)(instant.Instantiator.statefullyMakeSymbolTypeInferenceEnvironment3)
            }.valueOr { errs => State((_: SymbolTypeEnvironment[kinder.TypeLambdaInfo[parser.TypeLambdaInfo, LocalSymbol]], errs.failure)) }
        } yield res2).run(env.typeEnv)
        res3.map {
          t => (env.copy(typeEnv = typeEnv, kindTable = t.treeInfo.treeInfo.typeTree.treeInfo.kindTable, typeTable = t.treeInfo.typeTable, instArgTable = t.treeInfo.instArgTable), t.successNel)
        }.valueOr { errs => (env, errs.failure) }
      })
  }
  
  val transformToSymbolTerm3 = {
    (term: Term[SimpleTerm[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]]], env: SymbolEnvironment[instant.LambdaInfo[parser.LambdaInfo, LocalSymbol, GlobalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, kinder.TypeLambdaInfo[parser.TypeLambdaInfo, LocalSymbol]], kinder.TypeLambdaInfo[parser.TypeLambdaInfo, LocalSymbol]]) =>
      for {
        pair <- instant.Instantiator.transformToSymbolTerm2(env.kindTable, env.typeTable, env.typeEnv)(term)
        term <- instant.Instantiator.transformTermWithInstantiation(pair._1)(SymbolInstantiationEnvironment.fromInstanceTree[parser.LambdaInfo, parser.TypeLambdaInfo](env.instTree).withInstArgs(env.instArgTable.instArgs))
      } yield term
  }
}