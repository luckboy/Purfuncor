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
import pl.luckboy.purfuncor.frontend.kinder.TypeTreeInfo
import pl.luckboy.purfuncor.frontend.kinder.SymbolKindInferenceEnvironment
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.frontend.Bind
import pl.luckboy.purfuncor.common.Evaluator._
import pl.luckboy.purfuncor.common.Inferrer._
import pl.luckboy.purfuncor.common.Initializer._
import pl.luckboy.purfuncor.common.Result._
import TypeResult._

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
  
  def statefullyTransformToSymbolTree2(kindTable: InferredKindTable[GlobalSymbol]) = {
    (tree: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]]) =>
      State({
        (e: SymbolTypeEnvironment[TypeLambdaInfo[parser.TypeLambdaInfo, LocalSymbol]]) =>
          val tree4 = (for {
            tree2 <- lmbdindexer.LambdaIndexer.transform(tree)
            tree3 <- kinder.Kinder.transform(tree2)(kindTable) { (kt: InferredKindTable[GlobalSymbol]) => SymbolKindInferenceEnvironment.fromInferredKindTable[parser.TypeLambdaInfo](kt) }
          } yield (tree3))
          (e, tree4)
      })
  }
  
  val transformToSymbolTypeTerm = {
    (term: Term[TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]]) => term.successNel[AbstractError]
  }
  
  def transformToSymbolTypeTerm2(kindTable: InferredKindTable[GlobalSymbol]) = {
    (term: Term[TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]]) =>
      (for {
        term2 <- lmbdindexer.LambdaIndexer.transformTypeTerm(term)
        term3 <- kinder.Kinder.transformTypeTermWithKindInference(term2)(SymbolKindInferenceEnvironment.fromInferredKindTable[parser.TypeLambdaInfo](kindTable))
      } yield term3).map { _._1 }
  }
  
  //
  // A type inferrer.
  //
  
  def transformTypeTable[T, U](typeTable: TypeTable[T, U]) =
    typeTable.types.foldLeft(Map[T, InferredType[U]]().successNel[AbstractError]) {
      case (Success(ts), (l, t: InferredType[U])) => (ts + (l -> t)).successNel
      case (Success(_), _)                        => FatalError("can't instantiate type", none, NoPosition).failureNel
      case (Failure(errs), _)                     => errs.failure
    }.map { InferredTypeTable(_) }
  
  def transformTypes[T](types: Seq[Type[T]]) =
    types.foldLeft(Seq[InferredType[T]]().successNel[AbstractError]) {
      case (Success(ts), t: InferredType[T]) => (ts :+ t).successNel
      case (Success(_), _)                   => FatalError("can't instantiate type", none, NoPosition).failureNel
      case (Failure(errs), _)                => errs.failure
    }
    
  def transformLambdaInfo[T, U, V, W, E](lambdaInfo: lmbdindexer.LambdaInfo[T])(env: E)(implicit enval: TypeInferenceEnvironmental[E, U, V, W]) =
    lambdaInfo match {
      case lmbdindexer.LambdaInfo(lambdaInfo2, lambdaIdx) =>
        enval.getLambdaInfoFromEnvironment(env)(lambdaIdx).map {
          inferenceLambdaInfo =>
            for {
              tt2 <- transformTypeTable(inferenceLambdaInfo.typeTable)
              ts2 <- transformTypes(inferenceLambdaInfo.instTypes)
            } yield LambdaInfo(lambdaInfo2, lambdaIdx, tt2, ts2)
        }.getOrElse(FatalError("incorrect lambda index", none, NoPosition).failureNel)
    }
  
  private def transformTermNel1[T, U, E](terms: NonEmptyList[T])(env: E)(transform: (T, E) => ValidationNel[AbstractError, U]) =
    terms.tail.foldLeft(transform(terms.head, env).map { NonEmptyList(_) }) {
      case (Success(ts), t)   => transform(t, env).map { _ <:: ts }
      case (Failure(errs), _) => errs.failure
    }.map { _.reverse }
  
  def transformTermNel[T, U, V, W, X, Y, Z, TT, E](terms: NonEmptyList[Term[SimpleTerm[T, lmbdindexer.LambdaInfo[U], TypeSimpleTerm[V, TypeLambdaInfo[W, X]]]]])(env: E)(implicit enval: TypeInferenceEnvironmental[E, Y, Z, TT]) =
    transformTermNel1(terms)(env)(transformTerm(_)(_))

  def transformBindNel[T, U, V, W, X, Y, Z, TT, E](binds: NonEmptyList[Bind[T, lmbdindexer.LambdaInfo[U], TypeSimpleTerm[V, TypeLambdaInfo[W, X]]]])(env: E)(implicit enval: TypeInferenceEnvironmental[E, Y, Z, TT]) =
    transformTermNel1(binds)(env)(transformBind(_)(_))
  
  def transformCaseNel[T, U, V, W, X, Y, Z, TT, E](cases: NonEmptyList[Case[T, lmbdindexer.LambdaInfo[U], TypeSimpleTerm[V, TypeLambdaInfo[W, X]]]])(env: E)(implicit enval: TypeInferenceEnvironmental[E, Y, Z, TT]) =
    transformTermNel1(cases)(env)(transformCase(_)(_))
   
  def transformBind[T, U, V, W, X, Y, Z, TT, E](bind: Bind[T, lmbdindexer.LambdaInfo[U], TypeSimpleTerm[V, TypeLambdaInfo[W, X]]])(env: E)(implicit enval: TypeInferenceEnvironmental[E, Y, Z, TT]) =
    transformTerm(bind.body)(env).map { Bind(bind.name, _, bind.pos) }
  
  def transformCase[T, U, V, W, X, Y, Z, TT, E](cas: Case[T, lmbdindexer.LambdaInfo[U], TypeSimpleTerm[V, TypeLambdaInfo[W, X]]])(env: E)(implicit enval: TypeInferenceEnvironmental[E, Y, Z, TT]): ValidationNel[AbstractError, Case[T, LambdaInfo[U, Z, TT], TypeSimpleTerm[V, TypeLambdaInfo[W, X]]]] =
    cas match {
      case Case(name, typ, body, lambdaInfo) =>
        for {
          body2 <- transformTerm(body)(env)
          lambdaInfo2 <- transformLambdaInfo(lambdaInfo)(env)
        } yield Case(name, typ, body2, lambdaInfo2)
    }
  
  def transformTerm[T, U, V, W, X, Y, Z, TT, E](term: Term[SimpleTerm[T, lmbdindexer.LambdaInfo[U], TypeSimpleTerm[V, TypeLambdaInfo[W, X]]]])(env: E)(implicit enval: TypeInferenceEnvironmental[E, Y, Z, TT]): ValidationNel[AbstractError, Term[SimpleTerm[T, LambdaInfo[U, Z, TT], TypeSimpleTerm[V, TypeLambdaInfo[W, X]]]]] = {
    term match {
      case App(fun, args, pos) =>
        (transformTerm(fun)(env) |@| transformTermNel(args)(env)) { App(_, _, pos) }
      case Simple(Let(binds, body, lambdaInfo), pos) =>
        for {
          binds2 <- transformBindNel(binds)(env)
          body2 <- transformTerm(body)(env)
          lambdaInfo2 <- transformLambdaInfo(lambdaInfo)(env)
        } yield Simple(Let(binds2, body2, lambdaInfo2), pos)
      case Simple(Lambda(args, body, lambdaInfo), pos) =>
        for {
          body2 <- transformTerm(body)(env)
          lambdaInfo2 <- transformLambdaInfo(lambdaInfo)(env)
        } yield Simple(Lambda(args, body2, lambdaInfo2), pos)
      case Simple(Var(loc, lambdaInfo), pos) =>
        transformLambdaInfo(lambdaInfo)(env).map { li => Simple(Var(loc, li), pos) }
      case Simple(Literal(value), pos) =>
        Simple(Literal(value), pos).success
      case Simple(TypedTerm(term, typ), pos) =>
        transformTerm(term)(env).map { t => Simple(TypedTerm(t, typ), pos) }
      case Simple(Construct(n, lambdaInfo), pos) =>
        transformLambdaInfo(lambdaInfo)(env).map { li => Simple(Construct(n, li), pos) }
      case Simple(Select(term, cases, lambdaInfo), pos) =>
        for {
          term2 <- transformTerm(term)(env)
          cases2 <- transformCaseNel(cases)(env)
          lambdaInfo2 <- transformLambdaInfo(lambdaInfo)(env)
        } yield Simple(Select(term2, cases2, lambdaInfo2), pos)
      case Simple(Extract(term, args, body, lambdaInfo), pos) =>
        for {
          term2 <- transformTerm(term)(env)
          body2 <- transformTerm(body)(env)
          lambdaInfo2 <- transformLambdaInfo(lambdaInfo)(env)
        } yield Simple(Extract(term2, args, body2, lambdaInfo2), pos)
    }
  }
  
  def transformTree[T, U, V, W, X, Y, Z, TT, TU, E](tree: Tree[T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], Z])(env: E)(implicit enval: TypeInferenceEnvironmental[E, T, TT, TU]) =
    tree.combs.foldLeft(Map[T, AbstractCombinator[U, LambdaInfo[V, TT, TU], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]]]().successNel[AbstractError]) {
      case (res, (loc, Combinator(typ, args, body, lambdaInfo, file))) =>
        val env2 = enval.withCurrentCombinatorLocation(env)(some(loc))
        val res2 = for {
          body2 <- transformTerm(body)(env2)
          lambdaInfo2 <- transformLambdaInfo(lambdaInfo)(env2)
        } yield Combinator(typ, args, body2, lambdaInfo2, file)
        (res |@| resultForFile(res2, file)) { (cs, c) => cs + (loc -> c) }
    }.flatMap {
      combs =>
        transformTypeTable(enval.globalTypeTableFromEnvironment(env)).map {
          tt => Tree(combs, TreeInfo(tree.treeInfo, InferredTypeTable(tt.types.filterKeys(combs.keySet.contains))))
        }
    }
  
  def transformTermWithTypeInference[T, U, V, W, X, Y, Z, TT, E](term: Term[SimpleTerm[T, lmbdindexer.LambdaInfo[U], TypeSimpleTerm[V, TypeLambdaInfo[W, X]]]])(env: E)(implicit inferrer: Inferrer[SimpleTerm[T, lmbdindexer.LambdaInfo[U], TypeSimpleTerm[V, TypeLambdaInfo[W, X]]], E, Type[Y]], envSt: TypeInferenceEnvironmentState[E, Z, Y], enval: TypeInferenceEnvironmental[E, Z, TT, Y]) = {
    val (newEnv, typ) = inferTermTypeS(term)(env)
    typ match {
      case noType: NoType[Y] => noType.errs.toNel.getOrElse(NonEmptyList(FatalError("no error", none, NoPosition))).failure
      case _                 => transformTerm(term)(newEnv).map { (_, typ) }
    }
  }
  
  def inferTermTypeS[T, U, V, W, X, Y, Z, E](term: Term[SimpleTerm[T, lmbdindexer.LambdaInfo[U], TypeSimpleTerm[V, TypeLambdaInfo[W, X]]]])(env: E)(implicit inferrer: Inferrer[SimpleTerm[T, lmbdindexer.LambdaInfo[U], TypeSimpleTerm[V, TypeLambdaInfo[W, X]]], E, Type[Y]], envSt: TypeInferenceEnvironmentState[E, Z, Y]): (E, Type[Y]) =
    envSt.withClearS {
      envSt.withCombinatorLocationS(none) {
        env2 =>
          val (env3, typ) = inferS(term)(env2)
          typ match {
            case noType: NoType[Y] =>
              (env3, noType)
            case _                 =>
              val (env4, res) = envSt.instantiateTypesFromLambdaInfosS(env3)
              res.map { _ => envSt.instantiateTypeS(typ)(env4) }.valueOr { (env4, _) }
          }
      } 
    } (env)
    
  def inferTermType[T, U, V, W, X, Y, Z, E](term: Term[SimpleTerm[T, lmbdindexer.LambdaInfo[U], TypeSimpleTerm[V, TypeLambdaInfo[W, X]]]])(implicit inferrer: Inferrer[SimpleTerm[T, lmbdindexer.LambdaInfo[U], TypeSimpleTerm[V, TypeLambdaInfo[W, X]]], E, Type[Y]], envSt: TypeInferenceEnvironmentState[E, Z, Y]) =
    State(inferTermTypeS[T, U, V, W, X, Y, Z, E](term))
    
  def inferTreeTypesS[E, L, C, I, F](tree: Tree[L, C, I])(env: F)(implicit init: Initializer[E, L, C, F]) =
    initializeS(tree)(env)
  
  def inferTreeTypes[E, L, C, I, F](tree: Tree[L, C, I])(implicit init: Initializer[E, L, C, F]) =
    State(inferTreeTypesS[E, L, C, I, F](tree))
  
  def transformS[T, U, V, W, X, Y, Z, TT, TU, TV, E, TC, TE](tree: Tree[T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], Z])(kindTable: InferredKindTable[TT], typeTable: InferredTypeTable[T, TT])(f: (InferredKindTable[TT], InferredTypeTable[T, TT]) => State[TE, E])(typeEnv: TE)(implicit init: Initializer[NoType[TT], T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], E], typeInit: Initializer[NoTypeValue[TT, W, TypeLambdaInfo[X, Y], TC], TT, AbstractTypeCombinator[W, TypeLambdaInfo[X, Y]], TE], envSt: TypeInferenceEnvironmentState[E, T, TT], enval: TypeInferenceEnvironmental[E, T, TU, TT], treeInfoExtractor: TreeInfoExtractor[Z, Tree[TT, AbstractTypeCombinator[W, TypeLambdaInfo[X, Y]], TypeTreeInfo[TV, TT]]]) =
    (for {
      res <- interpretTypeTreeFromTree(tree)(typeInit, treeInfoExtractor)
      res3 <- res.map {
        _ =>
          val typeTree = treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo)
          f(InferredKindTable(typeTree.treeInfo.kindTable.kinds ++ kindTable.kinds), typeTable).flatMap {
            env =>
              val (env2, res2) = inferTreeTypesS(tree)(env)
              State((_: TE, resultFromTypeResult(res2).flatMap { _ => transformTree(tree)(env2) }))
          }
      }.valueOr { ntv => State((_: TE, ntv.err.failureNel)) }
    } yield res3).run(typeEnv)
  
  def transform[T, U, V, W, X, Y, Z, TT, TU, TV, E, TC, TE](tree: Tree[T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], Z])(kindTable: InferredKindTable[TT], typeTable: InferredTypeTable[T, TT])(f: (InferredKindTable[TT], InferredTypeTable[T, TT]) => State[TE, E])(implicit init: Initializer[NoType[TT], T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], E], typeInit: Initializer[NoTypeValue[TT, W, TypeLambdaInfo[X, Y], TC], TT, AbstractTypeCombinator[W, TypeLambdaInfo[X, Y]], TE], envSt: TypeInferenceEnvironmentState[E, T, TT], enval: TypeInferenceEnvironmental[E, T, TU, TT], treeInfoExtractor: TreeInfoExtractor[Z, Tree[TT, AbstractTypeCombinator[W, TypeLambdaInfo[X, Y]], TypeTreeInfo[TV, TT]]]) =
    State(transformS[T, U, V, W, X, Y, Z, TT, TU, TV, E, TC, TE](tree)(kindTable, typeTable)(f))
  
  def inferTypesFromTreeStringS[T, U, V, W, X, Y, E](s: String)(nameTree: resolver.NameTree)(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, V, W], X]])(env: E)(implicit init: Initializer[NoType[Y], T, AbstractCombinator[U, V, W], E]) =
    (for {
      tree <- resolver.Resolver.transformString(s)(nameTree)
      tree2 <- f(tree)
    } yield {
      inferTreeTypesS(tree2)(env).mapElements(identity, _.successNel)
    }).valueOr { errs => (env, errs.failure) }
  
  def inferTypesFromTreeString[T, U, V, W, X, Y, E](s: String)(nameTree: resolver.NameTree)(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, V, W], X]])(implicit init: Initializer[NoType[Y], T, AbstractCombinator[U, V, W], E]) =
    State(inferTypesFromTreeStringS[T, U, V, W, X, Y, E](s)(nameTree)(f))
  
  def transformStringS[T, U, V, W, X, Y, Z, TT, TU, TV, E, TC, TE](s: String)(nameTree: resolver.NameTree, kindTable: InferredKindTable[TT], typeTable: InferredTypeTable[T, TT])(f: (Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]], InferredKindTable[TT]) => State[TE, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], Z]]])(g: (InferredKindTable[TT], InferredTypeTable[T, TT]) => State[TE, E])(typeEnv: TE)(implicit init: Initializer[NoType[TT], T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], E], typeInit: Initializer[NoTypeValue[TT, W, TypeLambdaInfo[X, Y], TC], TT, AbstractTypeCombinator[W, TypeLambdaInfo[X, Y]], TE], envSt: TypeInferenceEnvironmentState[E, T, TT], enval: TypeInferenceEnvironmental[E, T, TU, TT], treeInfoExtractor: TreeInfoExtractor[Z, Tree[TT, AbstractTypeCombinator[W, TypeLambdaInfo[X, Y]], TypeTreeInfo[TV, TT]]]) =
    resolver.Resolver.transformString(s)(nameTree).map {
      tree =>
        (for {
          res <- f(tree, kindTable)
          res2 <- res.map { 
            tree2 => transform(tree2)(kindTable, typeTable)(g)
          }.getOrElse { State((_: TE, FatalError("result is failure", none, NoPosition).failureNel)) }
        } yield res2).run(typeEnv)
    }.valueOr { errs => (typeEnv, errs.failure) }
  
  def transformString[T, U, V, W, X, Y, Z, TT, TU, TV, E, TC, TE](s: String)(nameTree: resolver.NameTree, kindTable: InferredKindTable[TT], typeTable: InferredTypeTable[T, TT])(f: (Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]], InferredKindTable[TT]) => State[TE, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], Z]]])(g: (InferredKindTable[TT], InferredTypeTable[T, TT]) => State[TE, E])(implicit init: Initializer[NoType[TT], T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], E], typeInit: Initializer[NoTypeValue[TT, W, TypeLambdaInfo[X, Y], TC], TT, AbstractTypeCombinator[W, TypeLambdaInfo[X, Y]], TE], envSt: TypeInferenceEnvironmentState[E, T, TT], enval: TypeInferenceEnvironmental[E, T, TU, TT], treeInfoExtractor: TreeInfoExtractor[Z, Tree[TT, AbstractTypeCombinator[W, TypeLambdaInfo[X, Y]], TypeTreeInfo[TV, TT]]]) =
    State(transformStringS[T, U, V, W, X, Y, Z, TT, TU, TV, E, TC, TE](s)(nameTree, kindTable, typeTable)(f)(g))
  
  def transformTermStringWithTypeInference[T, U, V, W, X, Y, Z, TT, E](s: String)(nameTree: resolver.NameTree, env: E)(f: Term[SimpleTerm[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]]] => ValidationNel[AbstractError, Term[SimpleTerm[T, lmbdindexer.LambdaInfo[U], TypeSimpleTerm[V, TypeLambdaInfo[W, X]]]]])(implicit inferrer: Inferrer[SimpleTerm[T, lmbdindexer.LambdaInfo[U], TypeSimpleTerm[V, TypeLambdaInfo[W, X]]], E, Type[Y]], envSt: TypeInferenceEnvironmentState[E, Z, Y], enval: TypeInferenceEnvironmental[E, Z, TT, Y]) =
    for {
      term <- resolver.Resolver.transformTermString(s)(resolver.Scope.fromNameTree(nameTree))
      term2 <- f(term)
      term3 <- transformTermWithTypeInference(term2)(env)
    } yield term3

  def transformToSymbolTree2(kindTable: kinder.InferredKindTable[GlobalSymbol]) = {
    (tree: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]]) =>
      for {
        tree2 <- lmbdindexer.LambdaIndexer.transform(tree)
        tree3 <- kinder.Kinder.transform(tree2)(kindTable) { (kt: kinder.InferredKindTable[GlobalSymbol]) => SymbolKindInferenceEnvironment.fromInferredKindTable[parser.TypeLambdaInfo](kt) }
      } yield tree3
  }
  
  val statefullyTransformToSymbolTree3 = {
    (tree: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]], kindTable: InferredKindTable[GlobalSymbol]) =>
      statefullyTransformToSymbolTree2(kindTable)(tree)
  }
  
  val statefullyMakeSymbolTypeInferenceEnvironment3 = {
    (kindTable: InferredKindTable[GlobalSymbol], typeTable: InferredTypeTable[GlobalSymbol, GlobalSymbol]) =>
      State({
        (typeEnv: SymbolTypeEnvironment[TypeLambdaInfo[parser.TypeLambdaInfo, LocalSymbol]]) =>
          (typeEnv, SymbolTypeInferenceEnvironment.fromInferredTypeTable[parser.LambdaInfo, parser.TypeLambdaInfo](typeTable).withTypeEnv(typeEnv).withKindInferenceEnv(kinder.SymbolKindInferenceEnvironment.fromInferredKindTable(kindTable)))
      })
  }
    
  def transformToSymbolTerm2(kindTable: kinder.InferredKindTable[GlobalSymbol]) = {
    (term: Term[SimpleTerm[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]]]) =>
      for {
        term2 <- lmbdindexer.LambdaIndexer.transformTerm(term)
        term3 <- kinder.Kinder.transformTerm(term2)(SymbolKindInferenceEnvironment.fromInferredKindTable[parser.TypeLambdaInfo](kindTable))
      } yield term3
  }
}