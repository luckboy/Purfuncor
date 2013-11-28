package pl.luckboy.purfuncor.frontend.kinder
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.Bind
import pl.luckboy.purfuncor.common.Inferrer._
import pl.luckboy.purfuncor.common.Initializer._
import pl.luckboy.purfuncor.common.Result._

object Kinder
{
  def transformKindTable[T](kindTable: KindTable[T]) =
    kindTable.kinds.foldLeft(Map[T, InferredKind]().successNel[AbstractError]) {
      case (Success(ks), (l, k: InferredKind)) => (ks + (l -> k)).successNel
      case (Success(ks), _)                    => FatalError("can't instantiate kind", none, NoPosition).failureNel
      case (Failure(errs), _)                  => errs.failure
    }.map { InferredKindTable(_) }

  def transformTypeTermWithKindInference[T, U, V, W, E](typeTerm: Term[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo[U]]])(env: E)(implicit inferrer: Inferrer[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo[U]], E, Kind], envSt: KindInferenceEnvironmentState[E, V], enval: KindInferenceEnvironmental[E, V, W]) = {
    val (newEnv, kind) = inferTypeTermKindS(typeTerm)(enval.copyEnvironment(env))
    kind match {
      case noKind: NoKind => noKind.errs.toNel.getOrElse(NonEmptyList(FatalError("no error", none, NoPosition))).failure
      case _              => transformTypeTerm(typeTerm)(newEnv).map { (_, kind) }
    }
  }
    
  def transformArg[T, U, V, W, E](arg: Arg[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo[U]]])(env: E)(implicit inferrer: Inferrer[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo[U]], E, Kind], envSt: KindInferenceEnvironmentState[E, V], enval: KindInferenceEnvironmental[E, V, W]) =
    arg.typ.map {
      transformTypeTermWithKindInference(_)(env).map { case (tt, _) => Arg(arg.name, some(tt), arg.pos) }
    }.getOrElse(Arg(arg.name, none, arg.pos).successNel)
  
  def transformArgNel[T, U, V, W, E](args: NonEmptyList[Arg[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo[U]]]])(env: E)(implicit inferrer: Inferrer[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo[U]], E, Kind], envSt: KindInferenceEnvironmentState[E, V], enval: KindInferenceEnvironmental[E, V, W]) =
    args.tail.foldLeft(transformArg(args.head)(env).map { NonEmptyList(_) }) {
      case (Success(as), a)   => transformArg(a)(env).map { _ <:: as }
      case (Failure(errs), _) => errs.failure
    }.map { _.reverse }
  
  def transformArgs[T, U, V, W, E](args: List[Arg[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo[U]]]])(env: E)(implicit inferrer: Inferrer[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo[U]], E, Kind], envSt: KindInferenceEnvironmentState[E, V], enval: KindInferenceEnvironmental[E, V, W]) =
    args.toNel.map { transformArgNel(_)(env).map { _.list } }.getOrElse(Nil.successNel)
    
  private def transformTermNel1[T, U, E](terms: NonEmptyList[T])(env: E)(transform: (T, E) => ValidationNel[AbstractError, U]) =
    terms.tail.foldLeft(transform(terms.head, env).map { NonEmptyList(_) }) {
      case (Success(ts), t)   => transform(t, env).map { _ <:: ts }
      case (Failure(errs), _) => errs.failure
    }.map { _.reverse }

  def transformTermNel[T, U, V, W, X, Y, E](terms: NonEmptyList[Term[SimpleTerm[T, lmbdindexer.LambdaInfo[U], TypeSimpleTerm[V, lmbdindexer.TypeLambdaInfo[W]]]]])(env: E)(implicit inferrer: Inferrer[TypeSimpleTerm[V, lmbdindexer.TypeLambdaInfo[W]], E, Kind], envSt: KindInferenceEnvironmentState[E, X], enval: KindInferenceEnvironmental[E, X, Y]) =
    transformTermNel1(terms)(env)(transformTerm(_)(_))
  
  def transformTypeTermNel[T, U, V, W, E](terms: NonEmptyList[Term[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo[U]]]])(env: E)(implicit enval: KindInferenceEnvironmental[E, V, W]) =
    transformTermNel1(terms)(env)(transformTypeTerm(_)(_))
  
  def transformCase[T, U, V, W, X, Y, E](cas: Case[T, lmbdindexer.LambdaInfo[U], TypeSimpleTerm[V, lmbdindexer.TypeLambdaInfo[W]]])(env: E)(implicit inferrer: Inferrer[TypeSimpleTerm[V, lmbdindexer.TypeLambdaInfo[W]], E, Kind], envSt: KindInferenceEnvironmentState[E, X], enval: KindInferenceEnvironmental[E, X, Y]) =
    cas match {
      case Case(name, typ, body, lambdaInfo) =>
        transformTypeTermWithKindInference(typ)(env).flatMap { 
          case (tt, _) => transformTerm(body)(env).map { Case(name, tt, _, lambdaInfo) }
        }
    }
    
  def transformCaseNel[T, U, V, W, X, Y, E](cases: NonEmptyList[Case[T, lmbdindexer.LambdaInfo[U], TypeSimpleTerm[V, lmbdindexer.TypeLambdaInfo[W]]]])(env: E)(implicit inferrer: Inferrer[TypeSimpleTerm[V, lmbdindexer.TypeLambdaInfo[W]], E, Kind], envSt: KindInferenceEnvironmentState[E, X], enval: KindInferenceEnvironmental[E, X, Y]): ValidationNel[AbstractError, NonEmptyList[Case[T, lmbdindexer.LambdaInfo[U], TypeSimpleTerm[V, TypeLambdaInfo[W, Y]]]]] =
    transformTermNel1(cases)(env)(transformCase(_)(_))
  
  def transformTerm[T, U, V, W, X, Y, E](term: Term[SimpleTerm[T, lmbdindexer.LambdaInfo[U], TypeSimpleTerm[V, lmbdindexer.TypeLambdaInfo[W]]]])(env: E)(implicit inferrer: Inferrer[TypeSimpleTerm[V, lmbdindexer.TypeLambdaInfo[W]], E, Kind], envSt: KindInferenceEnvironmentState[E, X], enval: KindInferenceEnvironmental[E, X, Y]): ValidationNel[AbstractError, Term[SimpleTerm[T, lmbdindexer.LambdaInfo[U], TypeSimpleTerm[V, TypeLambdaInfo[W, Y]]]]] =
    term match {
      case App(fun, args, pos) =>
        (transformTerm(fun)(env) |@| transformTermNel(args)(env)) { App(_, _, pos) }
      case Simple(Let(binds, body, lambdaInfo), pos) =>
        transformTermNel(binds.map { _.body })(env).flatMap {
          bindTerms =>
            val binds2 = binds.zip(bindTerms).map { case (Bind(name, _, bindPos), bindTerm) => Bind(name, bindTerm, bindPos) }
            transformTerm(body)(env).map { body2 => Simple(Let(binds2, body2, lambdaInfo), pos) }
        }
      case Simple(Lambda(args, body, lambdaInfo), pos) =>
        transformArgNel(args)(env).flatMap {
          args2 => transformTerm(body)(env).map { body2 => Simple(Lambda(args2, body2, lambdaInfo), pos) }
        }
      case Simple(Var(loc, lambdaInfo), pos) =>
        Simple(Var(loc, lambdaInfo), pos).successNel
      case Simple(Literal(value), pos) =>
        Simple(Literal(value), pos).successNel
      case Simple(TypedTerm(term, typ), pos) =>
        transformTypeTermWithKindInference(typ)(env).flatMap {
          case (typ2, _) => transformTerm(term)(env).map { term2 => Simple(TypedTerm(term2, typ2), pos) }
        }
      case Simple(Construct(n, lambdaInfo), pos) =>
        Simple(Construct(n, lambdaInfo), pos).successNel
      case Simple(Select(term, cases, lambdaInfo), pos) =>
        transformTerm(term)(env).flatMap {
          term2 => transformCaseNel(cases)(env).map { cases2 => Simple(Select(term2, cases2, lambdaInfo), pos)}
        }
      case Simple(Extract(term, args, body, lambdaInfo), pos) =>
        for {
          term2 <- transformTerm(term)(env)
          args2 <- transformArgNel(args)(env)
          body2 <- transformTerm(body)(env)
        } yield Simple(Extract(term2, args2, body2, lambdaInfo), pos)
    }
  
  def transformTypeTerm[T, U, V, W, E](term: Term[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo[U]]])(env: E)(implicit enval: KindInferenceEnvironmental[E, V, W]): ValidationNel[AbstractError, Term[TypeSimpleTerm[T, TypeLambdaInfo[U, W]]]] =
    term match {
      case App(fun, args, pos) =>
        (transformTypeTerm(fun)(env) |@| transformTypeTermNel(args)(env)) { App(_, _, pos) }
      case Simple(TypeLambda(args, body, lmbdindexer.TypeLambdaInfo(lambdaInfo, lambdaIdx)), pos) =>
        transformTypeTerm(body)(env).flatMap {
          body2 =>
            enval.getLocalKindTableFromEnvironment(env)(lambdaIdx).map {
              kt => transformKindTable(kt).map { kt2 => Simple(TypeLambda(args, body2, TypeLambdaInfo(lambdaInfo, kt2)), pos) }
            }.getOrElse(FatalError("incorrect type lambda index", none, NoPosition).failureNel)
        }
      case Simple(TypeVar(loc), pos) =>
        Simple(TypeVar(loc), pos).successNel
      case Simple(TypeLiteral(value), pos) =>
        Simple(TypeLiteral(value), pos).successNel
      case Simple(KindedTypeTerm(term, kind), pos) =>
        transformTypeTerm(term)(env).map { t => Simple(KindedTypeTerm(t, kind), pos) }
    }
  
  def transformTree[T, U, V, W, X, Y, Z, TT, E](tree: Tree[T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, lmbdindexer.TypeLambdaInfo[X]]], Y])(env: E)(implicit inferrer: Inferrer[TypeSimpleTerm[W, lmbdindexer.TypeLambdaInfo[X]], E, Kind], envSt: KindInferenceEnvironmentState[E, Z], enval: KindInferenceEnvironmental[E, Z, TT]) =
    tree.combs.foldLeft(Map[T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, TT]]]]().successNel[AbstractError]) {
      case (res, (loc, comb)) =>
        comb match {
          case Combinator(typ, args, body, lambdaInfo, file) =>
            val typ2Res = typ.map { transformTypeTermWithKindInference(_)(env).map { case (tt, _) => some(tt) } }.getOrElse(none.successNel)
            val res2 = (typ2Res |@| transformArgs(args)(env) |@| transformTerm(body)(env)) {
              (typ2, args2, body2) => Combinator(typ2, args2, body2, lambdaInfo, file)
            }
            (res |@| resultForFile(res2, file)) { (cs, c) => cs + (loc -> c) }
          case PolyCombinator(typ, file)                     =>
            val res2 = typ.map { 
              transformTypeTermWithKindInference(_)(env).map { case (tt, _) => PolyCombinator(some(tt), file) }
            }.getOrElse(PolyCombinator(none, file).successNel)
            (res |@| resultForFile(res2, file)) { (cs, c) => cs + (loc -> c) }
        }
    }.map { combs => Tree(combs = combs, treeInfo = tree.treeInfo) }
  
  def transformTypeTree[T, U, V, W, X, E](tree: Tree[T, AbstractTypeCombinator[U, lmbdindexer.TypeLambdaInfo[V]], W])(env: E)(implicit enval: KindInferenceEnvironmental[E, T, X]) =
    tree.combs.foldLeft(Map[T, AbstractTypeCombinator[U, TypeLambdaInfo[V, X]]]().successNel[AbstractError]) {
      case (res, (loc, comb)) =>
        comb match {
          case TypeCombinator(kind, args, body, lmbdindexer.TypeLambdaInfo(lambdaInfo, lambdaIdx), file) =>
            val env2 = enval.withCurrentTypeCombinatorLocation(env)(some(loc))
            val res2 = transformTypeTerm(body)(env2).flatMap {
              body2 =>
                enval.getLocalKindTableFromEnvironment(env2)(lambdaIdx).map {
                  kt => transformKindTable(kt).map { kt2 => TypeCombinator(kind, args, body2, TypeLambdaInfo(lambdaInfo, kt2), file) }
                }.getOrElse(FatalError("incorrect type lambda index", none, NoPosition).failureNel)
            }
            (res |@| resultForFile(res2, file)) { (cs, c) => cs + (loc -> c) }
          case UnittypeCombinator(kind, n, file) =>
            res.map { cs => (cs + (loc -> UnittypeCombinator(kind, n, file))) }
        }
    }.flatMap {
      combs =>
        transformKindTable(enval.globalKindTableFromEnvironment(env)).map {
          kt => Tree(combs = combs, treeInfo = TypeTreeInfo(tree.treeInfo, InferredKindTable(kt.kinds.filterKeys(combs.keySet.contains))))
        }
    }
  
  def transformSelectConstructInstance[T, U, V, W, E](instance: SelectConstructInstance[T, lmbdindexer.TypeLambdaInfo[U]])(env: E)(implicit enval: KindInferenceEnvironmental[E, V, W]) = {
    val res = for {
      supertype <- transformTypeTerm(instance.supertype)(env)
      types <- transformTypeTermNel(instance.types)(env)
    } yield SelectConstructInstance(supertype, types, instance.file)
    resultForFile(res, instance.file)
  }
    
  def transformSelectConstructInstances[T, U, V, W, E](instances: List[SelectConstructInstance[T, lmbdindexer.TypeLambdaInfo[U]]])(env: E)(implicit enval: KindInferenceEnvironmental[E, V, W]) =
    instances.foldLeft(List[SelectConstructInstance[T, TypeLambdaInfo[U, W]]]().successNel[AbstractError]) {
      case (res, instance) =>
        val res2 = transformSelectConstructInstance(instance)(env)
        (res |@| res2) { (scis, sci) => sci :: scis }
    }.map { _.reverse }
  
  def inferTypeTermKindS[T, U, V, E](term: Term[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo[U]]])(env: E)(implicit inferrer: Inferrer[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo[U]], E, Kind], envSt: KindInferenceEnvironmentState[E, V]) =
    envSt.withClearS {
      envSt.withTypeCombinatorLocationS(none) {
        env2 =>
          val (env3, kind) = inferS(term)(env2)
          kind match {
            case noKind: NoKind =>
              (env3, noKind)
            case _              =>
              val (env4, res) = envSt.instantiateLocalKindTablesS(env3)
              res.map { _ => envSt.instantiateKindS(kind)(env4) }.valueOr { (env4, _) }
          }
      }
    } (env)
    
  def inferTypeTermKind[T, U, V, E](term: Term[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo[U]]])(implicit inferrer: Inferrer[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo[U]], E, Kind], envSt: KindInferenceEnvironmentState[E, V]) =
    State(inferTypeTermKindS[T, U, V, E](term))
  
  def inferTypeTreeKindsS[E, L, C, I, F](tree: Tree[L, C, I])(env: F)(implicit init: Initializer[E, L, C, F]) =
    initializeS(tree)(env)
    
  def inferTypeTreeKinds[E, L, C, I, F](tree: Tree[L, C, I])(implicit init: Initializer[E, L, C, F]) =
    State(inferTypeTreeKindsS[E, L, C, I, F](tree))
    
  def inferTreeKindsS[T, U, V, E, L, C, I, F](tree: Tree[T, U, V])(env: F)(implicit init: Initializer[E, L, C, F], treeInfoExtractor: TreeInfoExtractor[V, Tree[L, C, I]]) =
    inferTypeTreeKindsS(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo))(env)
    
  def inferTreeKinds[T, U, V, E, L, C, I, F](tree: Tree[T, U, V])(env: F)(implicit init: Initializer[E, L, C, F], treeInfoExtractor: TreeInfoExtractor[V, Tree[L, C, I]]) =
    State(inferTreeKindsS[T, U, V, E, L, C, I, F](tree))
  
  def transform[T, U, V, W, X, Y[_, _], Z, TT, TU, E](tree: Tree[T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, lmbdindexer.TypeLambdaInfo[X]]], Y[lmbdindexer.TypeLambdaInfo[X], Z]])(kindTable: InferredKindTable[TT])(f: InferredKindTable[TT] => E)(implicit init: Initializer[NoKind, TT, AbstractTypeCombinator[W, lmbdindexer.TypeLambdaInfo[X]], E], inferrer: Inferrer[TypeSimpleTerm[W, lmbdindexer.TypeLambdaInfo[X]], E, Kind], envSt: KindInferenceEnvironmentState[E, TT], enval: KindInferenceEnvironmental[E, TT, TU], treeInfoTransformer: TreeInfoTransformer[Y, TT, TU], treeInfoExtractor: TreeInfoExtractor[Y[lmbdindexer.TypeLambdaInfo[X], Z], Tree[TT, AbstractTypeCombinator[W, lmbdindexer.TypeLambdaInfo[X]], Z]]) = {
    val (env, res) = inferTreeKindsS(tree)(f(kindTable))
    res.map {
      _ =>
        for {
          tree2 <- transformTree(tree)(env)
          treeInfo2 <- treeInfoTransformer.transformTreeInfo(tree.treeInfo)(env)
        } yield Tree(combs = tree2.combs, treeInfo = treeInfo2)
    }.valueOr { _.errs.toNel.getOrElse(NonEmptyList(FatalError("no error", none, NoPosition))).failure }
  }

  def inferKindsFromTreeStringS[T, U, V, W, X, Y, Z, TT, E](s: String)(nameTree: resolver.NameTree)(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, lmbdindexer.TypeLambdaInfo[X]]], Y]])(env: E)(implicit init: Initializer[NoKind, Z, AbstractTypeCombinator[W, lmbdindexer.TypeLambdaInfo[X]], E], treeInfoExtractor: TreeInfoExtractor[Y, Tree[Z, AbstractTypeCombinator[W, lmbdindexer.TypeLambdaInfo[X]], TT]]) =
    (for {
      tree <- resolver.Resolver.transformString(s)(nameTree)
      tree2 <- f(tree)
    } yield {
      inferTreeKindsS(tree2)(env).mapElements(identity, _.successNel)
    }).valueOr { errs => (env, errs.failure) }
  
  def inferKindsFromTreeString[T, U, V, W, X, Y, Z, TT, E](s: String)(nameTree: resolver.NameTree)(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, lmbdindexer.TypeLambdaInfo[X]]], Y]])(implicit init: Initializer[NoKind, Z, AbstractTypeCombinator[W, lmbdindexer.TypeLambdaInfo[X]], E], treeInfoExtractor: TreeInfoExtractor[Y, Tree[Z, AbstractTypeCombinator[W, lmbdindexer.TypeLambdaInfo[X]], TT]]) =
    State(inferKindsFromTreeStringS[T, U, V, W, X, Y, Z, TT, E](s)(nameTree)(f))
  
  def transformString[T, U, V, W, X, Y[_, _], Z, TT, TU, E](s: String)(nameTree: resolver.NameTree, kindTable: InferredKindTable[TT])(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, lmbdindexer.TypeLambdaInfo[X]]], Y[lmbdindexer.TypeLambdaInfo[X], Z]]])(g: InferredKindTable[TT] => E)(implicit init: Initializer[NoKind, TT, AbstractTypeCombinator[W, lmbdindexer.TypeLambdaInfo[X]], E], inferrer: Inferrer[TypeSimpleTerm[W, lmbdindexer.TypeLambdaInfo[X]], E, Kind], envSt: KindInferenceEnvironmentState[E, TT], enval: KindInferenceEnvironmental[E, TT, TU], treeInfoTransformer: TreeInfoTransformer[Y, TT, TU], treeInfoExtractor: TreeInfoExtractor[Y[lmbdindexer.TypeLambdaInfo[X], Z], Tree[TT, AbstractTypeCombinator[W, lmbdindexer.TypeLambdaInfo[X]], Z]]) =
    for {
      tree <- resolver.Resolver.transformString(s)(nameTree)
      tree2 <- f(tree)
      tree3 <- transform(tree2)(kindTable)(g)
    } yield tree3
  
  def transformTypeTermStringWithKindInference[T, U, V, W, E](s: String)(nameTree: resolver.NameTree, env: E)(f: Term[TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]] => ValidationNel[AbstractError, Term[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo[U]]]])(implicit inferrer: Inferrer[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo[U]], E, Kind], envSt: KindInferenceEnvironmentState[E, V], enval: KindInferenceEnvironmental[E, V, W]) =
    for {
      term <- resolver.Resolver.transformTypeTermString(s)(resolver.Scope.fromNameTree(nameTree))
      term2 <- f(term)
      term3 <- transformTypeTermWithKindInference(term2)(env)
    } yield term3
    
  val transformToSymbolTree = {
    (tree: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]]) =>
      lmbdindexer.LambdaIndexer.transform(tree)
  }
  
  val transformToSymbolTypeTerm = {
    (term: Term[TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]]) => lmbdindexer.LambdaIndexer.transformTypeTerm(term)
  }
}