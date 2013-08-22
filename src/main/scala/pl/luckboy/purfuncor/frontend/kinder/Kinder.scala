package pl.luckboy.purfuncor.frontend.kinder
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.common.Tree
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

  private def inferKindAndTransformTypeTerm[T, U, V, E](typeTerm: Term[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo]])(env: E)(implicit inferrer: Inferrer[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo], E, Kind], envSt: KindInferenceEnvironmentState[E, U], enval: KindInferenceEnvironmental[E, U, V]) = {
    val (newEnv, kind) = inferTypeTermKindS(typeTerm)(enval.copyEnvironment(env))
    kind match {
      case noKind: NoKind => FatalError("no error", none, NoPosition).failureNel
      case _              => transformTypeTerm(typeTerm)(newEnv)
    }
  }
    
  def transformArg[T, U, V, E](arg: Arg[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo]])(env: E)(implicit inferrer: Inferrer[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo], E, Kind], envSt: KindInferenceEnvironmentState[E, U], enval: KindInferenceEnvironmental[E, U, V]) =
    arg.typ.map {
      inferKindAndTransformTypeTerm(_)(env).map { tt => Arg(arg.name, some(tt), arg.pos) }
    }.getOrElse(Arg(arg.name, none, arg.pos).successNel)
  
  def transformArgNel[T, U, V, E](args: NonEmptyList[Arg[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo]]])(env: E)(implicit inferrer: Inferrer[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo], E, Kind], envSt: KindInferenceEnvironmentState[E, U], enval: KindInferenceEnvironmental[E, U, V]) =
    args.tail.foldLeft(transformArg(args.head)(env).map { NonEmptyList(_) }) {
      case (Success(as), a)   => transformArg(a)(env).map { _ <:: as }
      case (Failure(errs), _) => errs.failure
    }.map { _.reverse }
  
  def transformArgs[T, U, V, E](args: List[Arg[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo]]])(env: E)(implicit inferrer: Inferrer[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo], E, Kind], envSt: KindInferenceEnvironmentState[E, U], enval: KindInferenceEnvironmental[E, U, V]) =
    args.toNel.map { transformArgNel(_)(env).map { _.list } }.getOrElse(Nil.successNel)
    
  private def transformTermNel1[T, U, E](terms: NonEmptyList[T])(env: E)(transform: (T, E) => ValidationNel[AbstractError, U]) =
    terms.tail.foldLeft(transform(terms.head, env).map { NonEmptyList(_) }) {
      case (Success(ts), t)   => transform(t, env).map { _ <:: ts }
      case (Failure(errs), _) => errs.failure
    }.map { _.reverse }

  def transformTermNel[T, U, V, W, E](terms: NonEmptyList[Term[SimpleTerm[T, lmbdindexer.LambdaInfo, TypeSimpleTerm[U, lmbdindexer.TypeLambdaInfo]]]])(env: E)(implicit inferrer: Inferrer[TypeSimpleTerm[U, lmbdindexer.TypeLambdaInfo], E, Kind], envSt: KindInferenceEnvironmentState[E, V], enval: KindInferenceEnvironmental[E, V, W]) =
    transformTermNel1(terms)(env)(transformTerm(_)(_))
  
  def transformTypeTermNel[T, U, V, E](terms: NonEmptyList[Term[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo]]])(env: E)(implicit enval: KindInferenceEnvironmental[E, U, V]) =
    transformTermNel1(terms)(env)(transformTypeTerm(_)(_))
    
  def transformTerm[T, U, V, W, E](term: Term[SimpleTerm[T, lmbdindexer.LambdaInfo, TypeSimpleTerm[U, lmbdindexer.TypeLambdaInfo]]])(env: E)(implicit inferrer: Inferrer[TypeSimpleTerm[U, lmbdindexer.TypeLambdaInfo], E, Kind], envSt: KindInferenceEnvironmentState[E, V], enval: KindInferenceEnvironmental[E, V, W]): ValidationNel[AbstractError, Term[SimpleTerm[T, lmbdindexer.LambdaInfo, TypeSimpleTerm[U, TypeLambdaInfo[W]]]]] =
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
      case Simple(Var(loc), pos) =>
        Simple(Var(loc), pos).successNel
      case Simple(Literal(value), pos) =>
        Simple(Literal(value), pos).successNel
      case Simple(TypedTerm(term, typ), pos) =>
        inferKindAndTransformTypeTerm(typ)(env).flatMap {
          typ2 => transformTerm(term)(env).map { term2 => Simple(TypedTerm(term2, typ2), pos) }
        }
    }
  
  def transformTypeTerm[T, U, V, E](term: Term[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo]])(env: E)(implicit enval: KindInferenceEnvironmental[E, U, V]): ValidationNel[AbstractError, Term[TypeSimpleTerm[T, TypeLambdaInfo[V]]]] =
    term match {
      case App(fun, args, pos) =>
        (transformTypeTerm(fun)(env) |@| transformTypeTermNel(args)(env)) { App(_, _, pos) }
      case Simple(TypeLambda(args, body, lmbdindexer.TypeLambdaInfo(lambdaIdx)), pos) =>
        transformTypeTerm(body)(env).flatMap {
          body2 =>
            enval.getLocalKindTableFromEnvironment(env)(lambdaIdx).map {
              kt => transformKindTable(kt).map { kt2 => Simple(TypeLambda(args, body2, TypeLambdaInfo(kt2)), pos) }
            }.getOrElse(FatalError("incorrect type lambda index", none, NoPosition).failureNel)
        }
      case Simple(TypeVar(loc), pos) =>
        Simple(TypeVar(loc), pos).successNel
      case Simple(TypeLiteral(value), pos) =>
        Simple(TypeLiteral(value), pos).successNel
      case Simple(KindedTypeTerm(term, kind), pos) =>
        transformTypeTerm(term)(env).map { t => Simple(KindedTypeTerm(t, kind), pos) }
    }
  
  def transformTree[T, U, V, W, X, Y, E](tree: Tree[T, AbstractCombinator[U, lmbdindexer.LambdaInfo, TypeSimpleTerm[V, lmbdindexer.TypeLambdaInfo]], W])(env: E)(implicit inferrer: Inferrer[TypeSimpleTerm[V, lmbdindexer.TypeLambdaInfo], E, Kind], envSt: KindInferenceEnvironmentState[E, X], enval: KindInferenceEnvironmental[E, X, Y]) =
    tree.combs.foldLeft(Map[T, AbstractCombinator[U, lmbdindexer.LambdaInfo, TypeSimpleTerm[V, TypeLambdaInfo[Y]]]]().successNel[AbstractError]) {
      case (Success(combs), (loc, comb)) =>
        comb match {
          case Combinator(typ, args, body, lambdaInfo, file) =>
            val typ2Res = typ.map { inferKindAndTransformTypeTerm(_)(env).map(some) }.getOrElse(none.successNel)
            val res = (typ2Res |@| transformArgs(args)(env) |@| transformTerm(body)(env)) {
              (typ2, args2, body2) => combs + (loc -> Combinator(typ2, args2, body2, lambdaInfo, file))
            }
            resultForFile(res, file)
        }
      case (Failure(errs), _)            =>
        errs.failure
    }.map { combs => Tree(combs = combs, treeInfo = tree.treeInfo) }
  
  def transformTypeTree[T, U, V, E](tree: Tree[T, AbstractTypeCombinator[U, lmbdindexer.TypeLambdaInfo], resolver.TypeTreeInfo])(env: E)(implicit enval: KindInferenceEnvironmental[E, T, V]) =
    tree.combs.foldLeft(Map[T, AbstractTypeCombinator[U, TypeLambdaInfo[V]]]().successNel[AbstractError]) {
      case (Success(combs), (loc, comb)) =>
        comb match {
          case TypeCombinator(kind, args, body, lmbdindexer.TypeLambdaInfo(lambdaIdx), file) =>
            val res = transformTypeTerm(body)(env).flatMap {
              body2 =>
                enval.getLocalKindTableFromEnvironment(env)(lambdaIdx).map {
                  kt => transformKindTable(kt).map { kt2 => combs + (loc -> TypeCombinator(kind, args, body2, TypeLambdaInfo(kt2), file)) }
                }.getOrElse(FatalError("incorrect type lambda index", none, NoPosition).failureNel)
            }
            resultForFile(res, file)
          case UnittypeCombinator(kind, n, file) =>
            (combs + (loc -> UnittypeCombinator(kind, n, file))).successNel
        }
      case (Failure(errs), _)            =>
        errs.failure
    }.flatMap {
      combs =>
        transformKindTable(enval.globalKindTableFromEnvironment(env)).map {
          kt => Tree(combs = combs, treeInfo = TypeTreeInfo(kt))
        }
    }
  
  def inferTypeTermKindS[T, U, E](term: Term[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo]])(env: E)(implicit inferrer: Inferrer[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo], E, Kind], envSt: KindInferenceEnvironmentState[E, U]) =
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
    
  def inferTypeTermKind[T, U, E](term: Term[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo]])(implicit inferrer: Inferrer[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo], E, Kind], envSt: KindInferenceEnvironmentState[E, U]) =
    State(inferTypeTermKindS[T, U, E](term))
  
  def inferTypeTreeKindsS[E, L, C, I, F](tree: Tree[L, C, I])(env: F)(implicit init: Initializer[E, L, C, F]) =
    initializeS(tree)(env)
    
  def inferTypeTreeKinds[E, L, C, I, F](tree: Tree[L, C, I])(implicit init: Initializer[E, L, C, F]) =
    State(inferTypeTreeKindsS[E, L, C, I, F](tree))
    
  def transform[T, U, V, W[_, _], X, Y, E](tree: Tree[T, AbstractCombinator[U, lmbdindexer.LambdaInfo, TypeSimpleTerm[V, lmbdindexer.TypeLambdaInfo]], W[lmbdindexer.TypeLambdaInfo, resolver.TypeTreeInfo]])(kindTable: InferredKindTable[T])(f: InferredKindTable[T] => E)(implicit init: Initializer[NoKind, X, AbstractTypeCombinator[V, lmbdindexer.TypeLambdaInfo], E], inferrer: Inferrer[TypeSimpleTerm[V, lmbdindexer.TypeLambdaInfo], E, Kind], envSt: KindInferenceEnvironmentState[E, X], enval: KindInferenceEnvironmental[E, X, Y], treeInfoTransformer: TreeInfoTransformer[W, X, Y], treeInfoExtractor: TreeInfoExtractor[W[lmbdindexer.TypeLambdaInfo, resolver.TypeTreeInfo], V, X]) = {
    val (env, res) = inferTypeTreeKindsS(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo))(f(kindTable))
    res.map {
      _ =>
        for {
          tree2 <- transformTree(tree)(env)
          treeInfo2 <- treeInfoTransformer.transformTreeInfo(tree.treeInfo)(env)
        } yield Tree(combs = tree2.combs, treeInfo = treeInfo2)
    }.valueOr { _.errs.toNel.getOrElse(NonEmptyList(FatalError("no error", none, NoPosition))).failure }
  }
}