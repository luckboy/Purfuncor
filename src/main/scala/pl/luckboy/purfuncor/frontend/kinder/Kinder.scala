package pl.luckboy.purfuncor.frontend.kinder
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.common.Inferrer._
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.frontend.Bind

object Kinder
{
  def transformKindTable[T](kindTable: KindTable[T]) =
    kindTable.kinds.foldLeft(Map[T, InferredKind]().successNel[AbstractError]) {
      case (Success(ks), (l, k: InferredKind)) => (ks + (l -> k)).successNel
      case (Success(ks), _)                    => FatalError("can't instantiate kind", none, NoPosition).failureNel
      case (Failure(errs), _)                  => errs.failure
    }.map { InferredKindTable(_) }

  def transformArg[T, U, V, E](arg: Arg[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo]])(env: E)(implicit inferrer: Inferrer[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo], E, Kind], envSt: KindInferenceEnvironmentState[E, U], enval: KindInferenceEnvironmental[E, U, V]) =
    arg.typ.map {
      tt =>
        val (newEnv, kind) = inferTypeTermKindS(tt)(enval.copyEnvironment(env))
        kind match {
          case noKind: NoKind =>
            FatalError("no errors", none, NoPosition).failureNel
          case _              =>
            transformTypeTerm(tt)(newEnv).map { tt2 => Arg(arg.name, some(tt2), arg.pos) }
        }
    }.getOrElse(Arg(arg.name, none, arg.pos).successNel)
    
  def transformArgNel[T, U, V, E](args: NonEmptyList[Arg[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo]]])(env: E)(implicit inferrer: Inferrer[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo], E, Kind], envSt: KindInferenceEnvironmentState[E, U], enval: KindInferenceEnvironmental[E, U, V]) =
    args.tail.foldLeft(transformArg(args.head)(env).map { NonEmptyList(_) }) {
      case (Success(as), a)   => transformArg(a)(env).map { _ <:: as }
      case (Failure(errs), _) => errs.failure
    }.map { _.reverse }
  
  private def transformTermNel1[T, U, E](terms: NonEmptyList[T])(env: E)(transform: (T, E) => ValidationNel[AbstractError, U]) =
    terms.tail.foldLeft(transform(terms.head, env).map { NonEmptyList(_) }) {
      case (Success(ts), t)   => transform(t, env).map { _ <:: ts }
      case (Failure(errs), _) => errs.failure
    }.map { _.reverse }

  def transformTermNel[T, U, V, E](terms: NonEmptyList[Term[SimpleTerm[T, lmbdindexer.LambdaInfo, TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo]]]])(env: E)(implicit inferrer: Inferrer[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo], E, Kind], envSt: KindInferenceEnvironmentState[E, U], enval: KindInferenceEnvironmental[E, U, V]) =
    transformTermNel1(terms)(env)(transformTerm(_)(_))
  
  def transformTypeTermNel[T, U, V, E](terms: NonEmptyList[Term[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo]]])(env: E)(implicit enval: KindInferenceEnvironmental[E, U, V]) =
    transformTermNel1(terms)(env)(transformTypeTerm(_)(_))
    
  def transformTerm[T, U, V, E](term: Term[SimpleTerm[T, lmbdindexer.LambdaInfo, TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo]]])(env: E)(implicit inferrer: Inferrer[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo], E, Kind], envSt: KindInferenceEnvironmentState[E, U], enval: KindInferenceEnvironmental[E, U, V]): ValidationNel[AbstractError, Term[SimpleTerm[T, lmbdindexer.LambdaInfo, TypeSimpleTerm[T, TypeLambdaInfo[V]]]]] =
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
        val (newEnv, kind) = inferTypeTermKindS(typ)(enval.copyEnvironment(env))
        kind match {
          case noKind: NoKind =>
            FatalError("no errors", none, NoPosition).failureNel
          case _              =>
            (transformTerm(term)(env) |@| transformTypeTerm(typ)(newEnv)) {
              (term2, typ2) => Simple(TypedTerm(term2, typ2), pos)
            }
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
  
  def inferTypeTermKindS[T, U, E](term: Term[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo]])(env: E)(implicit inferrer: Inferrer[TypeSimpleTerm[T, lmbdindexer.TypeLambdaInfo], E, Kind], envSt: KindInferenceEnvironmentState[E, U]) =
    envSt.withClearS {
      envSt.withCombinatorLocationS(none) {
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
}