package pl.luckboy.purfuncor.frontend.instant
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.kinder.TypeLambdaInfo
import pl.luckboy.purfuncor.frontend.typer.InferredType
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.frontend.Bind
import pl.luckboy.purfuncor.common.Result._

object Instantiator 
{
  def transformLambdaInfo[T, U, V, W, E](lambdaInfo: typer.LambdaInfo[T, U, V])(env: E)(implicit enval: InstantiationEnvironmental[E, W, V]) =
    lambdaInfo match {
      case typer.LambdaInfo(lambdaInfo2, lambdaIdx, typeTable, _, _) =>
        enval.getLambdaInfoFromEnvironment(env)(lambdaIdx).map {
          instantiationLambdaInfo =>
            LambdaInfo(lambdaInfo2, lambdaIdx, typeTable, instantiationLambdaInfo.insts).successNel
        }.getOrElse(FatalError("incorrect lambda index", none, NoPosition).failureNel)
    }
  
  private def transformTermNel1[T, U, E](terms: NonEmptyList[T])(env: E)(transform: (T, E) => ValidationNel[AbstractError, U]) =
    terms.tail.foldLeft(transform(terms.head, env).map { NonEmptyList(_) }) {
      case (Success(ts), t)   => transform(t, env).map { _ <:: ts }
      case (Failure(errs), _) => errs.failure
    }
  
  def transformTermNel[T, U, V, W, X, Y, Z, TT, E](terms: NonEmptyList[Term[SimpleTerm[T, typer.LambdaInfo[U, V, W], TypeSimpleTerm[X, TypeLambdaInfo[Y, Z]]]]])(env: E)(implicit enval: InstantiationEnvironmental[E, TT, W]) =
    transformTermNel1(terms)(env)(transformTerm(_)(_))
  
  def transformBindNel[T, U, V, W, X, Y, Z, TT, E](binds: NonEmptyList[Bind[T, typer.LambdaInfo[U, V, W], TypeSimpleTerm[X, TypeLambdaInfo[Y, Z]]]])(env: E)(implicit enval: InstantiationEnvironmental[E, TT, W]) =
    transformTermNel1(binds)(env)(transformBind(_)(_))

  def transformCaseNel[T, U, V, W, X, Y, Z, TT, E](cases: NonEmptyList[Case[T, typer.LambdaInfo[U, V, W], TypeSimpleTerm[X, TypeLambdaInfo[Y, Z]]]])(env: E)(implicit enval: InstantiationEnvironmental[E, TT, W]) =
    transformTermNel1(cases)(env)(transformCase(_)(_))
  
  def transformBind[T, U, V, W, X, Y, Z, TT, E](bind: Bind[T, typer.LambdaInfo[U, V, W], TypeSimpleTerm[X, TypeLambdaInfo[Y, Z]]])(env: E)(implicit enval: InstantiationEnvironmental[E, TT, W]) =
    transformTerm(bind.body)(env).map { Bind(bind.name, _, bind.pos) }
  
  def transformCase[T, U, V, W, X, Y, Z, TT, E](cas: Case[T, typer.LambdaInfo[U, V, W], TypeSimpleTerm[X, TypeLambdaInfo[Y, Z]]])(env: E)(implicit enval: InstantiationEnvironmental[E, TT, W]) =
    cas match {
      case Case(name, typ, body, lambdaInfo) =>
        for {
          body2 <- transformTerm(body)(env)
          lambdaInfo2 <- transformLambdaInfo(lambdaInfo)(env)
        } yield Case(name, typ, body2, lambdaInfo2)
    }
  
  def transformTerm[T, U, V, W, X, Y, Z, TT, E](term: Term[SimpleTerm[T, typer.LambdaInfo[U, V, W], TypeSimpleTerm[X, TypeLambdaInfo[Y, Z]]]])(env: E)(implicit enval: InstantiationEnvironmental[E, TT, W]): ValidationNel[AbstractError, Term[SimpleTerm[T, LambdaInfo[U, V, W, TT], TypeSimpleTerm[X, TypeLambdaInfo[Y, Z]]]]] =
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
        Simple(Literal(value), pos).successNel
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
  
  def transformTree[T, U, V, W, X, Y, Z, TT, TU, E](tree: Tree[T, AbstractCombinator[U, typer.LambdaInfo[V, W, X], TypeSimpleTerm[Y, TypeLambdaInfo[Z, TT]]], typer.TreeInfo[TU, T, X]])(instTree: InstanceTree[AbstractPolyFunction[T], X, GlobalInstance[T]], instArgTable: InstanceArgTable[T, X])(env: E)(implicit enval: InstantiationEnvironmental[E, T, X]) =
    tree.combs.foldLeft(Map[T, AbstractCombinator[U, LambdaInfo[V, W, X, T], TypeSimpleTerm[Y, TypeLambdaInfo[Z, TT]]]]().successNel[AbstractError]) {
      case (res, (loc, Combinator(typ, args, body, lambdaInfo, file))) =>
        val env2 = enval.withCurrentCombinatorLocation(env)(some(loc))
        val res2 = for {
          body2 <- transformTerm(body)(env2)
          lambdaInfo2 <- transformLambdaInfo(lambdaInfo)(env2)
        } yield Combinator(typ, args, body2, lambdaInfo2, file)
        (res |@| resultForFile(res2, file)) { (cs, c) => cs + (loc -> c) }
      case (res, (loc, PolyCombinator(typ, file))) =>
        res.map { _ + (loc -> PolyCombinator(typ, file)) }
    }.map {
      combs =>
        Tree(combs, TreeInfo(tree.treeInfo.treeInfo, tree.treeInfo.typeTable, enval.treeGlobalInstanceTreeFromEnvironment(env), InstanceArgTable(enval.instanceArgTableFromFromEnvironment(env).instArgs.filterKeys(combs.keySet.contains))))
    }
}