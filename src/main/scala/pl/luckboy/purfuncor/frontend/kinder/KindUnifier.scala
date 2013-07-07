package pl.luckboy.purfuncor.frontend.kinder
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.common.Arrow

object KindUnifier
{
  def matchesKindTermsS[T, E](term1: KindTerm[StarKindTerm[Int]], term2: KindTerm[StarKindTerm[Int]])(z: T)(f: (Int, Either[Int, KindTerm[StarKindTerm[Int]]], T, E) => (E, Validation[NoKind, T]))(env: E)(implicit unifier: Unifier[NoKind, KindTerm[StarKindTerm[Int]], E, Int]): (E, Validation[NoKind, T]) =
    (term1, term2) match {
      case (Arrow(arg1, ret1, _), Arrow(arg2, ret2, _)) =>
        val (env2, argRes) = matchesKindTermsS(arg1, arg2)(z)(f)(env)
        argRes match {
          case Success(y)      => matchesKindTermsS(ret1, ret2)(y)(f)(env2)
          case Failure(noKind) => (env2, noKind.failure)
        }
      case (Star(KindType, _), Star(KindType, _)) =>
        (env, z.success)
      case (Star(KindParam(param1), _), Star(KindParam(param2), _)) =>
        f(param1, Left(param2), z, env)
      case (Star(KindParam(param1), _), _) =>
        f(param1, Right(term2), z, env)
      case (_, Star(KindParam(param2), _)) =>
        f(param2, Right(term1), z, env)
      case _ =>
        (env, unifier.mismatchedTermError(env).failure)
    }
  
  def paramsFromKindTerm[T](term: KindTerm[StarKindTerm[T]]): Set[T] =
    term match {
      case Arrow(arg, ret, _)        => paramsFromKindTerm(arg) | paramsFromKindTerm(ret)
      case Star(KindParam(param), _) => Set(param)
      case _                         => Set()
    }
  
  def paramKindTermFromKindTermS[T, E](term: KindTerm[StarKindTerm[T]])(params: Map[T, Int])(env: E)(implicit unifier: Unifier[NoKind, KindTerm[StarKindTerm[Int]], E, Int]): (E, Validation[NoKind, (Map[T, Int], KindTerm[StarKindTerm[Int]])]) =
    term match {
      case Arrow(arg, ret, pos) =>
        val (env2, argRes) = paramKindTermFromKindTermS(arg)(params)(env)
        argRes match {
          case Success((params2, arg2)) =>
            val (env3, retRes) = paramKindTermFromKindTermS(ret)(params2)(env)
            retRes.map { case (ps, ret2) => (env3, (ps, Arrow(arg2, ret2, pos)).success) }.valueOr { nk => (env3, nk.failure) } 
          case Failure(noKind)          =>
            (env2, noKind.failure)
        }
      case Star(KindType, pos) =>
        (env, (params, Star(KindType, pos)).success)
      case Star(KindParam(param), pos) =>
        params.get(param).map { param2 => (env, (params, Star(KindParam(param2), pos)).success) }.getOrElse {
          val (env2, res) = unifier.allocateParamS(env)
          res.map { param2 => (env2, (params + (param -> param2), Star(KindParam(param2), pos)).success) }.valueOr {
            nk => (env2, nk.failure) 
          }
        }
    }
}