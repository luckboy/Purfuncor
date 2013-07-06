package pl.luckboy.purfuncor.frontend.kinder
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.common.Arrow

object KindUnifier
{
  def matchesTermsS[T, E](term1: KindTerm[StarKindTerm[Int]], term2: KindTerm[StarKindTerm[Int]])(z: T)(f: (Int, Either[Int, KindTerm[StarKindTerm[Int]]], T, E) => (E, Validation[NoKind, T]))(env: E)(implicit unifier: Unifier[NoKind, KindTerm[StarKindTerm[Int]], E, Int]): (E, Validation[NoKind, T]) =
    (term1, term2) match {
      case (Arrow(arg1, ret1, _), Arrow(arg2, ret2, _)) =>
        val (env2, res) = matchesTermsS(arg1, arg2)(z)(f)(env)
        res match {
          case Success(y)      => matchesTermsS(ret1, ret2)(y)(f)(env2)
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
}