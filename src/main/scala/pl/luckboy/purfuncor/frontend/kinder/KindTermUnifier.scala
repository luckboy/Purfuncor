package pl.luckboy.purfuncor.frontend.kinder
import scala.collection.immutable.BitSet
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.common.Arrow
import pl.luckboy.purfuncor.frontend.KindTermUtils._

object KindTermUnifier
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
        unifier.mismatchedTermErrorS(env).mapElements(identity, _.failure)
    }
  
  def replaceKindTermParamsS[E](term: KindTerm[StarKindTerm[Int]])(f: (Int, E) => (E, Validation[NoKind, Either[Int, KindTerm[StarKindTerm[Int]]]]))(env: E): (E, Validation[NoKind, KindTerm[StarKindTerm[Int]]]) = {
    term match {
      case Arrow(arg, ret, pos) =>
        val (env2, argRes) = replaceKindTermParamsS(arg)(f)(env)
        argRes match {
          case Success(arg2)   =>
            val (env3, retRes) = replaceKindTermParamsS(ret)(f)(env)
            (env3, retRes.map { ret2 => Arrow(arg2, ret2, NoPosition) })
          case Failure(noKind) =>
            (env2, noKind.failure)
        }
      case Star(KindParam(param), pos) =>
        val (env2, res) = f(param, env)
        res match {
          case Success(Left(param2))     => (env2, Star(KindParam(param2), pos).success)
          case Success(Right(paramTerm)) => (env2, paramTerm.success)
          case Failure(noKind)           => (env2, noKind.failure)
        }
      case Star(KindType, pos) =>
        (env, Star(KindType, pos).success)
    }
  }
  
  private def unsafeAllocateKindTermParamsS[T, E](term: KindTerm[StarKindTerm[T]])(allocatedParams: Map[T, Int])(env: E)(implicit unifier: Unifier[NoKind, KindTerm[StarKindTerm[Int]], E, Int]): (E, Validation[NoKind, (Map[T, Int], KindTerm[StarKindTerm[Int]])]) =
    term match {
      case Arrow(arg, ret, pos) =>
        val (env2, argRes) = unsafeAllocateKindTermParamsS(arg)(allocatedParams)(env)
        argRes match {
          case Success((params2, arg2)) =>
            val (env3, retRes) = unsafeAllocateKindTermParamsS(ret)(params2)(env2)
            retRes.map { case (ps, ret2) => (env3, (ps, Arrow(arg2, ret2, pos)).success) }.valueOr { nk => (env3, nk.failure) } 
          case Failure(noKind)          =>
            (env2, noKind.failure)
        }
      case Star(KindType, pos) =>
        (env, (allocatedParams, Star(KindType, pos)).success)
      case Star(KindParam(param), pos) =>
        allocatedParams.get(param).map { param2 => (env, (allocatedParams, Star(KindParam(param2), pos)).success) }.getOrElse {
          val (env2, res) = unifier.allocateParamS(env)
          res.map { param2 => (env2, (allocatedParams + (param -> param2), Star(KindParam(param2), pos)).success) }.valueOr {
            nk => (env2, nk.failure) 
          }
        }
    }

  def allocateKindTermParamsS[T, E](term: KindTerm[StarKindTerm[T]])(allocatedParams: Map[T, Int])(env: E)(implicit unifier: Unifier[NoKind, KindTerm[StarKindTerm[Int]], E, Int]) =
    unifier.withSaveS(unsafeAllocateKindTermParamsS(term)(allocatedParams))(env)
    
  def checkDefinedKindTermS[E](term: KindTerm[StarKindTerm[Int]])(env: E)(implicit unifier: Unifier[NoKind, KindTerm[StarKindTerm[Int]], E, Int]) = {
    val params = kindParamsFromKindTerm(term)
    val (env2, res) = params.foldLeft((env, BitSet().success[NoKind])) {
      case ((newEnv, Success(rps)), p) => unifier.findRootParamS(p)(newEnv).mapElements(identity, _.map { rps + _ })
      case ((newEnv, Failure(nk)), _)  => (newEnv, nk.failure)
    }
    (env2, res.map {
      rootParams => if(rootParams.size === params.size) ().success else NoKind.fromError(Error("parameters are distinct at defined kind " + intKindTermShowing.stringFrom(intKindTermFromKindTerm(term)), none, term.pos)).failure
    }.valueOr { _.failure })
  }
  
  def checkDefinedKindTermsS[E](terms: Seq[KindTerm[StarKindTerm[Int]]])(env: E)(implicit unifier: Unifier[NoKind, KindTerm[StarKindTerm[Int]], E, Int]) =
    terms.foldLeft((env, ().success[NoKind])) {
      case ((newEnv, newRes), t)  =>
        val (newEnv2, newRes2) = checkDefinedKindTermS(t)(newEnv)
        (newEnv2, (newRes |@| newRes2) { (u, _) => u })
    }
}