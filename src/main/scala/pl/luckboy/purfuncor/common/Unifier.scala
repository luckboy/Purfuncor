package pl.luckboy.purfuncor.common
import scala.annotation.tailrec
import scalaz._
import scalaz.Scalaz._

trait Unifier[E, T, F, P]
{
  def matchesTermsS[U](term1: T, term2: T)(f: (P, Either[P, T], U, F) => (F, Validation[E, U]))(env: F): (F, Validation[E, U])

  def getParamTermS(param: P)(env: F): (F, Option[T])
  
  def findRootParamS(param: P)(env: F): (F, Validation[E, P])
  
  def replaceParamS(param: P, term: T)(env: F): (F, Validation[E, Unit])
  
  def unionParamsS(param1: P, param2: P)(env: F): (F, Validation[E, Unit])
  
  def mismatchedTermErrorS(env: F): (F, E)
}

object Unifier
{
  def unifyS[E, T, F, P](term1: T, term2: T)(env: F)(implicit unifier: Unifier[E, T, F, P]) = {
    val (env2, res) = fullyMatchesAndReplaceS(term1, term2)(env)
    res.map { term => (env2, term.success) }.valueOr { err => (env, err.failure) }
  }
    
  def unify[E, T, F, P](term1: T, term2: T)(implicit unifier: Unifier[E, T, F, P]) =
    State(unifyS[E, T, F, P](term1, term2))
  
  @tailrec
  private def fullyMatchesAndReplaceS[E, T, F, P](term1: T, term2: T)(env: F)(implicit unifier: Unifier[E, T, F, P]): (F, Validation[E, T]) = {
    val (env2, res) = matchesAndReplaceS(term1, term2)(Set(), false)(env)
    res match {
      case Success(true)  => fullyMatchesAndReplaceS(term1, term2)(env2)
      case Success(false) => (env2, term1.success)
      case Failure(err)    => (env2, err.failure)
    }
  }
    
  private def matchesAndReplaceS[E, T, F, P](term1: T, term2: T)(markedParams: Set[P], areChangedParams: Boolean)(env: F)(implicit unifier: Unifier[E, T, F, P]): (F, Validation[E, Boolean]) =
    unifier.matchesTermsS[Boolean](term1, term2) {
      case (param1, Left(param2), areChangedNewParams, newEnv) =>
        val (newEnv2, res1) = unifier.findRootParamS(param1)(newEnv)
        val (newEnv3, res2) = unifier.findRootParamS(param2)(newEnv2)
        (res1, res2) match {
          case (Success(rootParam1), Success(rootParam2)) =>
            if(!markedParams.contains(rootParam1) && !markedParams.contains(rootParam2)) {            
              val (newEnv4, optParamTerm1) = unifier.getParamTermS(rootParam1)(newEnv3)
              val (newEnv5, optParamTerm2) = unifier.getParamTermS(rootParam2)(newEnv4)
              (optParamTerm1, optParamTerm2) match {
                case (Some(paramTerm1), Some(paramTerm2)) =>
                  matchesAndReplaceS(paramTerm1, paramTerm2)((markedParams + rootParam1) + rootParam2, areChangedNewParams)(newEnv5)
                case (None, Some(paramTerm2))             =>
                  val (newEnv6, res3) = unifier.replaceParamS(rootParam1, paramTerm2)(newEnv5)
                  (newEnv6, res3.map { _ => true })
                case (Some(paramTerm1), None)             =>
                  val (newEnv6, res3) = unifier.replaceParamS(rootParam2, paramTerm1)(newEnv5)
                  (newEnv6, res3.map { _ => true })
                case (None, None)                         =>
                  val (newEnv6, res3) = unifier.unionParamsS(rootParam1, rootParam2)(newEnv5)
                  (newEnv6, res3.map { _ => true })
              }
            } else
              unifier.mismatchedTermErrorS(newEnv3).map { _.failure }
          case (_, Failure(err)) =>
            (newEnv3, err.failure)
          case (Failure(err), _) =>
            (newEnv3, err.failure)
        }
      case (param1, Right(childTerm2), areChangedNewParams, newEnv) =>
        val (newEnv2, res1) = unifier.findRootParamS(param1)(env)
        res1 match {
          case Success(rootParam1) =>
            if(!markedParams.contains(rootParam1)) {
              val (newEnv3, optParamTerm1) = unifier.getParamTermS(rootParam1)(newEnv2)
              optParamTerm1 match {
                case Some(paramTerm1) =>
                  matchesAndReplaceS(paramTerm1, childTerm2)(markedParams + param1, areChangedNewParams)(newEnv3)
                case None             =>
                  val (newEnv4, res2) = unifier.replaceParamS(param1, childTerm2)(newEnv3)
                  (newEnv4, res2.map { _ => true })
              }
            } else 
              unifier.mismatchedTermErrorS(newEnv2).map { _.failure }
          case Failure(err) =>
            (newEnv2, err.failure)
        }
    } (env)
}