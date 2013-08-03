package pl.luckboy.purfuncor.common
import scala.collection.immutable.Queue
import scala.annotation.tailrec
import scalaz._
import scalaz.Scalaz._

trait InferenceFinalizer[E, L, F]
{
  def inferringInfoGlobalVarsFromEnvironmentS(env: F): (F, Set[L])

  def recursiveGlobalVarsFromEnvironmentS(env: F): (F, Set[L])
  
  def usedGlobalVarsFromGlobalVarS(loc: L)(env: F): (F, Validation[E, Set[L]])
  
  def finalizeGlobalVarInfoInferencesS(locs: Set[L])(env: F): (F, Validation[E, Unit])

  def withSaveS[T, U](f: F => (F, Validation[T, U]))(env: F): (F, Validation[T, U])
}

object InferenceFinalizer
{
  def finalizeInferenceS[E, L, F](env: F)(implicit inferenceFinal: InferenceFinalizer[E, L, F]): (F, Validation[E, Unit]) =
    inferenceFinal.withSaveS {
      env2 =>
        val (env3, res) = recursiveGlobalVarDependencesS(env2)
        res.map {
          locs =>
            val (env4, inferringInfoGlobalVars) = inferenceFinal.inferringInfoGlobalVarsFromEnvironmentS(env3)
            inferenceFinal.finalizeGlobalVarInfoInferencesS(inferringInfoGlobalVars &~ locs)(env4)
        }.valueOr { err => (env3, err.failure) }
    } (env)

  def finalizeInference[E, L, F](implicit inferenceFinal: InferenceFinalizer[E, L, F]) =
    State(finalizeInferenceS[E, L, F])
    
  def recursiveGlobalVarDependencesS[E, L, F](env: F)(implicit inferenceFinal: InferenceFinalizer[E, L, F]) = {
    val (env2, locs) = inferenceFinal.recursiveGlobalVarsFromEnvironmentS(env)
    bfsS(Queue[L]().enqueue(locs))(locs.toSet)(env2)
  }
  
  @tailrec
  private def bfsS[E, L, F](q: Queue[L])(markedLocs: Set[L])(env: F)(implicit inferenceFinal: InferenceFinalizer[E, L, F]): (F, Validation[E, Set[L]]) =
    if(!q.isEmpty) {
      val (loc, q2) = q.dequeue
      inferenceFinal.usedGlobalVarsFromGlobalVarS(loc)(env) match {
        case (env2, Success(neighborLocs)) =>
          bfsS(q2.enqueue(neighborLocs &~ markedLocs))(markedLocs ++ neighborLocs)(env2)
        case (env2, Failure(err))          =>
          (env2, err.failure)
      }
    } else {
      (env, markedLocs.success)
    }
}