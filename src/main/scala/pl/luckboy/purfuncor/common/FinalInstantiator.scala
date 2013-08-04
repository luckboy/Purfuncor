package pl.luckboy.purfuncor.common
import scala.collection.immutable.Queue
import scala.annotation.tailrec
import scalaz._
import scalaz.Scalaz._

trait FinalInstantiator[E, L, F]
{
  def uninstantiatedInfoGlobalVarsFromEnvironmentS(env: F): (F, Set[L])
  
  def usedGlobalVarsFromGlobalVarS(loc: L)(env: F): (F, Validation[E, Set[L]])
  
  def instantiateGlobalVarInfosS(locs: Set[L])(env: F): (F, Validation[E, Unit])

  def withSaveS[T, U](f: F => (F, Validation[T, U]))(env: F): (F, Validation[T, U])
}

object FinalInstantiator
{
  def finallyInstantiateS[E, L, F](loc: L)(env: F)(implicit finalInst: FinalInstantiator[E, L, F]): (F, Validation[E, Unit]) =
    finalInst.withSaveS {
      env2 =>
        val (env3, res) = uninstantiatedInfoVarDependencesS(loc)(env2)
        res.map {
          locs =>
            val (env4, uninstantiatedInfoGlobalVars) = finalInst.uninstantiatedInfoGlobalVarsFromEnvironmentS(env3)
            if((locs &~ uninstantiatedInfoGlobalVars).isEmpty)
              finalInst.instantiateGlobalVarInfosS(locs)(env4)
            else
              (env4, ().success)
        }.valueOr { err => (env3, err.failure) }
    } (env)

  def finallyInstantiate[E, L, F](loc: L)(implicit finalInst: FinalInstantiator[E, L, F]) =
    State(finallyInstantiateS[E, L, F](loc))
    
  def uninstantiatedInfoVarDependencesS[E, L, F](loc: L)(env: F)(implicit finalInst: FinalInstantiator[E, L, F]) =
    bfsS(Queue[L]().enqueue(loc))(Set(loc))(env)
  
  @tailrec
  private def bfsS[E, L, F](q: Queue[L])(markedLocs: Set[L])(env: F)(implicit finalInst: FinalInstantiator[E, L, F]): (F, Validation[E, Set[L]]) =
    if(!q.isEmpty) {
      val (loc, q2) = q.dequeue
      finalInst.usedGlobalVarsFromGlobalVarS(loc)(env) match {
        case (env2, Success(neighborLocs)) =>
          bfsS(q2.enqueue(neighborLocs &~ markedLocs))(markedLocs ++ neighborLocs)(env2)
        case (env2, Failure(err))          =>
          (env2, err.failure)
      }
    } else {
      (env, markedLocs.success)
    }
}