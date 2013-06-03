package pl.luckboy.purfuncor.common
import scala.collection.immutable.Stack
import scala.annotation.tailrec
import scalaz._
import scalaz.Scalaz._

trait Initializer[E, L, C, F]
{
  def markedGlobalVars(env: F): Set[L]
  
  def usedGlobalVars(comb: C): Set[L]
  
  def prepareGlobalVarS(loc: L)(env: F): (F, Unit)
  
  def initializeGlobalVarS(loc: L, comb: C)(env: F): (F, Validation[E, Unit])
  
  def undefinedGlobalVarError: E
}

object Initializer
{
  def initializeS[E, L, C, I, F](tree: Tree[L, C, I])(env: F)(implicit init: Initializer[E, L, C, F]): (F, Validation[E, Unit]) =
    tree.combs.keys.foldLeft((init.markedGlobalVars(env), List[L]()).success[E]) {
      case (Success((markedLocs, locs)), loc) => 
        varDependenceS(tree)(loc)(markedLocs).map {
          case (markedLocs2, locs2) => (markedLocs2, locs ++ locs2)
        }
      case (Failure(err), _)                  =>
        err.failure
    } match {
      case Success((_, locs)) =>
        val (env3, _) = locs.foldLeft((env, ())) { 
          case ((env2, ()), loc) =>
            init.prepareGlobalVarS(loc)(env2)
        }
        val (env5, res2) = locs.foldLeft((env3, ().success[E])) {
          case ((env4, Success(_)), loc) =>
            tree.combs.get(loc).map { init.initializeGlobalVarS(loc, _)(env4) }.getOrElse {
              (env4, init.undefinedGlobalVarError.failure[Unit])
            }
          case (res, _)                  =>
            res
        }
        res2.map { u => (env5, u.success[E]) }.getOrElse((env, res2))
      case Failure(err)       =>
        (env, err.failure)
    }
  
  def initialize[E, L, C, I, F](tree: Tree[L, C, I])(implicit init: Initializer[E, L, C, F]) =
    State(initializeS[E, L, C, I, F](tree))
    
  def varDependenceS[E, L, C, I, F](tree: Tree[L, C, I])(loc: L)(markedLocs: Set[L])(implicit init: Initializer[E, L, C, F]): Validation[E, (Set[L], List[L])] =
    tree.combs.get(loc).map {
      comb => dfs(tree, Stack((loc, init.usedGlobalVars(comb).toList)), Nil)(markedLocs)
    }.getOrElse((markedLocs, Nil).success)

  @tailrec
  private def dfs[E, L, C, I, F](tree: Tree[L, C, I], stck: Stack[(L, List[L])], locs: List[L])(markedLocs: Set[L])(implicit init: Initializer[E, L, C, F]): Validation[E, (Set[L], List[L])] =
    if(!stck.isEmpty) {
      val ((loc, childLocs), stck2) = stck.pop2
      childLocs match {
        case childLoc :: nextChildLocs =>
          val stck3 = stck.push((loc, nextChildLocs))
          if(!markedLocs.contains(childLoc)) {
            val markedLocs2 = markedLocs + childLoc
            tree.combs.get(childLoc) match {
              case Some(comb) =>
                dfs(tree, stck3.push((childLoc, init.usedGlobalVars(comb).toList)), locs)(markedLocs2)
              case None       =>
                init.undefinedGlobalVarError.failure
            }
          } else 
            dfs(tree, stck3, locs)(markedLocs)
        case Nil                       =>
          dfs(tree, stck2, loc :: locs)(markedLocs)
      }
    } else {
      (markedLocs, locs).success
    }
}