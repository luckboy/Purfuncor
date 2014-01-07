/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.common
import scala.collection.immutable.Stack
import scala.annotation.tailrec
import scalaz._
import scalaz.Scalaz._

trait Initializer[E, L, C, F]
{
  def globalVarsFromEnvironmentS(env: F): (F, Set[L])
  
  def usedGlobalVarsFromCombinator(comb: C): Set[L]
  
  def prepareGlobalVarS(loc: L)(env: F): (F, Unit)
  
  def initializeGlobalVarS(loc: L, comb: C)(env: F): (F, Validation[E, Unit])
  
  def checkEnvironmentS(env: F): (F, Validation[E, Unit])
  
  def undefinedGlobalVarError: E

  def withSaveS[T, U](f: F => (F, Validation[T, U]))(env: F): (F, Validation[T, U])
}

object Initializer
{
  def initializeS[E, L, C, I, F](tree: Tree[L, C, I])(env: F)(implicit init: Initializer[E, L, C, F]) =
    init.withSaveS {
      env2 =>
        val (env3, globalVars) = init.globalVarsFromEnvironmentS(env2)
        tree.combs.keys.foldLeft((globalVars, List[L]()).success[E]) {
          case (Success((markedLocs, locs)), loc) => 
            varDependencesS(tree)(loc)(markedLocs).map {
              case (markedLocs2, locs2) => (markedLocs2, locs ++ locs2)
            }
          case (Failure(err), _)                  =>
            err.failure
        } match {
          case Success((_, locs)) =>
            val (env5, _) = locs.foldLeft((env3, ())) { 
              case ((env4, ()), loc) =>
                init.prepareGlobalVarS(loc)(env4)
            }
            val (env7, res2) = locs.foldLeft((env5, ().success[E])) {
              case ((env6, Success(_)), loc) =>
                tree.combs.get(loc).map { init.initializeGlobalVarS(loc, _)(env6) }.getOrElse {
                  (env6, init.undefinedGlobalVarError.failure[Unit])
                }
              case (res, _)                  =>
                res
            }
            res2.map {
              _ =>
                val (env8, res3) = init.checkEnvironmentS(env7)
                res3.map { u => (env8, u.success[E]) }.getOrElse((env8, res3))
            }.getOrElse((env7, res2))
          case Failure(err)       =>
            (env3, err.failure)
        }
    } (env)
  
  def initialize[E, L, C, I, F](tree: Tree[L, C, I])(implicit init: Initializer[E, L, C, F]) =
    State(initializeS[E, L, C, I, F](tree))
    
  def varDependencesS[E, L, C, I, F](tree: Tree[L, C, I])(loc: L)(markedLocs: Set[L])(implicit init: Initializer[E, L, C, F]): Validation[E, (Set[L], List[L])] =
    if(!markedLocs.contains(loc))
      tree.combs.get(loc).map {
        comb => 
          dfs(tree, Stack((loc, init.usedGlobalVarsFromCombinator(comb).toList)), Nil)(markedLocs + loc).map {
            case (markedLocs2, locs2) => (markedLocs2, locs2.reverse)
          }
      }.getOrElse((markedLocs, Nil).success)
    else
      (markedLocs, Nil).success

  @tailrec
  private def dfs[E, L, C, I, F](tree: Tree[L, C, I], stck: Stack[(L, List[L])], locs: List[L])(markedLocs: Set[L])(implicit init: Initializer[E, L, C, F]): Validation[E, (Set[L], List[L])] =
    if(!stck.isEmpty) {
      val ((loc, neighborLocs), stck2) = stck.pop2
      neighborLocs match {
        case neighborLoc :: nextNeighborLocs =>
          val stck3 = stck2.push((loc, nextNeighborLocs))
          if(!markedLocs.contains(neighborLoc)) {
            val markedLocs2 = markedLocs + neighborLoc
            tree.combs.get(neighborLoc) match {
              case Some(comb) =>
                dfs(tree, stck3.push((neighborLoc, init.usedGlobalVarsFromCombinator(comb).toList)), locs)(markedLocs2)
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
