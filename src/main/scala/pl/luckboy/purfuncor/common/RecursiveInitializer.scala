package pl.luckboy.purfuncor.common
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.util._
import pl.luckboy.purfuncor.common.Initializer._

trait RecursiveInitializer[E, L, C, N, F]
{
  def combinatorFromNode(node: N): C
  
  def recursiveCombinatorsFromNode(node: N): Set[L]
  
  def markedRecursiveCombinatorsFromNode(node: N): Set[L]
  
  def createNode(comb: C, recursiveCombLocs: Set[L], markedRecursiveCombLocs: Set[L]): N
  
  def addNodeS(node: N)(env: F): (F, Unit)
  
  def isRecursiveS(env: F): (F, Boolean)
  
  def isUninitializedGlobalVarS(loc: L)(env: F): (F, Boolean)
  
  def finallyInitializeGlobalVarS(loc: L, comb: C)(env: F): (F, Validation[E, Unit])

  def postInitializeGlobalVarS(oldNodes: Map[L, N])(env: F): (F, Validation[E, Unit])
  
  def nodesFromEnvironmentS(env: F): (F, Map[L, N])
  
  def withRecursiveS[T](combLocs: Set[L], newNodes: Map[L, N])(f: F => (F, T))(env: F): (F, T)
  
  def withClearS[T](f: F => (F, T))(env: F): (F, T)
}

object RecursiveInitializer
{
  def initializeGlobalVarS[E, L, C, I, N, F](loc: L, comb: C)(treeInfo: I)(env: F)(implicit recInit: RecursiveInitializer[E, L, C, N, F], init: Initializer[E, L, C, F], showing: Showing[Tree[L, C, I]]) =
    recInit.withClearS {
      env2 =>
        val (env3, isRecursive) = recInit.isRecursiveS(env2)
        val (env4, depLocs) = if(!isRecursive) varDependencesS(comb)(env3) else (env3, Set[L]())
        if(depLocs.isEmpty) {
          recInit.finallyInitializeGlobalVarS(loc, comb)(env4)
        } else {
          val (env5, nodes) = recInit.nodesFromEnvironmentS(env4)
          val (nonRecursiveDepLocs, recursiveDepLocs) = depLocs.partition(nodes.contains)
          val recursiveCombLocs = recursiveDepLocs ++ nonRecursiveDepLocs.flatMap {
            nodes.get(_).toSet.flatMap(recInit.recursiveCombinatorsFromNode)
          }
          val markedRecursiveCombLocs = (recursiveCombLocs & Set(loc)) ++ nonRecursiveDepLocs.flatMap {
            nodes.get(_).toSet.flatMap(recInit.markedRecursiveCombinatorsFromNode)
          }
          val (env6, _) = recInit.addNodeS(recInit.createNode(comb, recursiveCombLocs, markedRecursiveCombLocs))(env5)
          if((recursiveCombLocs &~ markedRecursiveCombLocs).isEmpty) {
            val combs = nodes.flatMap {
              case (l, n) => if(!(recInit.recursiveCombinatorsFromNode(n) & recursiveCombLocs).isEmpty) some(l -> recInit.combinatorFromNode(n)) else Map()
            }
            val (oldNodes, newNodes) = nodes.partition {
              case (l, n) => !(recInit.recursiveCombinatorsFromNode(n) & recursiveCombLocs).isEmpty
            }
            val (env7, res) = recInit.withRecursiveS(combs.keySet, newNodes) {
              initializeS(Tree(combs, treeInfo))(_) 
            } (env6)
            res.map { _ => recInit.postInitializeGlobalVarS(oldNodes)(env6) }.valueOr { e => (env7, e.failure) }
          } else
            (env4, ().success)
        }
    } (env)
  
  private def varDependencesS[E, L, C, I, N, F](comb: C)(env: F)(implicit recInit: RecursiveInitializer[E, L, C, N, F], init: Initializer[E, L, C, F]) =
    init.usedGlobalVarsFromCombinator(comb).foldLeft((env, Set[L]())) {
      case ((newEnv, ls), l) => recInit.isUninitializedGlobalVarS(l)(newEnv).mapElements(identity, if(_) ls + l else ls)
    }
}