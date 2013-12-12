package pl.luckboy.purfuncor.frontend
import scalaz._
import scalaz.Scalaz._

case class CombinatorNode[+T, +U, +V, W](
    comb: AbstractCombinator[T, U, V],
    recursiveCombSyms: Set[W],
    markedRecCombSyms: Set[W])