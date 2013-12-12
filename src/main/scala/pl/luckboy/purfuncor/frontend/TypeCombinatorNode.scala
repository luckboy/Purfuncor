package pl.luckboy.purfuncor.frontend
import scalaz._
import scalaz.Scalaz._

case class TypeCombinatorNode[+T, +U, V](
    comb: AbstractTypeCombinator[T, U],
    recursiveCombSyms: Set[V],
    markedRecCombSyms: Set[V])