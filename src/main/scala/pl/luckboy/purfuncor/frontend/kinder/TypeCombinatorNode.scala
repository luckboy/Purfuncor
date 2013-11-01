package pl.luckboy.purfuncor.frontend.kinder
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend._

case class TypeCombinatorNode[+T, +U, V](
    comb: AbstractTypeCombinator[T, lmbdindexer.TypeLambdaInfo[U]],
    recursiveCombSyms: Set[V],
    markedRecCombSyms: Set[V])