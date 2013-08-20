package pl.luckboy.purfuncor.frontend.kinder
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend._

case class TypeCombinatorNode[+T, U](
    comb: TypeCombinator[T, lmbdindexer.TypeLambdaInfo],
    recursiveCombSyms: Set[U],
    markedRecCombSyms: Set[U])