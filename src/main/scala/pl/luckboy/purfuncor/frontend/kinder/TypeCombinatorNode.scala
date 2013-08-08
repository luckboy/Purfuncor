package pl.luckboy.purfuncor.frontend.kinder
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.lmbdindexer.TypeLambdaInfo

case class TypeCombinatorNode[+T, U](
    comb: TypeCombinator[T, TypeLambdaInfo],
    recursiveCombSyms: Set[U],
    markedRecCombSyms: Set[U])