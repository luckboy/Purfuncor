package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend._

case class CombinatorNode[+T, +U, +V, W](
    comb: Combinator[T, lmbdindexer.LambdaInfo[U], V],
    recursiveCombSyms: Set[W],
    markedRecCombSyms: Set[W])