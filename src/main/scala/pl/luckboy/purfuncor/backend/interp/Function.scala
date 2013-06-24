package pl.luckboy.purfuncor.backend.interp
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend.SimpleTerm

abstract class Function(val argCount: Int)
{
  def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]): (E, Value[T, U, V, W])
  
  def apply[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
    State(applyS[T, U, V, W, E](argValues))
}