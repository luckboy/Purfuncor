package pl.luckboy.purfuncor.backend.interp
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend.SimpleTerm

abstract class Function(val argCount: Int)
{
  def applyS[T, U, V, E](argValues: Seq[Value[T, U, V]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U], E, Value[T, U, V]]): (E, Value[T, U, V])
  
  def apply[T, U, V, E](argValues: Seq[Value[T, U, V]])(implicit eval: Evaluator[SimpleTerm[T, U], E, Value[T, U, V]]) =
    State(applyS[T, U, V, E](argValues))
}