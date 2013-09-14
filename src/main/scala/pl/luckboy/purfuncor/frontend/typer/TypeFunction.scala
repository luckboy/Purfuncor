package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

abstract class TypeFunction(val argCount: Int)
{
  def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]): (E, TypeValue[T, U, V, W])

  def apply[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
    State(applyS[T, U, V, W, E](argValues))
}