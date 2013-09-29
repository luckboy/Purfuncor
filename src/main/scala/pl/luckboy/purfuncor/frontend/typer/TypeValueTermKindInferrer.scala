package pl.luckboy.purfuncor.frontend.typer
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend
import pl.luckboy.purfuncor.frontend.kinder.Kind
import pl.luckboy.purfuncor.frontend.kinder.NoKind
import pl.luckboy.purfuncor.frontend.kinder.InferredKind

object TypeValueTermKindInferrer
{
  def inferTypeValueTermS[T, U, E](term: TypeValueTerm[T])(env: E)(implicit inferrer: Inferrer[U, Kind, E]): (E, Kind) =
    throw new UnsupportedOperationException    
}