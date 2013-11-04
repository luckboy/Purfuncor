package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._

case class InferredTypeTable[T, U](types: Map[T, InferredType[U]])

object InferredTypeTable
{
  def empty[T, U] = InferredTypeTable[T, U](Map())
}