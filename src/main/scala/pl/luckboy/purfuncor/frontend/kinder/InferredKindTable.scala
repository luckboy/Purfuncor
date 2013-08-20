package pl.luckboy.purfuncor.frontend.kinder
import scalaz._
import scalaz.Scalaz._

case class InferredKindTable[T](kinds: Map[T, InferredKind])

object InferredKindTable
{
  def empty[T] = InferredKindTable[T](Map())
}