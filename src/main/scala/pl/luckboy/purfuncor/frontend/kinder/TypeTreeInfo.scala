package pl.luckboy.purfuncor.frontend.kinder
import scalaz._
import scalaz.Scalaz._

case class TypeTreeInfo[T](kindTable: InferredKindTable[T])
{
  override def toString = kindTable.kinds.map { case (l, k) => "// " + l + ": " + k + "\n" }.mkString("\n")
}