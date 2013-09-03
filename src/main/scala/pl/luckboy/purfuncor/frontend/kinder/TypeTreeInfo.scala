package pl.luckboy.purfuncor.frontend.kinder
import scalaz._
import scalaz.Scalaz._

case class TypeTreeInfo[T, U](treeInfo: T, kindTable: InferredKindTable[U])
{
  override def toString = 
    (if(!treeInfo.toString.isEmpty) treeInfo + "\n" else "") + kindTable.kinds.map { case (l, k) => "// " + l + ": " + k + "\n" }.mkString("\n")
}