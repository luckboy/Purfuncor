package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._

case class TreeInfo[+T, U, V](treeInfo: T, typeTable: InferredTypeTable[U, V])
{
  override def toString =
    (if(!treeInfo.toString.isEmpty) treeInfo + "\n" else "") +
    "//// typeTables\n" +
    typeTable.types.map { case (l, t) => "// " + l + ": " + t + "\n" }.mkString("\n")
}