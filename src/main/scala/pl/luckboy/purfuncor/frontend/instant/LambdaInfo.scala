package pl.luckboy.purfuncor.frontend.instant
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend.typer.InferredTypeTable

case class LambdaInfo[+T, U, V, +W](
    lambdaInfo: T,
    idx: Int,
    typeTable: InferredTypeTable[U, V],
    insts: Seq[Instance[W]])
{
  override def toString = {
    (if(!lambdaInfo.toString.isEmpty) List(lambdaInfo.toString) else Nil) ++
    List("idx=" + idx) ++
    (if(!typeTable.types.isEmpty) List("typeTable=" + typeTable.types.map { case (l, t) => l + ": " + t }.mkString(",")) else Nil) ++
    (if(!insts.isEmpty) List("insts=" + insts.mkString(",")) else Nil)
  }.mkString(";")
}