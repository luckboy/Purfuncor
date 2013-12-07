package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._

case class LambdaInfo[+T, U, V, +W](
    lambdaInfo: T,
    idx: Int,
    typeTable: InferredTypeTable[U, V],
    instTypes: Seq[InferredType[V]],
    insts: Seq[LocalInstance[W]],
    polyFuns: Seq[AbstractPolyFunction[W]])
{
  override def toString =
    (if(!lambdaInfo.toString.isEmpty) lambdaInfo + ";" else "") +
    "idx=" + idx + ";" +
    (if(!typeTable.types.isEmpty) "typeTable=" + typeTable.types.map { case (l, t) => l + ": " + t }.mkString(",") + ";" else "") +
    (if(!instTypes.isEmpty) "instTypes=" + instTypes.mkString(",") + ";" else "") +
    (if(!insts.isEmpty) "insts=" + insts.mkString(",") + ";" else "") +
    (if(!polyFuns.isEmpty) "polyFuns=" + polyFuns.mkString(",") else "")
}