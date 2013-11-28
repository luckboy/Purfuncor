package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._

case class LambdaInfo[+T, U, V](
    lambdaInfo: T,
    typeTable: InferredTypeTable[U, V],
    instanceTypes: Seq[InferredType[V]])
{
  override def toString =
    (if(!lambdaInfo.toString.isEmpty) lambdaInfo + ";" else "") +
    (if(!typeTable.types.isEmpty) "typeTable=" + typeTable.types.map { case (l, t) => l + ": " + t }.mkString(",") + ";" else "") +
    (if(!instanceTypes.isEmpty) "instanceTypes=" + instanceTypes.mkString(",") else "")
}