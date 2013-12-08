package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._

case class LambdaInfo[+T, U, V](
    lambdaInfo: T,
    idx: Int,
    typeTable: InferredTypeTable[U, V],
    polyFunType: Option[InferredType[V]])
{
  override def toString =
    (if(!lambdaInfo.toString.isEmpty) lambdaInfo + ";" else "") +
    "idx=" + idx + ";" +
    (if(!typeTable.types.isEmpty) "typeTable=" + typeTable.types.map { case (l, t) => l + ": " + t }.mkString(",") + ";" else "") +
    polyFunType.map { "polyFunType=" + _ }.getOrElse("")
}