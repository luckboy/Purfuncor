package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._

case class LambdaInfo[+T, U, V](
    lambdaInfo: T,
    idx: Int,
    typeTable: InferredTypeTable[U, V],
    polyFunType: Option[InferredType[V]],
    combTypeParams: Map[Int, Int])
{
  override def toString =
    (if(!lambdaInfo.toString.isEmpty) lambdaInfo + ";" else "") +
    "idx=" + idx + ";" +
    (if(!typeTable.types.isEmpty) "typeTable=" + typeTable.types.map { case (l, t) => l + ": " + t }.mkString(",") + ";" else "") +
    polyFunType.map { "polyFunType=" + _  + ";" }.getOrElse("") +
    (if(!combTypeParams.isEmpty) "combTypeParams=" + combTypeParams.map { p => p._1 + "->" + p._2 }.mkString(",") else "")
}