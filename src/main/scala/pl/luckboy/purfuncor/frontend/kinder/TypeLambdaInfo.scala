package pl.luckboy.purfuncor.frontend.kinder
import scalaz._
import scalaz.Scalaz._

case class TypeLambdaInfo[T, U](lambdaInfo: T, kindTable: InferredKindTable[U])
{
  override def toString =
    (if(!lambdaInfo.toString.isEmpty) lambdaInfo + ";" else "") + kindTable.kinds.map { case (l, k) => l + ": " + k }.mkString(",")
}