package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._

case class LambdaInfo[+T, U, V](
    lambdaInfo: T,
    typeTable: InferredTypeTable[U, V],
    instanceTypes: Seq[InferredType[V]])