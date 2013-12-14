package pl.luckboy.purfuncor.frontend.instant
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend.typer.InferredTypeTable

case class LambdaInfo[+T, U, V, +W](
    lambdaInfo: T,
    idx: Int,
    typeTable: InferredTypeTable[U, V],
    insts: Seq[Instance[W]],
    instArgs: Seq[InstanceArg[W, V]])