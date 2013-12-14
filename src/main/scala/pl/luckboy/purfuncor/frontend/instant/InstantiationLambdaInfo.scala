package pl.luckboy.purfuncor.frontend.instant
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend.typer.InferredTypeTable

case class InstantiationLambdaInfo[+T, U](insts: Seq[Instance[T]], instArgs: Seq[InstanceArg[T, U]])