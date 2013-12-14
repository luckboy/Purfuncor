package pl.luckboy.purfuncor.frontend.instant
import scalaz._
import scalaz.Scalaz._

case class InstantiationLambdaInfo[+T, U](insts: Seq[Instance[T]], instArgs: Seq[InstanceArg[T, U]])