package pl.luckboy.purfuncor.frontend.instant
import scalaz._
import scalaz.Scalaz._

case class InstantiationLambdaInfo[+T](insts: Seq[Instance[T]])