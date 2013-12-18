package pl.luckboy.purfuncor.frontend.instant
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend.typer.InferredType

case class InstanceArgTable[T, U](instArgs: Map[T, Seq[InstanceArg[T, U]]])

case class InstanceArg[+T, U](polyFun: AbstractPolyFunction[T], typ: InferredType[U])