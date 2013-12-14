package pl.luckboy.purfuncor.frontend.instant
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend.typer.InferredType

case class InstanceArg[+T, V](polyFun: AbstractPolyFunction[T], typ: InferredType[V])