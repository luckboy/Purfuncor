package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._

case class InferenceInstanceArg[T, U](fun: AbstractPolyFunction[T], typ: Type[U])