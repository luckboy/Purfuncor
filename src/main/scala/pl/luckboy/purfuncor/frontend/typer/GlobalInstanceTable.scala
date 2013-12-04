package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._

case class GlobalInstanceTable[T, U](pairs: Seq[(InferredType[T], U)])