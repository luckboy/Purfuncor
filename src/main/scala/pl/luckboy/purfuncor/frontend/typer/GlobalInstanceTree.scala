package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._

case class GlobalInstanceTree[T, U, V](instTables: Map[T, GlobalInstanceTable[U, V]])

case class GlobalInstanceTable[T, U](pairs: Seq[(InferredType[T], U)])