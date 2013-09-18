package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._

case class TypeTable[T, U](types: Map[T, Type[U]])