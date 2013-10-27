package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._

case class InferenceLambdaInfo[T, U](typeTable: TypeTable[T, U], instanceTypes: Seq[Type[U]])

case class TypeTable[T, U](types: Map[T, Type[U]])
