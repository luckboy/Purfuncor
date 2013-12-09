package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._

case class InferenceLambdaInfo[T, U](
    typeTable: TypeTable[T, U],
    polyFunType: Option[Type[U]],
    combTypeParams: Map[Int, Int])

case class TypeTable[T, U](types: Map[T, Type[U]])

object TypeTable
{
  def empty[T, U] = TypeTable[T, U](Map())
}