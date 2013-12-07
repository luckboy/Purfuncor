package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._

case class InferenceLambdaInfo[T, U, V](
    typeTable: TypeTable[T, U],
    instTypes: Seq[Type[U]],
    insts: Seq[LocalInstance[V]],
    polyFuns: Seq[AbstractPolyFunction[V]])

case class TypeTable[T, U](types: Map[T, Type[U]])

object TypeTable
{
  def empty[T, U] = TypeTable[T, U](Map())
}