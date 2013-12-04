package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

case class InstanceTable[T, U](pairs: Seq[(Type[T], U)])
{
  def findInstsS[V, E](typ: Type[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T]): (E, Validation[NoType[T], Seq[U]]) =
    throw new UnsupportedOperationException
  
  def addInstS[V, E](typ: Type[T], inst: U)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T]): (E, Validation[NoType[T], Option[InstanceTable[T, U]]]) =
    throw new UnsupportedOperationException
}

object InstanceTable
{
  def empty[T, U] = InstanceTable[T, U](Seq())
  
  def fromGlobalInstanceTable[T, U](instTable: GlobalInstanceTable[T, U]) = InstanceTable[T, U](instTable.pairs)
}