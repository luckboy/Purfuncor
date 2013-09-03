package pl.luckboy.purfuncor.frontend.lmbdindexer
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

trait TreeInfoTransformer[T[_, _]]
{
  def transformTreeInfo[U, V](treeInfo: T[U, V]): ValidationNel[AbstractError, T[TypeLambdaInfo[U], V]]
}