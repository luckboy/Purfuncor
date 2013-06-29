package pl.luckboy.purfuncor.frontend.lmbdindxer
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

trait TreeInfoTransformer[T[_, _]]
{
  def transformTreeInfo[U](treeInfo: T[parser.TypeLambdaInfo, U]): ValidationNel[AbstractError, T[parser.TypeLambdaInfo, U]]
}