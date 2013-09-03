package pl.luckboy.purfuncor.frontend.kinder
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

trait TreeInfoTransformer[T[_, _], U, V]
{
  def transformTreeInfo[W, X, E](treeInfo: T[lmbdindexer.TypeLambdaInfo[W], X])(env: E)(implicit enval: KindInferenceEnvironmental[E, U, V]): ValidationNel[AbstractError, T[TypeLambdaInfo[W, V], TypeTreeInfo[X, U]]]
}