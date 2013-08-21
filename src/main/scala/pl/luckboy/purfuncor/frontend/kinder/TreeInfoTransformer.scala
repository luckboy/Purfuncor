package pl.luckboy.purfuncor.frontend.kinder
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

trait TreeInfoTransformer[T[_, _]]
{
  def transformTreeInfo[U, V, E](treeInfo: T[lmbdindexer.TypeLambdaInfo, resolver.TypeTreeInfo])(env: E)(implicit enval: KindInferenceEnvironmental[E, U, V]): ValidationNel[AbstractError, T[TypeLambdaInfo[V], TypeTreeInfo[U]]]
}