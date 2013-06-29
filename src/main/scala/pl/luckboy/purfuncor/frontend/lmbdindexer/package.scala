package pl.luckboy.purfuncor.frontend
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._

package object lmbdindexer
{
  implicit val resolverTreeInfoTransformer: TreeInfoTransformer[resolver.TreeInfo] = new TreeInfoTransformer[resolver.TreeInfo] {
    override def transformTreeInfo[U](treeInfo: resolver.TreeInfo[parser.TypeLambdaInfo, U]): ValidationNel[AbstractError, resolver.TreeInfo[TypeLambdaInfo, U]] =
      LambdaIndexer.transformTypeTree(treeInfo.typeTree).map { resolver.TreeInfo(_) }
  }
}