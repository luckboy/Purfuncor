package pl.luckboy.purfuncor.frontend
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._

package object lmbdindexer
{
  implicit val resolverTreeInfoTransformer: TreeInfoTransformer[resolver.TreeInfo] = new TreeInfoTransformer[resolver.TreeInfo] {
    override def transformTreeInfo[U, V](treeInfo: resolver.TreeInfo[U, V]): ValidationNel[AbstractError, resolver.TreeInfo[TypeLambdaInfo[U], V]] =
      (LambdaIndexer.transformTypeTree(treeInfo.typeTree) |@| LambdaIndexer.transformSelectConstructInstances(treeInfo.selectConstructInsts)) {
        (tt, scis) => resolver.TreeInfo(tt, treeInfo.insts, scis)
      }
  }
}