package pl.luckboy.purfuncor.frontend.kinder
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.common.Tree

trait TreeInfoExtractor[T, U, V]
{
  def typeTreeFromTreeInfo(treeInfo: T): Tree[V, AbstractTypeCombinator[U, lmbdindexer.TypeLambdaInfo], resolver.TypeTreeInfo]
}