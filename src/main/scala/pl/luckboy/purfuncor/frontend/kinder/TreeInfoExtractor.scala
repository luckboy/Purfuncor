package pl.luckboy.purfuncor.frontend.kinder
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common.Tree

trait TreeInfoExtractor[T, U]
{
  def typeTreeFromTreeInfo(treeInfo: T): U
}