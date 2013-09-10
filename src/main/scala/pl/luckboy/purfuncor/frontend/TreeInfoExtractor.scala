package pl.luckboy.purfuncor.frontend
import scalaz._
import scalaz.Scalaz._

trait TreeInfoExtractor[T, U]
{
  def typeTreeFromTreeInfo(treeInfo: T): U
}