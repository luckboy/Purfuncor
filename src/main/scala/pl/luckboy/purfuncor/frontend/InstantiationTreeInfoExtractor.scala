package pl.luckboy.purfuncor.frontend
import scalaz._
import scalaz.Scalaz._

trait InstantiationTreeInfoExtractor[T, U, V, W]
{
  def instancesFromTreeInfo(treeInfo: T): Map[U, List[V]]
  
  def selectConstructInstancesFromTreeInfo(treeInfo: T): List[W]
}