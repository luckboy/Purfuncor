package pl.luckboy.purfuncor.frontend.instant
import scalaz._
import scalaz.Scalaz._

trait InstantiationEnvironmental[E, L, M]
{
  def copyEnvironment(env: E): E
  
  def getLambdaInfoFromEnvironment(env: E)(lambdaIdx: Int): Option[InstantiationLambdaInfo[L]]
  
  def withCurrentCombinatorLocation(env: E)(loc: Option[L]): E
  
  def treeGlobalInstanceTreeFromEnvironment(env: E): InstanceTree[AbstractPolyFunction[L], M, GlobalInstance[L]]
  
  def instanceArgTableFromFromEnvironment(env: E): InstanceArgTable[L, M]
}