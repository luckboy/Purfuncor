package pl.luckboy.purfuncor.frontend.kinder
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol

trait KindInferenceEnvironmental[T, U, V]
{
  def copyEnvironment(env: T): T
  
  def globalKindTableFromEnvironment(env: T): KindTable[U]
  
  def withCurrentCombinatorLocation(env: T)(loc: Option[U]): T
  
  def getLocalKindTableFromEnvironment(env: T)(lambdaIdx: Int): Option[KindTable[V]]
}