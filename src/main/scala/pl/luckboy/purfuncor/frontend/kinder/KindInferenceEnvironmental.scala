package pl.luckboy.purfuncor.frontend.kinder
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol

trait KindInferenceEnvironmental[E, L, M]
{
  def copyEnvironment(env: E): E
  
  def globalKindTableFromEnvironment(env: E): KindTable[L]
  
  def withCurrentTypeCombinatorLocation(env: E)(loc: Option[L]): E
  
  def getLocalKindTableFromEnvironment(env: E)(lambdaIdx: Int): Option[KindTable[M]]
}