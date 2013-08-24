package pl.luckboy.purfuncor.frontend.kinder
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol

trait KindInferenceEnvironmental[E, L, M]
{
  def copyEnvironment(env: E): E
  
  def globalTypeVarKindFromEnvironment(env: E)(sym: GlobalSymbol): Kind
  
  def localKindTablesFromEnvironment(env: E)(sym: Option[GlobalSymbol]): Map[Int, KindTable[LocalSymbol]]
  
  def globalKindTableFromEnvironment(env: E): KindTable[L]
  
  def withCurrentTypeCombinatorLocation(env: E)(loc: Option[L]): E
  
  def getLocalKindTableFromEnvironment(env: E)(lambdaIdx: Int): Option[KindTable[M]]
}