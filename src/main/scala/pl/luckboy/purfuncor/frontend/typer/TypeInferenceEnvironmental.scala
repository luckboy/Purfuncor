package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol

trait TypeInferenceEnvironmental[E, L, M, N]
{
  def copyEnvironment(env: E): E
  
  def globalVarTypeFromEnvironment(env: E)(sym: GlobalSymbol): Type[N]
  
  def lambdaInfosFromEnvironment(env: E)(sym: Option[GlobalSymbol]): Map[Int, InferenceLambdaInfo[LocalSymbol, GlobalSymbol, GlobalSymbol]]
  
  def getLambdaInfoFromEnvironment(env: E)(lambdaIdx: Int): Option[InferenceLambdaInfo[M, N, L]]
  
  def globalTypeTableFromEnvironment(env: E): TypeTable[L, N]
  
  def withCurrentCombinatorLocation(env: E)(loc: Option[L]): E
}