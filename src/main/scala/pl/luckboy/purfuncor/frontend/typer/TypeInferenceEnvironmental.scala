package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._

trait TypeInferenceEnvironmental[E, L, M, N]
{
  def getLambdaInfoFromEnvironment(env: E)(lambdaIdx: Int): Option[InferenceLambdaInfo[M, N]]
  
  def globalTypeTableFromEnvironment(env: E): TypeTable[L, N]
  
  def withCurrentCombinatorLocation(env: E)(loc: Option[L]): E
}