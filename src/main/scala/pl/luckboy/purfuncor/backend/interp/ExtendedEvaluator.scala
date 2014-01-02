package pl.luckboy.purfuncor.backend.interp
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

trait ExtendedEvaluator[L, E, V]
{
  def variableS(value: V, lambdaInfo: L)(env: E): (E, V)
  
  def constructS(n: Int, lambdaInfo: L)(env: E): (E, V)
  
  def selectS[T, U](value: V, cases: Seq[Case[T, L, U]], lambdaInfo: L)(env: E): (E, Validation[V, Case[T, L, U]])
}