package pl.luckboy.purfuncor.backend.interp
import scalaz._
import scalaz.Scalaz._

trait ExtendedInitializer[-I, L]
{
  def usedGlobalVarsFromLambdaInfo(lambdaInfo: I): Set[L]
}