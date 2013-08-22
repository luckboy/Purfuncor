package pl.luckboy.purfuncor.frontend.kinder
import scalaz._
import scalaz.Scalaz._

trait KindInferenceEnvironmentState[E, L]
{
  def instantiateLocalKindTablesS(env: E): (E, Validation[NoKind, Unit])
  
  def instantiateKindS(kind: Kind)(env: E): (E, Kind)
  
  def withTypeCombinatorLocationS[T](loc: Option[L])(f: E => (E, T))(env: E): (E, T)
  
  def withClearS[T](f: E => (E, T))(env: E): (E, T)
}