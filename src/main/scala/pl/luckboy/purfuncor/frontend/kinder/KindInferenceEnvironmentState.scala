package pl.luckboy.purfuncor.frontend.kinder
import scalaz._
import scalaz.Scalaz._

trait KindInferenceEnvironmentState[T, U]
{
  def instantiateLocalKindTablesS(env: T): (T, Validation[NoKind, Unit])
  
  def instantiateKindS(kind: Kind)(env: T): (T, Kind)
  
  def withTypeCombinatorLocationS[V](loc: Option[U])(f: T => (T, V))(env: T): (T, V)
  
  def withClearS[V](f: T => (T, V))(env: T): (T, V)
}