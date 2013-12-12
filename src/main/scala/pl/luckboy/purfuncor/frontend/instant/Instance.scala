package pl.luckboy.purfuncor.frontend.instant
import scalaz._
import scalaz.Scalaz._

sealed trait Instance[+T]

sealed trait GlobalInstance[+T] extends Instance[T]

case class PolyFunInstance[+T](loc: T) extends GlobalInstance[T]
case class ConstructInstance[+T](i: Int) extends GlobalInstance[T]
case class SelectInstance[+T](n: Int) extends GlobalInstance[T]

case class LocalInstance[+T](idx: Int) extends Instance[T]