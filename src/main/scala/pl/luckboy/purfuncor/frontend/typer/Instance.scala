package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._

sealed trait Instance[+T]

case class GlobalInstance[+T](loc: T) extends Instance[T]
case class LocalInstance[+T](idx: Int) extends Instance[T]
case class ConstructInstance[+T](i: Int) extends Instance[T]
case class SelectInstance[+T](n: Int) extends Instance[T]