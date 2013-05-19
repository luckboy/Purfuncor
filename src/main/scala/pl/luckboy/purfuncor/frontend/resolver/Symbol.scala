package pl.luckboy.purfuncor.frontend.resolver
import scalaz._
import scalaz.Scalaz._

case class Symbol(names: NonEmptyList[String])
{
  override def toString = "_root_." + names.list.mkString(".")
}