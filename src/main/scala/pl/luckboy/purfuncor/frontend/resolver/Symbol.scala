package pl.luckboy.purfuncor.frontend.resolver
import scalaz._
import scalaz.Scalaz._

sealed trait Symbol
{
  override def toString =
    this match {
      case GlobalSymbol(names) =>"#." + names.list.mkString(".")
      case LocalSymbol(name)   => name
    }
}

case class GlobalSymbol(names: NonEmptyList[String]) extends Symbol
{
  def ++ (ss: List[String]) = GlobalSymbol(names :::> ss)
  
  def moduleSymbol = ModuleSymbol(names.list.init)
}
case class LocalSymbol(name: String) extends Symbol