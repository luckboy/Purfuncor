package pl.luckboy.purfuncor.frontend.resolver
import scalaz._
import scalaz.Scalaz._

case class ModuleSymbol(names: List[String])
{
  def + (name: String) = ModuleSymbol(names :+ name)
  
  def ++ (ss: List[String]) = ModuleSymbol(names ++ ss)
  
  def globalSymbol(name: String) = GlobalSymbol(toNel(names).map { _ :::> List(name) }.getOrElse(NonEmptyList(name)) )

  def globalSymbolFromNames(ss: NonEmptyList[String]) = GlobalSymbol(names <::: ss)
  
  override def toString = "#." + names.mkString(".")
}

object ModuleSymbol
{
  val root = ModuleSymbol(Nil)
}