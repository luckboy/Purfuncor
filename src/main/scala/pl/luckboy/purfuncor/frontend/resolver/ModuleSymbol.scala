package pl.luckboy.purfuncor.frontend.resolver
import scalaz._
import scalaz.Scalaz._

case class ModuleSymbol(names: List[String])
{
  def + (name: String) = ModuleSymbol(names :+ name)
  
  def ++ (names: List[String]) = ModuleSymbol(this.names ++ names)
  
  def globalSymbolFromName(name: String) = GlobalSymbol(toNel(names).map { _ :::> List(name) }.getOrElse(NonEmptyList(name)) )

  def globalSymbolFromNames(names: NonEmptyList[String]) = GlobalSymbol(this.names <::: names)
  
  override def toString = "#." + names.mkString(".")
}

object ModuleSymbol
{
  val root = ModuleSymbol(Nil)
}