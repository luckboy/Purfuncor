package pl.luckboy.purfuncor.frontend.resolver

case class ModuleSymbol(names: List[String])
{
  override def toString = "_root_." + names.mkString(".")
}

object ModuleSymbol
{
  val root = ModuleSymbol(Nil)
}