package pl.luckboy.purfuncor.frontend.resolver

sealed trait TreeInfo
{
  override def toString = ""
}
case object TreeInfo extends TreeInfo