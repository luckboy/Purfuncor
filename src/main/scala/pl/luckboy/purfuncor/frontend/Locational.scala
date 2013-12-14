package pl.luckboy.purfuncor.frontend

trait Locational[T, U, V]
{
  def getGlobalLocationFromLocation(loc: T): Option[U]
  
  def getLocalLocationFromLocation(loc: T): Option[V]
}