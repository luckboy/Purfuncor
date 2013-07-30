package pl.luckboy.purfuncor.frontend.kinder

case class KindTable[T](kinds: Map[T, Kind])

object KindTable
{
  def empty[T] = KindTable[T](Map())
}