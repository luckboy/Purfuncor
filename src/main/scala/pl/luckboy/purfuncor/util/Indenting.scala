package pl.luckboy.purfuncor.util

trait Indenting[T]
{
  def indentedStringFrom(x: T)(n: Int): String
}