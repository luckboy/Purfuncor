package pl.luckboy.purfuncor.util

trait Showing[T]
{
  def stringFrom(x: T): String
}