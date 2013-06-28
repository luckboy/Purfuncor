package pl.luckboy.purfuncor.frontend

object TypeBuiltinFunction extends Enumeration
{
  val Any = Value("Any")
  val Nothing = Value("Nothing")
  val Zero = Value("Zero")
  val NonZero = Value("NonZero")
  val Boolean = Value("Boolean")
  val Char = Value("Char")
  val Byte = Value("Byte")
  val Short = Value("Short")
  val Int = Value("Int")
  val Long = Value("Long")
  val Float = Value("Float")
  val Double = Value("Double")
  val Empty = Value("Empty")
  val NonEmpty = Value("NonEmpty")
  val Array = Value("Array")
  val Fun = Value("->")
  val Conj = Value("&")
  val Disj = Value("|")
}