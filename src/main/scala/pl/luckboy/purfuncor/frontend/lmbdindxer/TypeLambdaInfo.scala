package pl.luckboy.purfuncor.frontend.lmbdindxer

case class TypeLambdaInfo(idx: Int)
{
  override def toString = "/*" + idx + "*/"
}