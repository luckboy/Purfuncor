package pl.luckboy.purfuncor.frontend.lmbdindxer

case class LambdaInfo(idx: Int)
{
  override def toString = "/*" + idx + "*/"
}