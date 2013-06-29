package pl.luckboy.purfuncor.frontend.lmbdindexer

case class LambdaInfo(idx: Int)
{
  override def toString = "/*" + idx + "*/"
}