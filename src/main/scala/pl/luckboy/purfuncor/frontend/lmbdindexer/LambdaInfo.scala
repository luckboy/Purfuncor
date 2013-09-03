package pl.luckboy.purfuncor.frontend.lmbdindexer

case class LambdaInfo[+T](lambdaInfo: T, idx: Int)
{
  override def toString = (if(!lambdaInfo.toString.isEmpty) lambdaInfo + ";" else "") + idx
}