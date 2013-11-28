package pl.luckboy.purfuncor.frontend.lmbdindexer

case class TypeLambdaInfo[+T](lambdaInfo: T ,idx: Int)
{
  override def toString = (if(!lambdaInfo.toString.isEmpty) "idx=" + lambdaInfo + ";" else "") + idx
}