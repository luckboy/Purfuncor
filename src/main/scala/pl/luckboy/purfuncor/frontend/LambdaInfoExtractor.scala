package pl.luckboy.purfuncor.frontend

trait LambdaInfoExtractor[T, U]
{
  def instancesFromLambdaInfo(lambdaInfo: T): Seq[U]
}