package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

object TypeValueTermUtils
{
  def typeParamsFromTypeValueTerm[T](term: TypeValueTerm[T]): Set[Int] =
    throw new UnsupportedOperationException
    
  def substituteTypeValueLambdas[T](term: TypeValueTerm[T], lambdas: Map[Int, TypeValueLambda[T]]): Option[TypeValueTerm[T]] =
    throw new UnsupportedOperationException
}