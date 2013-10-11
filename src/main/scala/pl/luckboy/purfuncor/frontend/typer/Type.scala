package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.kinder.NoKind
import pl.luckboy.purfuncor.frontend.kinder.InferredKind

sealed trait Type[T]

case class NoType[T]() extends Type[T]
{
  def errs: List[AbstractError] =
    throw new UnsupportedOperationException
  
  def toNoKind: NoKind =
    throw new UnsupportedOperationException
}

object NoType
{
  def fromError[T](err: AbstractError): NoType[T] =
    throw new UnsupportedOperationException
    
  def fromErrors[T](errs: NonEmptyList[AbstractError]): NoType[T] =
    throw new UnsupportedOperationException
    
  def fromNoKind[T](noKind: NoKind): NoType[T] =
    throw new UnsupportedOperationException
    
  def fromNoTypeValue[T, U, V, W](noTypeValue: NoTypeValue[T, U, V, W]): NoType[T] =
    throw new UnsupportedOperationException
}

case class InferredType[T](typeValueTerm: TypeValueTerm[T], argKinds: Seq[InferredKind]) extends Type[T]
case class InferringType[T](typeValueTerm: TypeValueTerm[T]) extends Type[T]
case class UninferredType[T]() extends Type[T]