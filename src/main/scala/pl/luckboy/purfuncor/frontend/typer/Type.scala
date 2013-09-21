package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend.kinder.InferredKind

sealed trait Type[T]

case class NoType[T]() extends Type[T]
case class InferredType[T](typeValueTerm: TypeValueTerm[T], argKinds: Seq[InferredKind]) extends Type[T]
case class InferringType[T](typeValueTerm: TypeValueTerm[T]) extends Type[T]
case class UninferredType[T]() extends Type[T]