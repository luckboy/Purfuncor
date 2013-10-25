package pl.luckboy.purfuncor.frontend.typer
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.kinder.NoKind
import pl.luckboy.purfuncor.frontend.kinder.InferredKind

sealed trait Type[T]
{
  def isNoType = isInstanceOf[NoType[T]]

  def isInferringType = isInstanceOf[InferringType[T]]
  
  def isUninferredType = isInstanceOf[UninferredType[T]]
  
  def instantiatedTypeValueTermS[E](env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int]): (E, Validation[NoType[T], TypeValueTerm[T]]) =
    throw new UnsupportedOperationException
    
  def instantiatedTypeS[E](env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int]): (E, Type[T]) =
    throw new UnsupportedOperationException
  
  def withPos(pos: Position) =
    this match {
      case noType: NoType[T] =>
        noType.copy[T](prevErrs = noType.prevErrs ++ noType.currentErrs.map { _.withPos(pos) }, currentErrs = Nil)
      case _                 =>
        this
    }
  
  override def toString =
    this match {
      case noType: NoType[T]                     =>
        "<no type>\n" + noType.errs.map { (" " * 8) + _ }.mkString("\n") + "\n"
      case InferredType(typeValueTerm, argKinds) =>
        if(!argKinds.isEmpty)
          "\\" + argKinds.zipWithIndex.map { case (k, i) => "(t" + i + ": " + k + ")" }.mkString(" ") + " => " + typeValueTerm
        else
          typeValueTerm.toString
      case InferringType(typeValueTerm)          =>
        "<inferring type: " + typeValueTerm + ">"
      case UninferredType()                      =>
        "<uninferred type>"
    }
}

case class NoType[T](prevErrs: List[AbstractError], currentErrs: List[AbstractError]) extends Type[T]
{
  def errs = prevErrs ++ currentErrs
  
  def toNoKind = NoKind(prevErrs, currentErrs)
}

object NoType
{
  def fromError[T](err: AbstractError) = NoType[T](Nil, List(err))
    
  def fromErrors[T](errs: NonEmptyList[AbstractError]) = NoType[T](Nil, errs.list)
    
  def fromNoKind[T](noKind: NoKind) = NoType[T](noKind.prevErrs, noKind.currentErrs)
    
  def fromNoTypeValue[T, U, V, W](noTypeValue: NoTypeValue[T, U, V, W]) = NoType.fromError[T](noTypeValue.err)
}

case class InferredType[T](typeValueTerm: TypeValueTerm[T], argKinds: Seq[InferredKind]) extends Type[T]
case class InferringType[T](typeValueTerm: TypeValueTerm[T]) extends Type[T]
case class UninferredType[T]() extends Type[T]