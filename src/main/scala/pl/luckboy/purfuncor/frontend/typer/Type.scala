package pl.luckboy.purfuncor.frontend.typer
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
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

  def uninstantiatedTypeValueTermS[E](env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T]): (E, Validation[NoType[T], TypeValueTerm[T]]) =
    throw new UnsupportedOperationException
    
  def uninstantiatedTypeS[E](env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T]): (E, Type[T]) =
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

object Type
{
  def uninstantiatedTypeValueTermFromTypesS[T, E](types: Seq[Type[T]])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T]) =
    types.foldLeft((env, Seq[TypeValueTerm[T]]().success[NoType[T]])) {
      case ((newEnv, Success(tvts)), t) => t.uninstantiatedTypeValueTermS(newEnv).mapElements(identity, _.map { tvts :+ _ })
      case ((newEnv, Failure(nt)), _)   => (newEnv, nt.failure)
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

object InferredType
{
  def fromBuiltinFunction[T](bf: BuiltinFunction.Value)(implicit builtinFunTypes: BuiltinFunTypes[T]) =
    builtinFunTypes.builtinFunTypes.get(bf).getOrElse(NoType.fromError[T](FatalError("unsupported built-in function", none, NoPosition)))
  
  def booleanType[T] = InferredType[T](BuiltinType(TypeBuiltinFunction.Boolean, Nil), Nil)

  def charType[T] = InferredType[T](BuiltinType(TypeBuiltinFunction.Char, Nil), Nil)
  
  def fromByte[T](x: Byte) = {
    val bf = if(x === 0) TypeBuiltinFunction.Zero else TypeBuiltinFunction.NonZero
    InferredType[T](BuiltinType(bf, Nil) & BuiltinType(TypeBuiltinFunction.Byte, Nil), Nil)
  }

  def fromShort[T](x: Short) = {
    val bf = if(x === 0) TypeBuiltinFunction.Zero else TypeBuiltinFunction.NonZero
    InferredType[T](BuiltinType(bf, Nil) & BuiltinType(TypeBuiltinFunction.Short, Nil), Nil)
  }
  
  def fromInt[T](x: Int) = {
    val bf = if(x === 0) TypeBuiltinFunction.Zero else TypeBuiltinFunction.NonZero
    InferredType[T](BuiltinType(bf, Nil) & BuiltinType(TypeBuiltinFunction.Int, Nil), Nil)
  }
  
  def fromLong[T](x: Long) = {
    val bf = if(x === 0L) TypeBuiltinFunction.Zero else TypeBuiltinFunction.NonZero
    InferredType[T](BuiltinType(bf, Nil) & BuiltinType(TypeBuiltinFunction.Long, Nil), Nil)
  }
  
  def floatType[T] = InferredType[T](BuiltinType(TypeBuiltinFunction.Float, Nil), Nil)

  def doubleType[T] = InferredType[T](BuiltinType(TypeBuiltinFunction.Double, Nil), Nil)
  
  def tupleFunType[T](n: Int) = {
    val typeValueTerm = (0 until n).foldRight(TupleType((0 until n).map { TypeParamApp[T](_, Nil, 0) }): TypeValueTerm[T]) {
      (p, tvt) => BuiltinType[T](TypeBuiltinFunction.Fun, Seq(TypeParamApp(p, Nil, 0), tvt))
    }
    InferredType[T](typeValueTerm, Seq.fill(n)(InferredKind(Star(KindType, NoPosition))))
  }
  
  def tupleFieldFunType[T](i: Int) = {
    val typeValueTerm = BuiltinType[T](
        TypeBuiltinFunction.Fun,
        Seq(TupleType((0 to i).map { TypeParamApp[T](_, Nil, 0) }), TypeParamApp(i, Nil, 0)))
    InferredType[T](typeValueTerm, Seq.fill(i + 1)(InferredKind(Star(KindType, NoPosition))))
  }
}

case class InferringType[T](typeValueTerm: TypeValueTerm[T]) extends Type[T]
case class UninferredType[T]() extends Type[T]