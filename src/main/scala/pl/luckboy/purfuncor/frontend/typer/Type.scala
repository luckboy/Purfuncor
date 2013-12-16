package pl.luckboy.purfuncor.frontend.typer
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.kinder.NoKind
import pl.luckboy.purfuncor.frontend.kinder.InferredKind
import pl.luckboy.purfuncor.common.Unifier._
import TypeValueTermUnifier._
import TypeValueTermUtils._

sealed trait Type[T]
{
  def isNoType = isInstanceOf[NoType[T]]

  def isInferringType = isInstanceOf[InferringType[T]]
  
  def isUninferredType = isInstanceOf[UninferredType[T]]
  
  def instantiatedTypeValueTermWithKindsAndTypeParamsForParamsS[U, E](params: Map[Int, Int])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    this match {
      case noType: NoType[T]                     =>
        (env, noType.failure)
      case InferredType(typeValueTerm, argKinds) =>
        (env, (typeValueTerm, argKinds, Map[Int, Int]()).success)
      case InferringType(typeValueTerm)          =>
        (for {
          res <- instantiate(typeValueTerm)
          res3 <- res.map {
            typeValueTerm2 =>
              val (typeValueTerm3, params2) = normalizeTypeParamsWithTypeParamsForParams(typeValueTerm2, (typeParamsFromTypeValueTerm(typeValueTerm2) | params.keySet).size)(params)
              State({
                (env2: E) =>
                  params2.foldLeft((env2, Map[Int, InferredKind]().success[NoType[T]])) {
                    case ((newEnv, Success(newKinds)), (param, param2)) => 
                      val (newEnv2, newRes) = envSt.inferTypeValueTermKindS(TypeParamApp(param, Nil, 0))(newEnv)
                      newRes.map {
                        kind =>
                          val (newEnv3, newRes2) = envSt.inferredKindFromKindS(kind)(newEnv2)
                          (newEnv3, newRes2.map { k => newKinds + (param2 -> k) })
                      }.valueOr { nt => (newEnv2, nt.failure) }
                  }
              }).map {
                _.flatMap { 
                  kinds =>
                    val res2 = (0 until kinds.size).foldLeft(some(Seq[InferredKind]())) { 
                      (optKs, i) => optKs.flatMap { ks => kinds.get(i).map { ks :+ _ } }
                    }.toSuccess(NoType.fromError[T](FatalError("index of out bounds", none, NoPosition)))
                    res2.map { (typeValueTerm3, _, params2) } 
                }
             }
          }.valueOr { nt => State((_: E, nt.failure)) }
        } yield res3).run(env)
      case UninferredType()                      =>
        (env, NoType.fromError[T](FatalError("uninferred type", none, NoPosition)).failure)
    }
  
  def instantiatedTypeValueTermWithKindsS[U, E](env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    instantiatedTypeValueTermWithKindsAndTypeParamsForParamsS(Map())(env).mapElements(identity, _.map { t => (t._1, t._2) })
  
  def instantiatedTypeS[U, E](env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]): (E, Type[T]) =
    instantiatedTypeValueTermWithKindsS(env).mapElements(identity, _.map { case (tvt, ks) => InferredType(tvt, ks) }.valueOr(identity))
  
  def instantiatedTypeWithTypeParamsS[U, E](env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]): (E, Validation[NoType[T], (Type[T], Map[Int, Int])]) =
    instantiatedTypeValueTermWithKindsAndTypeParamsForParamsS(Map())(env).mapElements(identity, _.map { case (tvt, ks, tps) => (InferredType(tvt, ks), tps) })

  def instantiatedTypeForParamsS[U, E](params: Map[Int, Int])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]): (E, Type[T]) =
    instantiatedTypeValueTermWithKindsAndTypeParamsForParamsS(params)(env).mapElements(identity, _.map { case (tvt, ks, _) => InferredType(tvt, ks) }.valueOr(identity))
    
  def uninstantiatedTypeValueTermWithTypeParamsS[U, E](env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    this match {
      case noType: NoType[T]                     =>
        (env, noType.failure)
      case InferredType(typeValueTerm, argKinds) =>
        val (env2, res) = allocateTypeValueTermParamsWithKindsS(typeValueTerm, argKinds.zipWithIndex.map { _.swap }.toMap)(Map(), 0)(env)
        (env2, res.map { f => (f._4, f._1) })
      case InferringType(typeValueTerm)          =>
        (env, (typeValueTerm, Map[Int, Int]()).success)
      case UninferredType()                      =>
        (env, NoType.fromError[T](FatalError("uninferred type", none, NoPosition)).failure)
    }

  def uninstantiatedTypeValueTermS[U, E](env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    uninstantiatedTypeValueTermWithTypeParamsS(env).mapElements(identity, _.map { _._1 })
  
  def uninstantiatedTypeS[U, E](env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    uninstantiatedTypeValueTermS(env).mapElements(identity, _.map { InferringType(_) }.valueOr(identity))
  
  def withPos(pos: Position) = this
  
  override def toString =
    this match {
      case noType: NoType[T]                     =>
        "<no type>\n" + noType.errs.map { (" " * 8) + _ }.mkString("\n") + "\n"
      case InferredType(typeValueTerm, argKinds) =>
        if(!argKinds.isEmpty)
          "\\" + argKinds.zipWithIndex.map { case (k, i) => "(t" + (i + 1) + ": " + k + ")" }.mkString(" ") + " => " + typeValueTerm
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
  def uninstantiatedTypeValueTermFromTypesS[T, U, E](types: Seq[Type[T]])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    types.foldLeft((env, Seq[TypeValueTerm[T]]().success[NoType[T]])) {
      case ((newEnv, Success(tvts)), t) => t.uninstantiatedTypeValueTermS(newEnv).mapElements(identity, _.map { tvts :+ _ })
      case ((newEnv, Failure(nt)), _)   => (newEnv, nt.failure)
    }
}

case class NoType[T](prevErrs: List[AbstractError], currentErrs: List[AbstractError]) extends Type[T]
{
  def errs = prevErrs ++ currentErrs
  
  def toNoKind = NoKind(prevErrs, currentErrs)
  
  def forFile(file: Option[java.io.File]) = copy[T](prevErrs = prevErrs.map { _.withFile(file) }, currentErrs = currentErrs.map { _.withFile(file) })

  override def withPos(pos: Position) = copy[T](prevErrs = prevErrs ++ currentErrs.map { _.withPos(pos) }, currentErrs = Nil)
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
  
  def tupleFieldFunType[T](n: Int, i: Int) = {
    val typeValueTerm = BuiltinType[T](
        TypeBuiltinFunction.Fun,
        Seq(TupleType((0 until n).map { TypeParamApp[T](_, Nil, 0) }), TypeParamApp(i, Nil, 0)))
    InferredType[T](typeValueTerm, Seq.fill(i + 1)(InferredKind(Star(KindType, NoPosition))))
  }
}

case class InferringType[T](typeValueTerm: TypeValueTerm[T]) extends Type[T]
case class UninferredType[T]() extends Type[T]