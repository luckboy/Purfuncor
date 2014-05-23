/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import scala.collection.immutable.IntMap
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.kinder.NoKind
import pl.luckboy.purfuncor.frontend.kinder.InferredKind
import pl.luckboy.purfuncor.common.Unifier._
import pl.luckboy.purfuncor.util.CollectionUtils._
import pl.luckboy.purfuncor.util.StateUtils._
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
        (env, (typeValueTerm, argKinds, IntMap[Int]()).success)
      case InferringType(typeValueTerm)          =>
        st(for {
          typeValueTerm2 <- ste(instantiate(typeValueTerm))
          tuple <- {
            val (typeValueTerm3, params2) = normalizeTypeParamsWithTypeParamsForParams(typeValueTerm2, (typeParamsFromTypeValueTerm(typeValueTerm2) | params.keySet).size)(params)
            State(stMapToIntMapValidationS(params2) {
              (pair, newEnv: E) =>
                val (param, param2) = pair
                val (newEnv2, newRes) = envSt.inferTypeValueTermKindS(TypeParamApp(param, Nil, 0))(newEnv)
                newRes.map {
                  kind =>
                    val (newEnv3, newRes2) = envSt.inferredKindFromKindS(kind)(newEnv2)
                    (newEnv3, newRes2.map { param2 -> _ })
                }.valueOr { nt => (newEnv2, nt.failure) }
            }).map {
              _.flatMap { 
                kinds =>
                  val res2 = mapToVectorOption(0 until kinds.size)(kinds.get).toSuccess { 
                    NoType.fromError[T](FatalError("index of out bounds", none, NoPosition))
                  }
                  res2.map { (typeValueTerm3, _, params2) } 
              }
            }
          }
        } yield tuple).run(env)
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
        (env, (typeValueTerm, IntMap[Int]()).success)
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
    stMapToVectorValidationS(types) { _.uninstantiatedTypeValueTermS(_: E) } (env)
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
    InferredType[T](typeValueTerm, Seq.fill(n)(InferredKind(Star(KindType, NoPosition))))
  }
  
  def makearrayFunType[T](n: Int) = {
    // \t1 => t1 #-> ... #-> t1 #-> (#Array t1)
    val typeValueTerm = (0 until n).foldRight(BuiltinType(TypeBuiltinFunction.Array, Seq(TypeParamApp[T](0, Nil, 0)))) {
      (_, tvt) => BuiltinType[T](TypeBuiltinFunction.Fun, Seq(TypeParamApp(0, Nil, 0), tvt))
    }
    InferredType[T](typeValueTerm, Seq(InferredKind(Star(KindType, NoPosition))))
  }
  
  def makelistFunType[T](n: Int) = {
    // \t1 t2 => (t1 #-> t2 #-> t2) #-> t1 #-> ... #-> t1 #-> t2 #-> t2
    val tmpTypeValueTerm1 = BuiltinType(TypeBuiltinFunction.Fun, Seq(
        TypeParamApp[T](0, Nil, 0), 
        BuiltinType(TypeBuiltinFunction.Fun, Seq(TypeParamApp[T](1, Nil, 0), TypeParamApp[T](1, Nil, 0)))))
    val tmpTypeValueTerm2 = (0 until n).foldRight(BuiltinType(TypeBuiltinFunction.Fun, Seq(TypeParamApp[T](1, Nil, 0), TypeParamApp[T](1, Nil, 0)))) {
      (_, tvt) => BuiltinType[T](TypeBuiltinFunction.Fun, Seq(TypeParamApp(0, Nil, 0), tvt))
    }
    val typeValueTerm = BuiltinType(TypeBuiltinFunction.Fun, Seq(tmpTypeValueTerm1, tmpTypeValueTerm2))
    InferredType[T](typeValueTerm, Seq.fill(2)(InferredKind(Star(KindType, NoPosition))))
  }
  
  def fieldFunType[T](i: Int) = {
    // \t1 => t1 #-> ## i t1
    val typeValueTerm = BuiltinType(TypeBuiltinFunction.Fun, Seq(
        TypeParamApp[T](0, Nil, 0),
        FieldType(i, TypeParamApp[T](0, Nil, 0))))
    InferredType[T](typeValueTerm, Seq(InferredKind(Star(KindType, NoPosition))))
  }
  
  def fieldsetFunType[T](n: Int) = {
    // \t1 ... tN => t1 #-> ... #-> tN #-> (((#FieldSet1 () ()) #| (#FieldSet1 ##1 () t1) #| ... #| (#FieldSet1 ##N () tN)) #& (#FieldSet2 ##1 () t1) #& ... #& (#FieldSet2 ##N () tN))
    val tmpTypeValueTerm1 = (0 until n).foldLeft(BuiltinType(TypeBuiltinFunction.FieldSet1, Seq(TupleType(Nil), TupleType(Nil))): TypeValueTerm[T]) { 
      (tvt, p) => tvt | BuiltinType(TypeBuiltinFunction.FieldSet1, Seq(FieldType(p, TupleType(Nil)), TypeParamApp(p, Nil, 0)))
    }
    val tmpTypeValueTerm2 = (0 until n).foldLeft(tmpTypeValueTerm1) { 
      (tvt, p) => tvt & BuiltinType(TypeBuiltinFunction.FieldSet2, Seq(FieldType(p, TupleType(Nil)), TypeParamApp(p, Nil, 0)))
    }
    val typeValueTerm = (0 until n).foldRight(tmpTypeValueTerm2) {
      (p, tvt) => BuiltinType(TypeBuiltinFunction.Fun, Seq(TypeParamApp(p, Nil, 0), tvt))
    }
    InferredType[T](typeValueTerm, Seq.fill(n)(InferredKind(Star(KindType, NoPosition))))
  }
  
  def fieldSetAppFunType[T](n: Int) = {
    // \t1 ... tN u v1 ... vN => (t1 #-> ... #-> tN #-> u) #-> (((#FieldSet1 () ()) #| (#FieldSet1 v1 ##1 t1) #| ... #| (#FieldSet1 vN ##N tN)) #& (#FieldSet2 v1 ##1 t1) #& ... #& (#FieldSet2 vN ##N tN)) #-> u
    val tmpTypeValueTerm1 = (0 until n).foldRight(TypeParamApp(n, Nil, 0): TypeValueTerm[T]) {
      (p, tvt) => BuiltinType(TypeBuiltinFunction.Fun, Seq(TypeParamApp[T](p, Nil, 0), tvt))
    }
    val tmpTypeValueTerm2 = (0 until n).foldLeft(BuiltinType(TypeBuiltinFunction.FieldSet1, Seq(TupleType(Nil), TupleType(Nil))): TypeValueTerm[T]) {
      (tvt, p) => tvt | BuiltinType(TypeBuiltinFunction.FieldSet1, Seq(TypeParamApp(p + n + 1, Nil, 0), FieldType(p, TypeParamApp(p, Nil, 0))))
    }
    val tmpTypeValueTerm3 = (0 until n).foldLeft(tmpTypeValueTerm2) {
      (tvt, p) => tvt & BuiltinType(TypeBuiltinFunction.FieldSet2, Seq(TypeParamApp(p + n + 1, Nil, 0), FieldType(p, TypeParamApp(p, Nil, 0))))
    }
    val typeValueTerm = BuiltinType(TypeBuiltinFunction.Fun, Seq(
        tmpTypeValueTerm1,
        BuiltinType(TypeBuiltinFunction.Fun, Seq(
            tmpTypeValueTerm3,
            TypeParamApp[T](n, Nil, 0)))))
    InferredType(typeValueTerm, Seq.fill(n * 2 + 1)(InferredKind(Star(KindType, NoPosition))))
  }
  
  def fieldswithFunType[T](n: Int, is: List[Int]) = {
    // \t1 ... tN u1 ... uN => ti1 #-> ... #-> tiM #-> (((#FieldSet1 () ()) #| (#FieldSet1 u1 ##1 t1) #| ... #| (#FieldSet1 uN ##N tN)) #& (#FieldSet2 ui1 ##i1 ti1) #& ... #& (#FieldSet2 uiM ##iM tiM)) #-> (((#FieldSet1 () ()) #| (#FieldSet1 ##1 () ##1 t1) #| ... #| (#FieldSet1 ##N () ##N tN)) #& (#FieldSet2 ##1 () ##1 t1) #& ... #& (#FieldSet2 ##N () ##N tN))
    val is2 = (0 until n).toList.filterNot(is.contains)
    val tmpTypeValueTerm1 = (0 until n).foldLeft(BuiltinType(TypeBuiltinFunction.FieldSet1, Seq(TupleType(Nil), TupleType(Nil))): TypeValueTerm[T]) {
      (tvt, p) => tvt | BuiltinType(TypeBuiltinFunction.FieldSet1, Seq(TypeParamApp(p + n, Nil, 0), FieldType(p, TypeParamApp(p, Nil, 0))))
    }
    val tmpTypeValueTerm2 = is2.foldLeft(tmpTypeValueTerm1) {
      (tvt, p) => tvt & BuiltinType(TypeBuiltinFunction.FieldSet2, Seq(TypeParamApp(p + n, Nil, 0), FieldType(p, TypeParamApp(p, Nil, 0))))
    }
    val tmpTypeValueTerm3 = (0 until n).foldLeft(BuiltinType(TypeBuiltinFunction.FieldSet1, Seq(TupleType(Nil), TupleType(Nil))): TypeValueTerm[T]) {
      (tvt, p) => tvt | BuiltinType(TypeBuiltinFunction.FieldSet1, Seq(FieldType(p, TupleType(Nil)), FieldType(p, TypeParamApp(p, Nil, 0))))
    }
    val tmpTypeValueTerm4 = (0 until n).foldLeft(tmpTypeValueTerm3) {
      (tvt, p) => tvt & BuiltinType(TypeBuiltinFunction.FieldSet2, Seq(FieldType(p, TupleType(Nil)), FieldType(p, TypeParamApp(p, Nil, 0))))
    }
    val typeValueTerm = is.foldRight(BuiltinType(TypeBuiltinFunction.Fun, Seq(tmpTypeValueTerm2, tmpTypeValueTerm4)): TypeValueTerm[T]) {
      (p, tvt) => BuiltinType(TypeBuiltinFunction.Fun, Seq(TypeParamApp(p, Nil, 0), tvt))
    }
    InferredType(typeValueTerm, Seq.fill(n * 2)(InferredKind(Star(KindType, NoPosition))))
  }
}

case class InferringType[T](typeValueTerm: TypeValueTerm[T]) extends Type[T]
case class UninferredType[T]() extends Type[T]
