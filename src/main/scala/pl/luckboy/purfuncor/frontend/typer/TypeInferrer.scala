/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.kinder.InferredKind
import pl.luckboy.purfuncor.common.Unifier._
import pl.luckboy.purfuncor.frontend.typer.TypeValueTermUnifier._
import pl.luckboy.purfuncor.frontend.typer.TypeValueTermUtils._

object TypeInferrer
{
  private def normalizeInferredTypeValueTermS[T, U, E](term: TypeValueTerm[T], kinds: Map[Int, InferredKind])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) = {
    val (env2, res) = allocateTypeValueTermParamsWithKindsS(term, kinds)(Map(), 0)(env)
    res.map { f => normalizeTypeValueTermS(f._4)(env2) }.valueOr { nt => (env2, nt.failure) }
  }
  
  def unifyTypesS[T, U, E](type1: Type[T], type2: Type[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    (type1, type2) match {
      case (InferredType(typeValueTerm1, argKinds1), InferredType(typeValueTerm2, argKinds2)) =>
        val argKindMap1 = argKinds1.zipWithIndex.map { _.swap }.toMap
        val (env2, res1) = normalizeInferredTypeValueTermS(typeValueTerm1, argKindMap1)(env)
        val argKindMap2 = argKinds2.zipWithIndex.map { _.swap }.toMap
        val (env3, res2) = normalizeInferredTypeValueTermS(typeValueTerm2, argKindMap2)(env2)
        ((res1 |@| res2) {
          case (inferringTypeValueTerm1, inferringTypeValueTerm2) =>
            val (env4, res2) = unifyS(inferringTypeValueTerm1, inferringTypeValueTerm2)(env3)
            (env4, res2.map { InferringType(_) }.valueOr(identity))
        }).valueOr { (env3, _) }
      case (InferredType(typeValueTerm1, argKinds1), InferringType(inferringTypeValueTerm2)) =>
        val argKindMap1 = argKinds1.zipWithIndex.map { _.swap }.toMap
        val (env2, res) = normalizeInferredTypeValueTermS(typeValueTerm1, argKindMap1)(env)
        res.map {
          case inferringTypeValueTerm1 =>
            val (env3, res2) = unifyS(inferringTypeValueTerm1, inferringTypeValueTerm2)(env2)
            (env3, res2.map { InferringType(_) }.valueOr(identity))
        }.valueOr { (env2, _) }
      case (InferringType(inferringTypeValueTerm1), InferredType(typeValueTerm2, argKinds2)) =>
        val argKindMap2 = argKinds2.zipWithIndex.map { _.swap }.toMap
        val (env2, res) = normalizeInferredTypeValueTermS(typeValueTerm2, argKindMap2)(env)
        res.map {
          case inferringTypeValueTerm2 =>
            val (env3, res2) = unifyS(inferringTypeValueTerm1, inferringTypeValueTerm2)(env2)
            (env3, res2.map { InferringType(_) }.valueOr(identity))
        }.valueOr { (env2, _) }
      case (InferringType(inferringTypeValueTerm1), InferringType(inferringTypeValueTerm2)) =>
        val (env2, res) = unifyS(inferringTypeValueTerm1, inferringTypeValueTerm2)(env)
        (env2, res.map { InferringType(_) }.valueOr(identity))
      case (UninferredType(), _) | (_, UninferredType()) =>
        (env, NoType.fromError[T](FatalError("uninferred type", none, NoPosition)))
      case (noType: NoType[T], _) =>
        (env, noType)
      case (_, noType: NoType[T]) =>
        (env, noType)
    }
  
  def unifyTypes[T, U, E](type1: Type[T], type2: Type[T])(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    State(unifyTypesS[T, U, E](type1, type2))
  
  def normalizeTypeS[T, U, E](typ: Type[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    typ match {
      case InferredType(typeApp: TypeApp[T], argKinds) =>
        val argKindMap = argKinds.zipWithIndex.map { _.swap }.toMap
        val (env2, res) = normalizeInferredTypeValueTermS(typeApp, argKindMap)(env)
        (env2, res.map { InferringType(_) }.valueOr(identity))
      case _                                           =>
        (env, typ)
    }
  
  private def evaluateInferredTypeValueTermS[T, U, E](term: TypeValueTerm[T], paramCount: Int)(env: E)(implicit envSt: TypeInferenceEnvironmentState[E, U, T]) =
    term match {
      case GlobalTypeApp(loc, args, _) => envSt.appForGlobalTypeS(loc, args, paramCount, 0)(env)
      case _                           => (env, term.success)
    }
  
  private def evaluateInferringTypeValueTermS[T, U, E](term: TypeValueTerm[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) = {
    val (env2, res) = partiallyInstantiateTypeValueTermS(term) { (_: E, inifityTypeValueTermNoType) } (env)
    res.map {
      case (GlobalTypeApp(loc, args, _), _) => appForGlobalTypeWithAllocatedTypeParamsS(loc, args)(env2)
      case (term2, _)                       => (env2, term2.success)
    }.valueOr { nt => (env, nt.failure) }
  }
  
  private def argTypeValueTermFromTypeValueTermS1[T, E](term: TypeValueTerm[T], argCount: Int)(env: E)(evaluate: (TypeValueTerm[T], E) => (E, Validation[NoType[T], TypeValueTerm[T]])) = {
    val (env2, res) = (0 until argCount).foldLeft((env, (term, List[TypeValueTerm[T]]()).success[NoType[T]])) {
      case ((newEnv, Success((typeFun, newTypes))), _) =>
        val (newEnv2, newRes) = evaluate(typeFun, newEnv)
        (newEnv2, newRes.flatMap {
          case BuiltinType(TypeBuiltinFunction.Fun, Seq(arg, ret)) =>
            (ret, arg :: newTypes).success
          case _                                                   =>
            NoType.fromError[T](FatalError("type value term isn't function", none, NoPosition)).failure
        })
      case ((newEnv, Failure(noType)), _)          =>
        (newEnv, noType.failure)
    }
    (env2, res.map { _._2.reverse })
  }
    
  def argTypesFromTypeS[T, U, E](typ: Type[T], argCount: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]): (E, Validation[NoType[T], List[Type[T]]]) =
    normalizeTypeS(typ)(env) match {
      case (env2, InferredType(typeValueTerm, argKinds)) =>
        val (env3, res) = argTypeValueTermFromTypeValueTermS1(typeValueTerm, argCount)(env2)(evaluateInferredTypeValueTermS(_, argKinds.size)(_))
        (env3, res.map { _.map { InferredType(_, argKinds) } })
      case (env2, InferringType(typeValueTerm)) =>
        val (env3, res) = argTypeValueTermFromTypeValueTermS1(typeValueTerm, argCount)(env2)(evaluateInferringTypeValueTermS(_)(_))
        (env3, res.map { _.map { InferringType(_) } })
      case (env2, UninferredType()) =>
        (env2, NoType.fromError[T](FatalError("uninferred type", none, NoPosition)).failure)
      case (env2, noType: NoType[T]) =>
        (env2, noType.failure)
    }
  
  private def returnTypeValueTermFromTypeValueTermS1[T, E](term: TypeValueTerm[T], argCount: Int)(env: E)(evaluate: (TypeValueTerm[T], E) => (E, Validation[NoType[T], TypeValueTerm[T]])) =
    (0 until argCount).foldLeft((env, term.success[NoType[T]])) {
      case ((newEnv, Success(typeFun)), _)    =>
        val (newEnv2, newRes) = evaluate(typeFun, newEnv)
        (newEnv2, newRes.flatMap {
          case BuiltinType(TypeBuiltinFunction.Fun, Seq(_, ret)) =>
            ret.success
          case _                                                 =>
            NoType.fromError[T](FatalError("type value term isn't function", none, NoPosition)).failure
        })
      case ((newEnv, Failure(noType)), _) => 
        (newEnv, noType.failure)
    }
  
  def returnTypeFromTypeS[T, U, E](typ: Type[T], argCount: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    normalizeTypeS(typ)(env) match {
      case (env2, InferredType(typeValueTerm, argKinds)) =>
        val (env3, res) = returnTypeValueTermFromTypeValueTermS1(typeValueTerm, argCount)(env2)(evaluateInferredTypeValueTermS(_, argKinds.size)(_))
        (env3, res.map { InferredType(_, argKinds) }.valueOr(identity))
      case (env2, InferringType(typeValueTerm)) =>
        val (env3, res) = returnTypeValueTermFromTypeValueTermS1(typeValueTerm, argCount)(env2)(evaluateInferringTypeValueTermS(_)(_))
        (env3, res.map { InferringType(_) }.valueOr(identity))
      case (env2, UninferredType()) =>
        (env2, NoType.fromError[T](FatalError("uninferred type", none, NoPosition)))
      case (env2, noType: NoType[T]) =>
        (env2, noType)
    }
  
  def functionType[T](argCount: Int) = {
    val typeValueTerm = (0 until argCount).foldRight(TypeParamApp(argCount, Nil, 0): TypeValueTerm[T]) {
      (p, t) => BuiltinType(TypeBuiltinFunction.Fun, Seq(TypeParamApp(p, Nil, 0), t))
    }
    val argKinds = (0 to argCount).map { _ => InferredKind(Star(KindType, NoPosition)) }
    InferredType(typeValueTerm, argKinds)
  }
  
  private def noTypeFromType[T](typ: Type[T]) =
    typ match {
      case noType: NoType[T] => noType
      case _                 => NoType.fromError[T](FatalError("uninferred type", none, NoPosition))
    }
  
  def functionTypeFromTypesS[T, U, E](argTypes: Seq[Type[T]], retType: Type[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) = {
    val (env3, retTypeKindRes) = retType match {
      case noType: NoType[T]            =>
        (env, noType.failure)
      case InferredType(retTypeValueTerm, retArgKinds) =>
        val retArgKindMap = retArgKinds.zipWithIndex.map { _.swap }.toMap
        val (env2, retRes) = normalizeInferredTypeValueTermS(retTypeValueTerm, retArgKindMap)(env)
        retRes.map { envSt.inferTypeValueTermKindS(_)(env2) }.valueOr { nt => (env2, nt.failure) }
      case InferringType(inferringRetTypeValueTerm)    =>
        envSt.inferTypeValueTermKindS(inferringRetTypeValueTerm)(env)
      case UninferredType()                            =>
        (env, NoType.fromError[T](FatalError("uninferred type", none, NoPosition)).failure)
    }
    argTypes.foldRight((env3, retTypeKindRes.map { (retType, _) })) {
      case (argType, (newEnv, Success((newRetType, newRetTypeKind)))) =>
        (argType, newRetType) match {
          case (InferredType(argTypeValueTerm, argArgKinds), InferredType(retTypeValueTerm, retArgKinds)) =>
            val argArgKindMap = argArgKinds.zipWithIndex.map { _.swap }.toMap
            val (newEnv2, argRes) = normalizeInferredTypeValueTermS(argTypeValueTerm, argArgKindMap)(newEnv)
            val retArgKindMap = retArgKinds.zipWithIndex.map { _.swap }.toMap
            val (newEnv3, retRes) = normalizeInferredTypeValueTermS(retTypeValueTerm, retArgKindMap)(newEnv2)
            (argRes |@| retRes) {
              (argInferringTypeValueTerm, retInferringTypeValueTerm) =>
                val (newEnv4, newRes) = envSt.inferTypeValueTermKindS(argInferringTypeValueTerm)(newEnv3)
                val (newEnv5, newRes2) = envSt.appStarKindS(Seq(newRes.valueOr { _.toNoKind }, newRetTypeKind))(newEnv4)
                newRes2.map {
                  k => (newEnv5, (InferringType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argInferringTypeValueTerm, retInferringTypeValueTerm))), k).success)
                }.valueOr { nt => (newEnv5, nt.failure) }
            }.valueOr { nt => (newEnv3, nt.failure) }
          case (InferredType(argTypeValueTerm, argArgKinds), InferringType(retInferringTypeValueTerm))    =>
            val argArgKindMap = argArgKinds.zipWithIndex.map { _.swap }.toMap
            val (newEnv2, res) = normalizeInferredTypeValueTermS(argTypeValueTerm, argArgKindMap)(newEnv)
            res.map {
              argInferringTypeValueTerm =>
                val (newEnv3, newRes) = envSt.inferTypeValueTermKindS(argInferringTypeValueTerm)(newEnv2)
                val (newEnv4, newRes2) = envSt.appStarKindS(Seq(newRes.valueOr { _.toNoKind }, newRetTypeKind))(newEnv3)
                newRes2.map {
                  k => (newEnv4, (InferringType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argInferringTypeValueTerm, retInferringTypeValueTerm))), k).success)
                }.valueOr { nt => (newEnv4, nt.failure) }
              }.valueOr { nt => (newEnv2, nt.failure) }
          case (InferringType(argInferringTypeValueTerm), InferredType(retTypeValueTerm, retArgKinds))    =>
            val retArgKindMap = retArgKinds.zipWithIndex.map { _.swap }.toMap
            val (newEnv2, res) = normalizeInferredTypeValueTermS(retTypeValueTerm, retArgKindMap)(newEnv)
            res.map {
              retInferringTypeValueTerm =>
              	val (newEnv3, newRes) = envSt.inferTypeValueTermKindS(argInferringTypeValueTerm)(newEnv2)
                val (newEnv4, newRes2) = envSt.appStarKindS(Seq(newRes.valueOr { _.toNoKind }, newRetTypeKind))(newEnv3)
                newRes2.map {
                  k => (newEnv4, (InferringType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argInferringTypeValueTerm, retInferringTypeValueTerm))), k).success)
                }.valueOr { nt => (newEnv4, nt.failure) }
            }.valueOr { nt => (newEnv2, nt.failure) }
          case (InferringType(argInferringTypeValueTerm), InferringType(retInferringTypeValueTerm))       =>
            val (newEnv2, newRes) = envSt.inferTypeValueTermKindS(argInferringTypeValueTerm)(newEnv)
            val (newEnv3, newRes2) = envSt.appStarKindS(Seq(newRes.valueOr { _.toNoKind }, newRetTypeKind))(newEnv2)
            newRes2.map {
              k => (newEnv3, (InferringType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argInferringTypeValueTerm, retInferringTypeValueTerm))), k).success)
            }.valueOr { nt => (newEnv3, nt.failure) }
          case ((type1 @ (UninferredType() | _: NoType[T])), (type2 @ (UninferredType() | _: NoType[T]))) =>
            (newEnv, (noTypeFromType(type1) |+| noTypeFromType(type2)).failure)
          case ((typ @ (UninferredType() | _: NoType[T])), _)                                             =>
            (newEnv, noTypeFromType(typ).failure)
          case (_, (typ @ (UninferredType() | _: NoType[T])))                                             =>
            (newEnv, noTypeFromType(typ).failure)
        }
      case (_, (newEnv, Failure(noType))) =>
        (newEnv, noType.failure)
    }.mapElements(identity, _.map { _._1 }.valueOr(identity))
  }
  
  def instantiateTypeMapWithTypeParamsS[T, U, V, E](types: Map[T, Type[U]])(env: E)(implicit unifier: Unifier[NoType[U], TypeValueTerm[U], E, Int], envSt: TypeInferenceEnvironmentState[E, V, U]) =
    types.foldLeft((env, Map[T, (Type[U], Map[Int, Int])]().success[NoType[U]])) {
      case ((newEnv, Success(newPairs)), (loc, typ)) =>
        typ.instantiatedTypeWithTypeParamsS(newEnv).mapElements(identity, _.map { p => newPairs + (loc -> p) })
      case ((newEnv, Failure(noType)), _)            =>
        (newEnv, noType.failure)
    }
  
  def instantiateTypeMapS[T, U, V, E](types: Map[T, Type[U]])(env: E)(implicit unifier: Unifier[NoType[U], TypeValueTerm[U], E, Int], envSt: TypeInferenceEnvironmentState[E, V, U]) =
    instantiateTypeMapWithTypeParamsS(types)(env).mapElements(identity, _.map { _.mapValues { _._1 } })
  
  def instantiateTypesS[T, U, E](types: Seq[Type[T]])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    types.foldLeft((env, Seq[Type[T]]().success[NoType[T]])) {
      case ((newEnv, Success(newTypes)), typ) =>
        typ.instantiatedTypeS(newEnv)  match {
          case (newEnv2, noType: NoType[T]) => (newEnv2, noType.failure)
          case (newEnv2, type2)             => (newEnv2, (newTypes :+ type2).success)
        }
      case ((newEnv, Failure(noType)), _)     =>
        (newEnv, noType.failure)
    }
  
  def instantiateTypeOptionForParamsS[T, U, E](optType: Option[Type[T]])(params: Map[Int, Int])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    optType.map {
      typ => 
        typ.instantiatedTypeForParamsS(params)(env) match {
          case (env2, noType: NoType[T]) => (env2, noType.failure)
          case (env2, type2)             => (env2, some(type2).success)
        }
    }.getOrElse((env, none.success))
    
  def instantiateTypeOptionS[T, U, E](optType: Option[Type[T]])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    instantiateTypeOptionForParamsS(optType)(Map())(env)
}
