/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer
import scala.collection.immutable.IntMap
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

object TypeValueTermUtils
{
  def typeParamsFromTypeValueTerm[T](term: TypeValueTerm[T]): Set[Int] =
    term match {
      case TupleType(args)              => args.flatMap(typeParamsFromTypeValueTerm).toSet
      case FieldType(_, term2)          => typeParamsFromTypeValueTerm(term2)
      case BuiltinType(_, args)         => args.flatMap(typeParamsFromTypeValueTerm).toSet
      case Unittype(_, args, _)         => args.flatMap { a => typeParamsFromTypeValueTerm(a.body) -- a.argParams }.toSet
      case GlobalTypeApp(_, args, _)    => args.flatMap { a => typeParamsFromTypeValueTerm(a.body) -- a.argParams }.toSet
      case TypeParamApp(param, args, _) => Set(param) | args.flatMap { a => typeParamsFromTypeValueTerm(a.body) -- a.argParams }.toSet
      case TypeConjunction(terms)       => terms.flatMap(typeParamsFromTypeValueTerm).toSet
      case TypeDisjunction(terms)       => terms.flatMap(typeParamsFromTypeValueTerm).toSet
    }
  
  def typeArgParamsFromTypeValueTerm[T](term: TypeValueTerm[T]): Set[Int] =
    term match {
      case TupleType(args)              => args.flatMap(typeArgParamsFromTypeValueTerm).toSet
      case FieldType(_, term2)          => typeArgParamsFromTypeValueTerm(term2)
      case BuiltinType(_, args)         => args.flatMap(typeArgParamsFromTypeValueTerm).toSet
      case Unittype(_, args, _)         => args.flatMap { _.argParams }.toSet
      case GlobalTypeApp(_, args, _)    => args.flatMap { _.argParams }.toSet
      case TypeParamApp(param, args, _) => args.flatMap { _.argParams }.toSet
      case TypeConjunction(terms)       => terms.flatMap(typeArgParamsFromTypeValueTerm).toSet
      case TypeDisjunction(terms)       => terms.flatMap(typeArgParamsFromTypeValueTerm).toSet
    }
  
  private def substituteTypeValueLambdasInTypeValueTerms[T](terms: Seq[TypeValueTerm[T]], paramLambdas: Map[Int, TypeValueLambda[T]]) =
    terms.foldLeft(some(Vector[TypeValueTerm[T]]())) {
      (o, t) => for(ts <- o; t2 <- substituteTypeValueLambdasInTypeValueTerm(t, paramLambdas)) yield (ts :+ t2)
    }

  private def substituteTypeValueLambdasInTypeValueLambdas[T](lambdas: Seq[TypeValueLambda[T]], paramLambdas: Map[Int, TypeValueLambda[T]]) =
    lambdas.foldLeft(some(Vector[TypeValueLambda[T]]())) {
      (o, l) => for(ls <- o; l2 <- substituteTypeValueLambdasInTypeValueLambda(l, paramLambdas -- l.argParams)) yield (ls :+ l2)
    }

  private def substituteTypeValueLambdasInTypeValueLambda[T](lambda: TypeValueLambda[T], paramLambdas: Map[Int, TypeValueLambda[T]]): Option[TypeValueLambda[T]] =
    lambda match {
      case TypeValueLambda(argParams, typeParamApp @ TypeParamApp(param, args, paramAppIdx)) =>
        substituteTypeValueLambdasInTypeValueLambdas(args, paramLambdas).flatMap {
          args2 =>
            paramLambdas.get(param).map {
              case TypeValueLambda(argParams2, body2) =>
                val argParams3 = argParams ++ argParams2.drop(args2.size)
                val paramLambdas3 = argParams2.take(args2.size).zip(args2).toMap
                val lambda3 = TypeValueLambda(Nil, body2)
                substituteTypeValueLambdasInTypeValueLambda(lambda3, paramLambdas3).map {
                  case TypeValueLambda(_, body3) => TypeValueLambda(argParams3, body3)
                }
            }.getOrElse(some(TypeValueLambda(argParams, TypeParamApp(param, args2, paramAppIdx))))
        }
      case TypeValueLambda(argParams, body) =>
        substituteTypeValueLambdasInTypeValueTerm(body, paramLambdas -- argParams).map { TypeValueLambda(argParams, _) }
    }
  
  private def substituteTypeValueLambdasInTypeValueTerm[T](term: TypeValueTerm[T], paramLambdas: Map[Int, TypeValueLambda[T]]): Option[TypeValueTerm[T]] =
    term match {
      case TupleType(args)               => 
        substituteTypeValueLambdasInTypeValueTerms(args, paramLambdas).map { TupleType(_) }
      case FieldType(i, term2)           =>
        substituteTypeValueLambdasInTypeValueTerm(term2, paramLambdas).map { FieldType(i, _) }
      case BuiltinType(bf, args)         =>
        substituteTypeValueLambdasInTypeValueTerms(args, paramLambdas).map { BuiltinType(bf, _) }
      case Unittype(loc, args, sym)      =>
        substituteTypeValueLambdasInTypeValueLambdas(args, paramLambdas).map { Unittype(loc, _, sym) }
      case GlobalTypeApp(loc, args, sym) =>
        substituteTypeValueLambdasInTypeValueLambdas(args, paramLambdas).map { GlobalTypeApp(loc, _, sym) }
      case typeParamApp: TypeParamApp[T] =>
        substituteTypeValueLambdasInTypeValueLambda(TypeValueLambda(Nil, typeParamApp), paramLambdas).flatMap {
          case TypeValueLambda(Seq(), body) => some(body)
          case _                            => none
        }
      case TypeConjunction(terms)        =>
        substituteTypeValueLambdasInTypeValueTerms(terms.toSeq, paramLambdas).map { ts => TypeValueTerm.normalizedTypeConjunction(ts.toSet) }
      case TypeDisjunction(terms)        =>
        substituteTypeValueLambdasInTypeValueTerms(terms.toSeq, paramLambdas).map { ts => TypeValueTerm.normalizedTypeDisjunction(ts.toSet) }
    }
  
  def substituteTypeValueLambdas[T](term: TypeValueTerm[T], paramLambdas: Map[Int, TypeValueLambda[T]], nextArgParam: Int) = {
    val params = (
        typeParamsFromTypeValueTerm(term) | 
        typeArgParamsFromTypeValueTerm(term) |
        paramLambdas.keySet |
        paramLambdas.values.flatMap { l => typeParamsFromTypeValueTerm(l.body) }.toSet)
    val params2 = paramLambdas.values.flatMap { l => typeArgParamsFromTypeValueTerm(l.body) ++ l.argParams }.toSet
    val nextArgParam2 = (params | params2).maximum.map(1 +).getOrElse(nextArgParam)
    val paramLambdas2 = paramLambdas.foldLeft((IntMap[TypeValueLambda[T]](), nextArgParam2)) {
      case ((newParamLambdas, newNextArgParam), (param, lambda @ TypeValueLambda(argParams, body))) =>
        val lambdaArgParams = typeArgParamsFromTypeValueTerm(body) ++ argParams
        if((params & lambdaArgParams).isEmpty) {
          (newParamLambdas + (param -> lambda), newNextArgParam)
        } else {
          val bodyParams = typeParamsFromTypeValueTerm(body)
          val (_, lambda2) = normalizeTypeParamsInTypeValueLambdaForParamsS(lambda, newNextArgParam)((lambdaArgParams &~ params).map { p => (p -> p) }.toMap)((bodyParams.map { p => p -> p }.toMap, bodyParams.size))
          lambda2 match {
            case TypeValueLambda(argParams2, body2) =>
              val lambdaArgParams2 = typeArgParamsFromTypeValueTerm(body2) ++ argParams2
              (newParamLambdas + (param -> lambda2), lambdaArgParams2.maximum.map(1 +).getOrElse(newNextArgParam))
          }
        }
    }._1
    substituteTypeValueLambdasInTypeValueTerm(term, paramLambdas2).map {
      term2 =>
        normalizeTypeParamsForParams(term2, nextArgParam)(typeParamsFromTypeValueTerm(term2).map { p => p -> p }.toMap)
    }
  }
    
  private def normalizeTypeParamsInTypeValueTermsForParamsS[T](terms: Seq[TypeValueTerm[T]], nextArgParam: Int)(lambdaParams: Map[Int, Int])(pair: (Map[Int, Int], Int)) =
    terms.foldLeft((pair, Vector[TypeValueTerm[T]]())) {
      case ((p, ts), t) => normalizeTypeParamsInTypeValyeTermForParamsS(t, nextArgParam)(lambdaParams)(p).mapElements(identity, ts :+ _)
    }
  
  private def normalizeTypeParamsInTypeValueLambdasForParamsS[T](lambdas: Seq[TypeValueLambda[T]], nextArgParam: Int)(lambdaParams: Map[Int, Int])(pair: (Map[Int, Int], Int)) =
    lambdas.foldLeft((pair, Vector[TypeValueLambda[T]]())) {
      case ((p, ls), l) => normalizeTypeParamsInTypeValueLambdaForParamsS(l, nextArgParam)(lambdaParams)(p).mapElements(identity, ls :+ _)
    }
  
  private def normalizeTypeParamsInTypeValueLambdaForParamsS[T](lambda: TypeValueLambda[T], nextArgParam: Int)(lambdaParams: Map[Int, Int])(pair: (Map[Int, Int], Int)) =
    lambda match {
      case TypeValueLambda(argParams, body) =>
        val argParams2 = nextArgParam until (nextArgParam + argParams.size)
        val lambdaParams2 = IntMap() ++ (lambdaParams ++ argParams.zipWithIndex.map { p => (p._1, nextArgParam + p._2) })
        val (pair2, body2) = normalizeTypeParamsInTypeValyeTermForParamsS(body, nextArgParam + argParams.size)(lambdaParams2)(pair)
        (pair2, TypeValueLambda(argParams2, body2))
    }
  
  private def normalizeTypeParamsInTypeValyeTermForParamsS[T](term: TypeValueTerm[T], nextArgParam: Int)(lambdaParams: Map[Int, Int])(pair: (Map[Int, Int], Int)): ((Map[Int, Int], Int), TypeValueTerm[T]) =
    term match {
      case TupleType(args)                        => 
        val (pair2, args2) = normalizeTypeParamsInTypeValueTermsForParamsS(args, nextArgParam)(lambdaParams)(pair)
        (pair2, TupleType(args2))
      case FieldType(i, term2)                    =>
        val (pair2, term3) = normalizeTypeParamsInTypeValyeTermForParamsS(term2, nextArgParam)(lambdaParams)(pair)
        (pair2, FieldType(i, term3))
      case BuiltinType(bf, args)                  =>
        val (pair2, args2) = normalizeTypeParamsInTypeValueTermsForParamsS(args, nextArgParam)(lambdaParams)(pair)
        (pair2, BuiltinType(bf, args2))
      case Unittype(loc, args, sym)               =>
        val (pair2, args2) = normalizeTypeParamsInTypeValueLambdasForParamsS(args, nextArgParam)(lambdaParams)(pair)
        (pair2, Unittype(loc, args2, sym))
      case GlobalTypeApp(loc, args, sym)          =>
        val (pair2, args2) = normalizeTypeParamsInTypeValueLambdasForParamsS(args, nextArgParam)(lambdaParams)(pair)
        (pair2, GlobalTypeApp(loc, args2, sym))
      case TypeParamApp(param, args, _) =>
        val (termParams, nextTermParam) = pair
        val (termParams2, nextTermParam2) = if(termParams.contains(param) || lambdaParams.contains(param)) 
          (termParams, nextTermParam)
        else
          (termParams + (param -> nextTermParam), nextTermParam + 1)
        val param2 = lambdaParams.getOrElse(param, termParams.getOrElse(param, nextTermParam))
        val ((termParams3, nextTermParam3), args2) = normalizeTypeParamsInTypeValueLambdasForParamsS(args, nextArgParam)(lambdaParams)((termParams2, nextTermParam2))
        ((termParams3, nextTermParam3), TypeParamApp(param2, args2, 0))
      case TypeConjunction(terms)                 =>
        val (pair2, terms2) = normalizeTypeParamsInTypeValueTermsForParamsS(terms.toSeq, nextArgParam)(lambdaParams)(pair)
        (pair2, TypeValueTerm.normalizedTypeConjunction(terms2.toSet))
      case TypeDisjunction(terms)                 =>
        val (pair2, terms2) = normalizeTypeParamsInTypeValueTermsForParamsS(terms.toSeq, nextArgParam)(lambdaParams)(pair)
        (pair2, TypeValueTerm.normalizedTypeDisjunction(terms2.toSet))
    }
  
  def normalizeTypeParams[T](term: TypeValueTerm[T], nextArgParam: Int) =
    normalizeTypeParamsInTypeValyeTermForParamsS(term, nextArgParam)(IntMap())((IntMap(), 0))._2

  def normalizeTypeParamsWithTypeParams[T](term: TypeValueTerm[T], nextArgParam: Int) =
    normalizeTypeParamsInTypeValyeTermForParamsS(term, nextArgParam)(IntMap())((IntMap(), 0)).swap
    
  def normalizeTypeParamsForParams[T](term: TypeValueTerm[T], nextArgParam: Int)(params: Map[Int, Int]) =
    normalizeTypeParamsInTypeValyeTermForParamsS(term, nextArgParam)(IntMap())((params, params.values.toSet.size))._2
    
  def normalizeTypeParamsWithTypeParamsForParams[T](term: TypeValueTerm[T], nextArgParam: Int)(params: Map[Int, Int]) =
    normalizeTypeParamsInTypeValyeTermForParamsS(term, nextArgParam)(IntMap())((params, params.values.toSet.size)).mapElements(_._1, identity).swap

  def normalizeTypeParamsForTermParamsAndLambdaParams[T](term: TypeValueTerm[T], nextArgParam: Int)(termParams: Map[Int, Int], lambdaParams: Map[Int, Int]) =
    normalizeTypeParamsInTypeValyeTermForParamsS(term, nextArgParam)(lambdaParams)((termParams, termParams.values.toSet.size))._2
}
