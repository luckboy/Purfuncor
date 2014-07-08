/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer.range
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend.typer._
import TypeValueTermUtils._

object LogicalTypeValueTermUtils
{
  private def typeParamsFromLogicalValueNode[T](node: TypeValueNode[T]): Set[Int] =
    node match {
      case TypeValueBranch(childs, tupleTypes, _)           =>
        childs.flatMap(typeParamsFromLogicalValueNode).toSet ++ tupleTypes.flatMap(typeParamsFromTypeValueTerm)
      case TypeValueLeaf(TypeParamAppIdentity(param), _, _) =>
        Set(param)
      case TypeValueLeaf(_, _, _)                           =>
        Set()
      case GlobalTypeAppNode(_, childs, tupleTypes, _, _)   =>
        childs.flatMap(typeParamsFromLogicalValueNode).toSet ++ tupleTypes.flatMap(typeParamsFromTypeValueTerm)
    }
  
  def typeParamsFromLogicalTypeValueTerm[T](term: LogicalTypeValueTerm[T]) =
    term match {
      case LogicalTypeValueTerm(conjNode, args) =>
        typeParamsFromLogicalValueNode(conjNode) ++ args.values.flatMap { _.flatMap {  a => typeParamsFromTypeValueTerm(a.body) -- a.argParams } }
    }
  
  def typeArgParamsFromLogicalTypeValueTerm[T](term: LogicalTypeValueTerm[T]) =
    term match {
      case LogicalTypeValueTerm(conjNode, args) =>
        args.values.flatMap { _.flatMap { _.argParams } }.toSet
    }
  
  private def normalizeTypeParamsInTypeValueNodesForParamsS[T](nodes: Seq[TypeValueNode[T]], nextArgParam: Int)(lambdaParams: Map[Int, Int])(pair: (Map[Int, Int], Int)) =
    nodes.foldLeft((pair, Vector[TypeValueNode[T]]())) {
      case ((p, ts), n) => normalizeTypeParamsInTypeValueNodeForParamsS(n, nextArgParam)(lambdaParams)(p).mapElements(identity, ts :+ _)
    }
  
  private def normalizeTypeParamsInTupleTypesForParamsS[T](terms: Seq[TupleType[T]], nextArgParam: Int)(lambdaParams: Map[Int, Int])(pair: (Map[Int, Int], Int)) =
    terms.foldLeft((pair, Vector[TupleType[T]]())) {
      case ((pair2, tupleTypes2), TupleType(args)) =>
        val (pair3, args2) = normalizeTypeParamsInTypeValueTermsForParamsS(args, nextArgParam)(lambdaParams)(pair2)
        (pair3, tupleTypes2 :+ TupleType(args2))
    }
  
  private def normalizeTypeParamInTypeValueIdenityS[T](ident: TypeValueIdentity[T], nextArgParam: Int)(lambdaParams: Map[Int, Int])(pair: (Map[Int, Int], Int)) =
    ident match {
      case TypeParamAppIdentity(param) =>
        val (termParams, nextTermParam) = pair
        val (termParams2, nextTermParam2) = if(termParams.contains(param) || lambdaParams.contains(param)) 
          (termParams, nextTermParam)
        else
          (termParams + (param -> nextTermParam), nextTermParam + 1)
        val param2 = lambdaParams.getOrElse(param, termParams.getOrElse(param, nextTermParam))
        ((termParams2, nextTermParam2), TypeParamAppIdentity(param2))
      case _ =>
        (pair, ident)
    }
  
  private def normalizeTypeParamsInTypeValueNodeForParamsS[T](node: TypeValueNode[T], nextArgParam: Int)(lambdaParams: Map[Int, Int])(pair: (Map[Int, Int], Int)): ((Map[Int, Int], Int), TypeValueNode[T]) =
    node match {
      case TypeValueBranch(childs, tupleTypes, leafCount)             =>
        val (pair2, childs2) = normalizeTypeParamsInTypeValueNodesForParamsS(childs, nextArgParam)(lambdaParams)(pair)
        val (pair3, tupleTypes2) = normalizeTypeParamsInTupleTypesForParamsS(tupleTypes, nextArgParam)(lambdaParams)(pair2)
        (pair3, TypeValueBranch(childs2, tupleTypes2, leafCount))
      case TypeValueLeaf(ident, paramAppIdx, leafCount)               =>
        val (pair2, ident2) = normalizeTypeParamInTypeValueIdenityS(ident, nextArgParam)(lambdaParams)(pair)
        (pair2, TypeValueLeaf(ident2, paramAppIdx, leafCount))
      case GlobalTypeAppNode(loc, childs, tupleTypes, leafCount, sym) =>
        val (pair2, childs2) = normalizeTypeParamsInTypeValueNodesForParamsS(childs, nextArgParam)(lambdaParams)(pair)
        val (pair3, tupleTypes2) = normalizeTypeParamsInTupleTypesForParamsS(tupleTypes, nextArgParam)(lambdaParams)(pair2)
        (pair3, GlobalTypeAppNode(loc, childs2, tupleTypes2, leafCount, sym))
    }
  
  def normalizeTypeParamsInLogicalTypeValyeTermForParamsS[T](term: LogicalTypeValueTerm[T], nextArgParam: Int)(lambdaParams: Map[Int, Int])(pair: (Map[Int, Int], Int)) =
    term match {
      case LogicalTypeValueTerm(conjNode, args) =>
        val (pair2, conjNode2) = normalizeTypeParamsInTypeValueNodeForParamsS(conjNode, nextArgParam)(lambdaParams)(pair)
        val (pair3, args2) = args.foldLeft((pair2, Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]]())) {
          case ((newPair, newArgs), (ident, argLambdas)) =>
            val (newPair2, ident2) = normalizeTypeParamInTypeValueIdenityS(ident, nextArgParam)(lambdaParams)(newPair)
            val (newPair3, argLambdas2) = normalizeTypeParamsInTypeValueLambdasForParamsS(argLambdas, nextArgParam)(lambdaParams)(newPair)
            (newPair3, newArgs + (ident2 -> argLambdas2))
        }
        (pair3, LogicalTypeValueTerm(conjNode2, args2))
    }
}
