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
  
  private def substituteTypeValueLambdasInTypeValueNodes[T](nodes: Iterable[TypeValueNode[T]], argMap: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], paramLambdas: Map[Int, TypeValueLambda[T]], isConj: Boolean)(newNodeMap: Map[TypeValueIdentity[T], TypeValueNode[T]], newArgMap: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]]) =
    nodes.foldLeft(some((newNodeMap, newArgMap, Vector[TypeValueNode[T]]()))) {
      case (Some((newNodeMap, newArgMap, newNodes)), node) =>
        substituteTypeValueLambdasInTypeValueNode(node, argMap, paramLambdas, isConj)(newNodeMap, newArgMap).map {
          _.mapElements(identity, identity, newNodes :+ _)
        }
      case (None, _)                                       =>
        none
    }
  
  private def substituteTypeValueLambdasInTypeValueNode[T](node: TypeValueNode[T], argMap: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], paramLambdas: Map[Int, TypeValueLambda[T]], isConj: Boolean)(newNodeMap: Map[TypeValueIdentity[T], TypeValueNode[T]], newArgMap: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]]): Option[(Map[TypeValueIdentity[T], TypeValueNode[T]], Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], TypeValueNode[T])] = {
    node match {
      case TypeValueBranch(childs, tupleTypes, leafCount)             =>
        substituteTypeValueLambdasInTypeValueNodes(childs, argMap, paramLambdas, !isConj)(newNodeMap, newArgMap).flatMap {
          case (newNodeMap, newArgMap, childs2) =>
            substituteTypeValueLambdasInTupleTypes(tupleTypes, paramLambdas).map {
              tts => (newNodeMap, newArgMap, TypeValueBranch(childs2, tts, childs2.foldLeft(0) { _ + _.leafCount }))
            }
        }
      case leaf @ TypeValueLeaf(ident, paramAppIdx, leafCount)        =>
        val optTuple = newNodeMap.get(ident) match {
          case Some(node) =>
            some((newNodeMap, newArgMap, node))
          case None       =>
            argMap.get(ident).flatMap(leaf.typeValueTerm) match {
              case Some(Some(term)) =>
                substituteTypeValueLambdasInTypeValueTerm(term, paramLambdas).map { 
                  _.unevaluatedLogicalTypeValueTerm
                }.flatMap {
                  case LogicalTypeValueTerm(conjNode2 @ TypeValueLeaf(ident2, _, _), argMap2) =>
                    some((newNodeMap + (ident -> conjNode2), argMap ++ argMap2, conjNode2))
                  case LogicalTypeValueTerm(conjNode2, argMap2)                               =>
                    val leafIdents = argMap.keySet & argMap2.keySet
                    if(leafIdents.forall { i => (argMap.get(i) |@| argMap2.get(i)) { (as1, as2) => as1.toVector === as2.toVector }.getOrElse(false) })
                      some((newNodeMap + (ident -> conjNode2), argMap ++ argMap2, conjNode2))
                    else
                      none
                }
              case Some(None)       =>
                some((newNodeMap + (ident -> node), argMap + (ident -> Vector()), node))
              case None             =>
                none
            }
        }
        optTuple.map { 
          case (newNodeMap2, newArgMap2, branch: TypeValueBranch[T]) if !isConj =>
            (newNodeMap2, newArgMap2, TypeValueBranch(Vector(branch), Vector(), branch.leafCount).normalizedTypeValueNode)
          case tuple                                                          =>
            tuple
        }
      case GlobalTypeAppNode(loc, childs, tupleTypes, leafCount, sym) =>
        val optTuple = substituteTypeValueLambdasInTypeValueNode(TypeValueLeaf(UnexpandedGlobalTypeAppIdentity(loc, sym), 0, leafCount), argMap, paramLambdas, isConj)(newNodeMap, newArgMap)
        optTuple match {
          case Some((newNodeMap2, newArgMap2, TypeValueLeaf(UnexpandedGlobalTypeAppIdentity(_, _), _, _))) =>
            substituteTypeValueLambdasInTypeValueNode(TypeValueBranch(childs, tupleTypes, leafCount), argMap, paramLambdas, isConj)(newNodeMap, newArgMap)
          case _                                                                                           =>
            none
        }
    }
  }
  
  def substituteTypeValueLambdasInLogicalTypeValueTerm[T](term: LogicalTypeValueTerm[T], paramLambdas: Map[Int, TypeValueLambda[T]]): Option[TypeValueTerm[T]] =
    substituteTypeValueLambdasInTypeValueNode(term.conjNode, term.args, paramLambdas, true)(Map(), Map()).map { 
      t => LogicalTypeValueTerm(t._3, t._2)
    }
  
  private def normalizeTypeParamsInTypeValueNodesForParamsS[T](nodes: Seq[TypeValueNode[T]], nextArgParam: Int)(lambdaParams: Map[Int, Int])(pair: (Map[Int, Int], Int)) =
    nodes.foldLeft((pair, Vector[TypeValueNode[T]]())) {
      case ((p, ts), n) => normalizeTypeParamsInTypeValueNodeForParamsS(n, nextArgParam)(lambdaParams)(p).mapElements(identity, ts :+ _)
    }
  
  private def normalizeTypeParamInTypeValueIdenityS[T](ident: TypeValueIdentity[T], nextArgParam: Int)(lambdaParams: Map[Int, Int])(pair: (Map[Int, Int], Int)) =
    ident match {
      case TypeParamAppIdentity(param) =>
        normalizeTypeParamForParamsS(param, nextArgParam)(lambdaParams)(pair).mapElements(identity, TypeParamAppIdentity(_))
      case _ =>
        (pair, ident)
    }
  
  private def normalizeTypeParamsInTypeValueNodeForParamsS[T](node: TypeValueNode[T], nextArgParam: Int)(lambdaParams: Map[Int, Int])(pair: (Map[Int, Int], Int)): ((Map[Int, Int], Int), TypeValueNode[T]) =
    node match {
      case TypeValueBranch(childs, tupleTypes, leafCount)             =>
        val (pair2, childs2) = normalizeTypeParamsInTypeValueNodesForParamsS(childs, nextArgParam)(lambdaParams)(pair)
        val (pair3, tupleTypes2) = normalizeTypeParamsInTupleTypesForParamsS(tupleTypes, nextArgParam)(lambdaParams)(pair2)
        (pair3, TypeValueBranch(childs2, tupleTypes2, leafCount))
      case TypeValueLeaf(ident, _, leafCount)                         =>
        val (pair2, ident2) = normalizeTypeParamInTypeValueIdenityS(ident, nextArgParam)(lambdaParams)(pair)
        (pair2, TypeValueLeaf(ident2, 0, leafCount))
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
