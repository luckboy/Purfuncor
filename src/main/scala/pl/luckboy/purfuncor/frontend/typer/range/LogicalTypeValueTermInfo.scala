/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer.range
import scala.collection.immutable.SortedSet
import scala.collection.immutable.SortedMap
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend.typer._
import pl.luckboy.purfuncor.util._

case class LogicalTypeValueTermInfo[T](
    conjRangeSets: Map[TypeValueIdentity[T], TypeValueRangeSet[T]],
    disjRangeSets: Map[TypeValueIdentity[T], TypeValueRangeSet[T]],
    conjDepthRangeSets: List[TypeValueRangeSet[T]],
    disjDepthRangeSets: List[TypeValueRangeSet[T]],
    conjParams: Map[TypeValueRange, List[SortedSet[Int]]],
    disjParams: Map[TypeValueRange, List[SortedSet[Int]]],
    allParams: SortedSet[Int],
    expandedLeafCount: Int,
    unexpandedLeafCount: Int)
    
object LogicalTypeValueTermInfo
{
  private type TupleT[T] = (Map[TypeValueIdentity[T], TypeValueRangeSet[T]], Map[TypeValueIdentity[T], TypeValueRangeSet[T]], List[TypeValueRangeSet[T]], List[TypeValueRangeSet[T]], Map[TypeValueRange, List[SortedSet[Int]]], Map[TypeValueRange, List[SortedSet[Int]]], SortedSet[Int], Int, Int)
  
  private def rangeSetsFromTypeConjunctionNode[T](node: TypeValueNode[T], conjTupleTypes: List[TupleType[T]], conjParamSets: List[SortedSet[Int]], disjParamSets: List[SortedSet[Int]])(leafIdx: Int)(tuple: TupleT[T]): (TupleT[T], Map[TypeValueIdentity[T], Set[Int]], Option[TypeValueRange]) = {
	val (
	    conjRangeSets, 
	    disjRangeSets, 
	    conjDepthRangeSets, 
	    disjDepthRangeSets, 
	    conjParams, 
	    disjParams, 
	    allParams, 
	    expandedLeafCount, 
	    unexpandedLeafCount) = tuple
	val (conjDepthRangeSet, nextConjDepthRangeSets) = conjDepthRangeSets.headOption.map {
     (_, conjDepthRangeSets.tail)
    }.getOrElse(TypeValueRangeSet.empty[T], Nil)
    node match {
      case TypeValueBranch(childs, tupleTypes, _) =>
        val tuple2 = tuple.copy(_3 = nextConjDepthRangeSets)
        val params = SortedSet[Int]() ++ childs.flatMap {
          case TypeValueLeaf(TypeParamAppIdentity(param), _) => Vector(param)
          case _                                             => Vector()
        }
        val conjParamSets2 = params :: conjParamSets
        val (tuple3, _, leafIdxs, optRange) = childs.foldLeft((tuple2, leafIdx, Map[TypeValueIdentity[T], Set[Int]](), none[TypeValueRange])) {
          case ((newTuple, newLeafIdx, newLeafIdxs, optNewRange), child) =>
            val (newTuple2, newLeafIdxs2, optNewRange2) = rangeSetsFromTypeDisjunctionNode(child, conjTupleTypes, conjParamSets2, disjParamSets)(newLeafIdx)(newTuple)
            (newTuple2, newLeafIdx + child.leafCount, newLeafIdxs |+| newLeafIdxs2, (optNewRange |@| optNewRange2) { _ | _ }.orElse(optNewRange2))
        }
        val conjRangeSets2 = tuple3._1
        val otherTupleTypes = some(tupleTypes.toList ++ conjTupleTypes)
        val conjRangeSets3 = conjRangeSets2 ++ leafIdxs.map { 
          case (ident, idxs) => 
            val value = TypeValueRangeValue[T](UnionSet.fromIterable(idxs), UnionSet(), UnionSet(), otherTupleTypes, UnionSet())
            ident -> (conjRangeSets2.getOrElse(ident, TypeValueRangeSet.empty) | optRange.map { r => TypeValueRangeSet(SortedMap(r -> value)) }.getOrElse(TypeValueRangeSet.empty))
        }
        val conjDepthRangeSet2 = conjDepthRangeSet | optRange.map { r => TypeValueRangeSet(SortedMap(r -> TypeValueRangeValue.empty[T])) }.getOrElse(TypeValueRangeSet.empty)
        val range = TypeValueRange(leafIdx, leafIdx + node.leafCount)
        (tuple3.copy(_1 = conjRangeSets3, _3 = conjDepthRangeSet2 :: tuple3._3, _5 = tuple3._5 + (range -> conjParamSets2), _7 = tuple3._7 ++ params), Map(), optRange)
      case TypeValueLeaf(ident, leafCount) =>
        val params = ident match {
          case TypeParamAppIdentity(param) => SortedSet(param)
          case _                           => SortedSet[Int]()
        }
        val conjParamSets2 = params :: conjParamSets
        val range = TypeValueRange(leafIdx, leafIdx + leafCount)
        val pair = TypeValueRangeValue[T](UnionSet(leafIdx), UnionSet(), UnionSet(), none, UnionSet())
        val conjRangeSets2 = conjRangeSets + (ident -> (conjRangeSets.getOrElse(ident, TypeValueRangeSet.empty) | TypeValueRangeSet(SortedMap(range -> pair))))
        val conjDepthRangeSet2 = conjDepthRangeSet | TypeValueRangeSet(SortedMap(range -> TypeValueRangeValue.empty))
        (tuple.copy(_1 = conjRangeSets2, _3 = conjDepthRangeSet2 :: nextConjDepthRangeSets, _5 = tuple._5 + (range -> conjParamSets2), _7 = tuple._7 ++ params, _8 = tuple._8 + 1, _9 = tuple._9 + 1), Map(ident -> Set(leafIdx)), some(range))
      case _ =>
        val expandedNode = node.typeValueBranchOrTypeValueLeaf(true)
        val unexpandedNode = node.typeValueBranchOrTypeValueLeaf(false)
        val (tuple2, leafIdxs, optRange) = rangeSetsFromTypeConjunctionNode(expandedNode, conjTupleTypes, conjParamSets, disjParamSets)(leafIdx)(tuple)
        val (tuple3, leafIdxs2, optRange2) = rangeSetsFromTypeConjunctionNode(unexpandedNode, conjTupleTypes, conjParamSets, disjParamSets)(leafIdx)(tuple2.copy(_8 = tuple._8, _9 = tuple._9))
        (tuple3.copy(_8 = tuple2._8), leafIdxs2 |+| leafIdxs, (optRange |@| optRange2) { _ | _ })
    }
  }
  
  private def rangeSetsFromTypeDisjunctionNode[T](node: TypeValueNode[T], conjTupleTypes: List[TupleType[T]], conjParamSets: List[SortedSet[Int]], disjParamSets: List[SortedSet[Int]])(leafIdx: Int)(tuple: TupleT[T]): (TupleT[T], Map[TypeValueIdentity[T], Set[Int]], Option[TypeValueRange]) = {
    val (
        conjRangeSets,
        disjRangeSets,
        conjDepthRangeSets,
        disjDepthRangeSets,
        conjParams,
        disjParams,
        allParams,
        expandedLeafCount, 
        unexpandedLeafCount) = tuple
    val (disjDepthRangeSet, nextDisjDepthRangeSets) = disjDepthRangeSets.headOption.map {
      (_, disjDepthRangeSets.tail)
    }.getOrElse(TypeValueRangeSet.empty[T], Nil)
    node match {
      case TypeValueBranch(childs, _, _) =>
        val tuple2 = tuple.copy(_4 = nextDisjDepthRangeSets)
        val params = SortedSet[Int]() ++ childs.flatMap {
          case TypeValueLeaf(TypeParamAppIdentity(param), 1) => Vector(param)
          case _                                             => Vector()
        }
        val disjParamSets2 = params :: disjParamSets
        val (tuple3, _, leafIdxs, optRange) = childs.foldLeft((tuple2, leafIdx, Map[TypeValueIdentity[T], Set[Int]](), none[TypeValueRange])) {
          case ((newTuple, newLeafIdx, newLeafIdxs, optNewRange), child) =>
            val (newTuple2, newLeafIdxs2, optNewRange2) = rangeSetsFromTypeConjunctionNode(child, conjTupleTypes, conjParamSets, disjParamSets2)(newLeafIdx)(newTuple)
            (newTuple2, newLeafIdx + child.leafCount, newLeafIdxs |+| newLeafIdxs2, (optNewRange |@| optNewRange2) { _ | _ }.orElse(optNewRange2))
        }
        val disjRangeSets2 = tuple3._2
        val disjRangeSets3 = disjRangeSets2 ++ leafIdxs.map { 
          case (ident, idxs) => 
            val value = TypeValueRangeValue[T](UnionSet.fromIterable(idxs), UnionSet(), UnionSet(), none, UnionSet())
            ident -> (disjRangeSets2.getOrElse(ident, TypeValueRangeSet.empty) | optRange.map { r => TypeValueRangeSet(SortedMap(r -> value)) }.getOrElse(TypeValueRangeSet.empty))
        }
        val disjDepthRangeSet2 = disjDepthRangeSet | optRange.map { r => TypeValueRangeSet(SortedMap(r -> TypeValueRangeValue.empty[T])) }.getOrElse(TypeValueRangeSet.empty)
        val range = TypeValueRange(leafIdx, leafIdx + node.leafCount)
        (tuple3.copy(_2 = disjRangeSets3, _4 = disjDepthRangeSet2 :: tuple3._4, _6 = tuple3._6 + (range -> disjParamSets2), _7 = tuple3._7 ++ params, _8 = tuple._8 + 1, _9 = tuple._9 + 1), Map(), optRange)
      case TypeValueLeaf(ident, leafCount) =>
        val params = ident match {
          case TypeParamAppIdentity(param) => SortedSet(param)
          case _                           => SortedSet[Int]()
        }
        val disjParamSets2 = params :: disjParamSets
        val range = TypeValueRange(leafIdx, leafIdx + leafCount)
        val pair = TypeValueRangeValue[T](UnionSet(leafIdx), UnionSet(), UnionSet(), none, UnionSet())
        val disjRangeSets2 = disjRangeSets + (ident -> (disjRangeSets.getOrElse(ident, TypeValueRangeSet.empty[T]) | TypeValueRangeSet(SortedMap(range -> pair))))
        val disjDepthRangeSet2 = disjDepthRangeSet | TypeValueRangeSet(SortedMap(range -> TypeValueRangeValue.empty))
        (tuple.copy(_2 = disjRangeSets2, _4 = disjDepthRangeSet2 :: nextDisjDepthRangeSets, _6 = tuple._6 + (range -> disjParamSets2), _7 = tuple._7 ++ params), Map(ident -> Set(leafIdx)), some(range))
      case _ =>
        val expandedNode = node.typeValueBranchOrTypeValueLeaf(true)
        val unexpandedNode = node.typeValueBranchOrTypeValueLeaf(false)
        val (tuple2, leafIdxs, optRange) = rangeSetsFromTypeDisjunctionNode(expandedNode, conjTupleTypes, conjParamSets, disjParamSets)(leafIdx)(tuple)
        val (tuple3, leafIdxs2, optRange2) = rangeSetsFromTypeDisjunctionNode(unexpandedNode, conjTupleTypes, conjParamSets, disjParamSets)(leafIdx)(tuple2.copy(_8 = tuple._8, _9 = tuple._9))
        (tuple3.copy(_8 = tuple2._8), leafIdxs2 |+| leafIdxs, (optRange |@| optRange2) { _ | _ })
    }
  }
  
  private def rangeSetsFromTypeValueNode[T](node: TypeValueNode[T])(tuple: (Map[TypeValueIdentity[T], TypeValueRangeSet[T]], Map[TypeValueIdentity[T], TypeValueRangeSet[T]], List[TypeValueRangeSet[T]], List[TypeValueRangeSet[T]])) = {
    val (tuple, varIdxs, optRange) = rangeSetsFromTypeConjunctionNode(node, Nil, Nil, Nil)(0)((Map(), Map(), Nil, Nil, Map(), Map(), SortedSet(), 0, 0))
    val (
        conjRangeSets,
        disjRangeSets,
        conjDepthRangeSets,
        disjDepthRangeSets,
        conjParams,
        disjParams,
        allParams,
        expandedLeafCount,
        unexpandedLeafCount) = tuple
    val disjRangeSets2 = tuple._2 ++ varIdxs.map { 
      case (ident, idxs) => 
        val value = TypeValueRangeValue[T](UnionSet.fromIterable(idxs), UnionSet(), UnionSet(), none, UnionSet())
        ident -> (disjRangeSets.getOrElse(ident, TypeValueRangeSet.empty) | optRange.map { r => TypeValueRangeSet(SortedMap(r -> value)) }.getOrElse(TypeValueRangeSet.empty))
    }
    tuple.copy(_2 = disjRangeSets2)
  }
  
  def fromTypeValueNode[T](node: TypeValueNode[T]) = {
    val (
        conjRangeSet,
        disjRangeSet,
        conjDepthRangeSets,
        disjDepthRangeSets,
        conjParams,
        disjParams,allParams,
        expandedLeafCount,
        unexpandedLeafCount) = rangeSetsFromTypeValueNode(node)((Map(), Map(), Nil, Nil))
    LogicalTypeValueTermInfo(conjRangeSet, disjRangeSet, conjDepthRangeSets, disjDepthRangeSets, conjParams, disjParams, allParams, expandedLeafCount, unexpandedLeafCount)
  }
}
