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
    allParams: SortedSet[Int])
    
object LogicalTypeValueTermInfo
{
  private def rangeSetsFromTypeConjunctionNode[T](node: TypeValueNode[T], conjTupleTypes: List[TupleType[T]])(leafIdx: Int)(tuple: (Map[TypeValueIdentity[T], TypeValueRangeSet[T]], Map[TypeValueIdentity[T], TypeValueRangeSet[T]], List[TypeValueRangeSet[T]], List[TypeValueRangeSet[T]])): ((Map[TypeValueIdentity[T], TypeValueRangeSet[T]], Map[TypeValueIdentity[T], TypeValueRangeSet[T]], List[TypeValueRangeSet[T]], List[TypeValueRangeSet[T]]), Map[TypeValueIdentity[T], Set[Int]], Option[TypeValueRange]) = {
	val (conjRangeSets, disjRangeSets, conjDepthRangeSets, disjDepthRangeSets) = tuple
	val (conjDepthRangeSet, nextConjDepthRangeSets) = conjDepthRangeSets.headOption.map {
     (_, conjDepthRangeSets.tail)
    }.getOrElse(TypeValueRangeSet.empty[T], Nil)
    node match {
      case TypeValueBranch(childs, tupleTypes, _) =>
        val tuple2 = tuple.copy(_3 = nextConjDepthRangeSets)
        val (tuple3, _, leafIdxs, optRange) = childs.foldLeft((tuple2, leafIdx, Map[TypeValueIdentity[T], Set[Int]](), none[TypeValueRange])) {
          case ((newTuple, newLeafIdx, newLeafIdxs, optNewRange), child) =>
            val (newTuple2, newLeafIdxs2, optNewRange2) = rangeSetsFromTypeDisjunctionNode(child, conjTupleTypes)(newLeafIdx)(newTuple)
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
        (tuple3.copy(_1 = conjRangeSets3, _3 = conjDepthRangeSet2 :: tuple3._3), Map(), optRange)
      case TypeValueLeaf(ident) =>
        val range = TypeValueRange(leafIdx, leafIdx)
        val pair = TypeValueRangeValue[T](UnionSet(leafIdx), UnionSet(), UnionSet(), none, UnionSet())
        val conjRangeSets2 = conjRangeSets + (ident -> (conjRangeSets.getOrElse(ident, TypeValueRangeSet.empty) | TypeValueRangeSet(SortedMap(range -> pair))))
        val conjDepthRangeSet2 = conjDepthRangeSet | TypeValueRangeSet(SortedMap(range -> TypeValueRangeValue.empty))
        (tuple.copy(_1 = conjRangeSets2, _3 = conjDepthRangeSet2 :: nextConjDepthRangeSets), Map(ident -> Set(leafIdx)), some(range))
    }
  }
  
  private def rangeSetsFromTypeDisjunctionNode[T](node: TypeValueNode[T], conjTupleTypes: List[TupleType[T]])(leafIdx: Int)(tuple: (Map[TypeValueIdentity[T], TypeValueRangeSet[T]], Map[TypeValueIdentity[T], TypeValueRangeSet[T]], List[TypeValueRangeSet[T]], List[TypeValueRangeSet[T]])): ((Map[TypeValueIdentity[T], TypeValueRangeSet[T]], Map[TypeValueIdentity[T], TypeValueRangeSet[T]], List[TypeValueRangeSet[T]], List[TypeValueRangeSet[T]]), Map[TypeValueIdentity[T], Set[Int]], Option[TypeValueRange]) = {
    val (conjRangeSets, disjRangeSets, conjDepthRangeSets, disjDepthRangeSets) = tuple
    val (disjDepthRangeSet, nextDisjDepthRangeSets) = disjDepthRangeSets.headOption.map {
      (_, disjDepthRangeSets.tail)
    }.getOrElse(TypeValueRangeSet.empty[T], Nil)
    node match {
      case TypeValueBranch(childs, _, _) =>
        val tuple2 = tuple.copy(_4 = nextDisjDepthRangeSets)
        val (tuple3, _, leafIdxs, optRange) = childs.foldLeft((tuple2, leafIdx, Map[TypeValueIdentity[T], Set[Int]](), none[TypeValueRange])) {
          case ((newTuple, newLeafIdx, newLeafIdxs, optNewRange), child) =>
            val (newTuple2, newLeafIdxs2, optNewRange2) = rangeSetsFromTypeConjunctionNode(child, conjTupleTypes)(newLeafIdx)(newTuple)
            (newTuple2, newLeafIdx + child.leafCount, newLeafIdxs |+| newLeafIdxs2, (optNewRange |@| optNewRange2) { _ | _ }.orElse(optNewRange2))
        }
        val disjRangeSets2 = tuple3._2
        val disjRangeSets3 = disjRangeSets2 ++ leafIdxs.map { 
          case (ident, idxs) => 
            val value = TypeValueRangeValue[T](UnionSet.fromIterable(idxs), UnionSet(), UnionSet(), none, UnionSet())
            ident -> (disjRangeSets2.getOrElse(ident, TypeValueRangeSet.empty) | optRange.map { r => TypeValueRangeSet(SortedMap(r -> value)) }.getOrElse(TypeValueRangeSet.empty))
        }
        val disjDepthRangeSet2 = disjDepthRangeSet | optRange.map { r => TypeValueRangeSet(SortedMap(r -> TypeValueRangeValue.empty[T])) }.getOrElse(TypeValueRangeSet.empty)
        (tuple3.copy(_2 = disjRangeSets3, _4 = disjDepthRangeSet2 :: tuple3._4), Map(), optRange)
      case TypeValueLeaf(ident) =>
        val range = TypeValueRange(leafIdx, leafIdx)
        val pair = TypeValueRangeValue[T](UnionSet(leafIdx), UnionSet(), UnionSet(), none, UnionSet())
        val disjRangeSets2 = disjRangeSets + (ident -> (disjRangeSets.getOrElse(ident, TypeValueRangeSet.empty[T]) | TypeValueRangeSet(SortedMap(range -> pair))))
        val disjDepthRangeSet2 = disjDepthRangeSet | TypeValueRangeSet(SortedMap(range -> TypeValueRangeValue.empty))
        (tuple.copy(_2 = disjRangeSets2, _4 = disjDepthRangeSet2 :: nextDisjDepthRangeSets), Map(ident -> Set(leafIdx)), some(range))
    }
  }
  
  private def rangeSetsFromTypeValueNode[T](node: TypeValueNode[T])(tuple: (Map[TypeValueIdentity[T], TypeValueRangeSet[T]], Map[TypeValueIdentity[T], TypeValueRangeSet[T]], List[TypeValueRangeSet[T]], List[TypeValueRangeSet[T]])) = {
    val (tuple, varIdxs, optRange) = rangeSetsFromTypeConjunctionNode(node, Nil)(0)((Map(), Map(), Nil, Nil))
    val (conjRangeSets, disjRangeSets, conjDepthRangeSets, disjDepthRangeSets) = tuple
    val disjRangeSets2 = tuple._2 ++ varIdxs.map { 
      case (ident, idxs) => 
        val value = TypeValueRangeValue[T](UnionSet.fromIterable(idxs), UnionSet(), UnionSet(), none, UnionSet())
        ident -> (disjRangeSets.getOrElse(ident, TypeValueRangeSet.empty) | optRange.map { r => TypeValueRangeSet(SortedMap(r -> value)) }.getOrElse(TypeValueRangeSet.empty))
    }
    tuple.copy(_2 = disjRangeSets2)
  }
  
  def fromTypeValueNode[T](node: TypeValueNode[T]) = {
    val (conjRangeSet, disjRangeSet, conjDepthRangeSets, disjDepthRangeSets) = rangeSetsFromTypeValueNode(node)((Map(), Map(), Nil, Nil))
    LogicalTypeValueTermInfo(conjRangeSet, disjRangeSet, conjDepthRangeSets, disjDepthRangeSets, Map(), Map(), SortedSet())
  }
}
