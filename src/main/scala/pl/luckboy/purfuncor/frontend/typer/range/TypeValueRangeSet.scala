/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer.range
import scala.collection.immutable.SortedMap
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend.typer._
import pl.luckboy.purfuncor.util._

case class TypeValueRangeSet[T](ranges: SortedMap[TypeValueRange, TypeValueRangeValue[T]])
{
  private def intersect(rangeSet: TypeValueRangeSet[T]) = {
    val newRanges3 = ranges.foldLeft(SortedMap[TypeValueRange, TypeValueRangeValue[T]]()) {
      case (newRanges, (range, value)) =>
        val from = TypeValueRange(range.minIdx, range.minIdx)
        val to = TypeValueRange(range.maxIdx, range.maxIdx)
        rangeSet.ranges.from(from).to(to).foldLeft(newRanges) {
          case (newRanges2, (range2, value2)) =>
            if(range.minIdx >= range2.minIdx && range.maxIdx <= range2.maxIdx)
              newRanges2 + (range -> (value | value2))
            else if(range.minIdx <= range2.minIdx && range.maxIdx >= range2.maxIdx)
              newRanges2 + (range2 -> (value | value2))
            else
              newRanges2
        }
    }
    TypeValueRangeSet(newRanges3)
  }
  
  def & (rangeSet: TypeValueRangeSet[T]) =
    if(ranges.size < rangeSet.ranges.size) intersect(rangeSet) else rangeSet.intersect(this)
    
  private def union(rangeSet: TypeValueRangeSet[T]) = {
    val newRanges2 = ranges.foldLeft(SortedMap[TypeValueRange, TypeValueRangeValue[T]]()) {
      case (newRanges, (range, value)) =>
        val from = TypeValueRange(range.minIdx, range.minIdx)
        val to = TypeValueRange(range.maxIdx, range.maxIdx)
        val ranges2 = rangeSet.ranges.from(from).to(to)
        ranges2.headOption.map {
          case (range2, value2) =>
            if(range.minIdx <= range2.minIdx && range.maxIdx >= range2.maxIdx)
              newRanges + (range -> ranges2.values.foldLeft(value) { _ | _ })
            else if(range.minIdx <= range2.minIdx && range.maxIdx >= range2.maxIdx)
              newRanges + (range2 -> newRanges.get(range).map(value | value2 |).getOrElse(value | value2))
            else
              newRanges
        }.getOrElse(newRanges)
    }
    val newRanges4 = ranges.foldLeft(newRanges2) {
      case (newRanges3, pair @ (range, _)) => if(newRanges2.contains(range)) newRanges3 else newRanges3 + pair 
    }
    val newRanges6 = ranges.foldLeft(newRanges2) {
      case (newRanges5, pair @ (range, _)) => if(newRanges2.contains(range)) newRanges5 else newRanges5 + pair 
    }
    TypeValueRangeSet(newRanges6)
  }
  
  def | (rangeSet: TypeValueRangeSet[T]) =
    if(ranges.size < rangeSet.ranges.size) union(rangeSet) else rangeSet.union(this)
    
  def isEmpty = ranges.isEmpty
    
  def superset(sepRangeSet: TypeValueRangeSet[T]) = {
    val newRanges2 = ranges.foldLeft(SortedMap[TypeValueRange, TypeValueRangeValue[T]]()) {
      case (newRanges, (range, value)) =>
        val from = TypeValueRange(range.minIdx, range.minIdx)
        val to = TypeValueRange(range.maxIdx, range.maxIdx)
        val sepRanges = sepRangeSet.ranges.from(from).to(to)
        sepRanges.headOption.map {
          case (sepRange, sepValue) =>
            if(range.minIdx <= sepRange.minIdx && range.maxIdx >= sepRange.maxIdx)
              newRanges + (range -> sepRanges.values.foldLeft(value) { _ | _ })
            else if(range.minIdx <= sepRange.minIdx && range.maxIdx >= sepRange.maxIdx)
              newRanges + (sepRange -> newRanges.get(range).map(value | sepValue |).getOrElse(value | sepValue))
            else
              newRanges
        }.getOrElse(newRanges)
    }
    val newRanges4 = ranges.foldLeft(newRanges2) {
      case (newRanges3, pair @ (range, _)) => if(newRanges2.contains(range)) newRanges3 else newRanges3 + pair 
    }
    TypeValueRangeSet(newRanges4)
  }
  
  def withConds(myRange: TypeValueRange, otherRanges: Iterable[TypeValueRange], myTupleTypes: Seq[TupleType[T]]) =
    TypeValueRangeSet(ranges.mapValues { _.withCond(myRange, otherRanges, myTupleTypes) })
    
  def swapLeafIdxPairsWithMyLeafIdx(leafIdx: Int) =
    TypeValueRangeSet(ranges.mapValues { v => v.copy(myLeafIdxs = UnionSet(leafIdx), otherLeafIdxs = v.myLeafIdxs) })

  def withMyParam(leafIdx: Int, param: Int) =
    TypeValueRangeSet(ranges.mapValues { v => v.copy(myParams = UnionSet(leafIdx -> param)) })
    
  def value = ranges.values.foldLeft(TypeValueRangeValue.empty[T]) { _ | _ }
}

object TypeValueRangeSet
{
  def empty[T] = TypeValueRangeSet[T](SortedMap())
  
  def full[T] = TypeValueRangeSet[T](SortedMap(TypeValueRange.full -> TypeValueRangeValue.empty))
}

case class TypeValueRange(minIdx: Int, maxIdx: Int)

object TypeValueRange
{
  def full = TypeValueRange(0, Integer.MAX_VALUE)
}

case class TypeValueRangeValue[T](
    myLeafIdxs: UnionSet[Int],
    otherLeafIdxs: UnionSet[Int],
    myParams: UnionSet[(Int, Int)],
    otherTupleTypes: Option[List[TupleType[T]]], 
    conds: UnionSet[((TypeValueRange, Iterable[TypeValueRange]), TypeValueRangeCondition[T])])
{
  def | (value: TypeValueRangeValue[T]) =
    TypeValueRangeValue[T](
        myLeafIdxs = myLeafIdxs | value.myLeafIdxs,
        otherLeafIdxs = otherLeafIdxs | value.otherLeafIdxs,
        myParams = myParams | value.myParams,
        otherTupleTypes = (otherTupleTypes |@| value.otherTupleTypes) { 
          (ott1, ott2) => if(ott1.size < ott2.size) ott1 else ott2
        }.orElse(otherTupleTypes).orElse(value.otherTupleTypes),
        conds = conds | value.conds)
        
  def withCond(myRange: TypeValueRange, otherRanges: Iterable[TypeValueRange], myTupleTypes: Seq[TupleType[T]]) =
    copy(conds = otherTupleTypes.map { ott => UnionSet((myRange, otherRanges) -> TypeValueRangeCondition(myTupleTypes, ott)) | conds }.getOrElse(conds))
}

object TypeValueRangeValue
{
  def empty[T] = TypeValueRangeValue[T](UnionSet.empty, UnionSet.empty, UnionSet.empty, none, UnionSet.empty)
}

case class TypeValueRangeCondition[T](myTupleTypes: Seq[TupleType[T]], otherTupleTypes: List[TupleType[T]])
    
object TypeValueRangeCondition
{
  def empty[T] = TypeValueRangeCondition[T](Nil, Nil)
}
