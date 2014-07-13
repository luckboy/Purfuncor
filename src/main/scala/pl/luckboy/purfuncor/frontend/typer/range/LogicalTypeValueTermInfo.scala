/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer.range
import scala.collection.immutable.IntMap
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
    conjParams: Map[TypeValueRange, Map[Int, List[SortedSet[Int]]]],
    disjParams: Map[TypeValueRange, Map[Int, List[SortedSet[Int]]]],
    allParams: SortedSet[Int],
    fieldSetTypeIdents: Map[FieldSetTypeIdentityKey, BuiltinTypeIdentity[T]],
    paramAppIdxs: Map[Int, Int],
    expandedLeafCount: Int,
    unexpandedLeafCount: Int)
    
object LogicalTypeValueTermInfo
{
  private def fieldSetTypeIdentityPairFromTypeValueIdentity[T](ident: TypeValueIdentity[T]) =
    ident match {
      case builtinTypeIdent @ BuiltinTypeIdentity(TypeBuiltinFunction.FieldSet1 | TypeBuiltinFunction.FieldSet2, Seq(argIdent1, argIdent2)) =>
        (argIdent1, argIdent2) match {
          case (FieldSetTypeParamIdentity(_), FieldSetTypeParamIdentity(_)) =>
            some(NoArgFieldSetTypeIdentityKey -> builtinTypeIdent)
          case (FieldSetTypeParamIdentity(_), _)                            =>
            some(SecondArgFieldSetTypeIdentityKey(argIdent2) -> builtinTypeIdent)
          case (_, FieldSetTypeParamIdentity(_))                            =>
            some(FirstArgFieldSetTypeIdentityKey(argIdent1) -> builtinTypeIdent)
          case _                                                            =>
            none
        }
      case _ =>
        none
    }
  
  private def fromTypeConjunctionNode[T](node: TypeValueNode[T], conjTupleTypes: List[TupleType[T]], conjParamSets: Map[Int, List[SortedSet[Int]]], disjParamSets: Map[Int, List[SortedSet[Int]]], args: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]])(leafIdx: Int)(info: LogicalTypeValueTermInfo[T]): (LogicalTypeValueTermInfo[T], Map[TypeValueIdentity[T], Set[Int]], Option[TypeValueRange]) = {
	val (conjDepthRangeSet, nextConjDepthRangeSets) = info.conjDepthRangeSets.headOption.map {
     (_, info.conjDepthRangeSets.tail)
    }.getOrElse(TypeValueRangeSet.empty[T], Nil)
    node match {
      case TypeValueBranch(childs, tupleTypes, _) =>
        val info2 = info.copy(conjDepthRangeSets = nextConjDepthRangeSets)
        val params = childs.foldLeft(IntMap[SortedSet[Int]]()) {
          case (newParams, TypeValueLeaf(ident @ TypeParamAppIdentity(param), _, _)) =>
            val argCount = args.get(ident).map { _.size }.getOrElse(0)
            newParams + (argCount -> (newParams.getOrElse(argCount, SortedSet[Int]()) + param))
          case (newParams, _)                                                        =>
            newParams
        }
        val conjParamSets2 = conjParamSets.map { case (n, ps) => n -> params.get(n).map { _ :: ps }.getOrElse(ps) }
        val (info3, _, leafIdxs, optRange) = childs.foldLeft((info2, leafIdx, Map[TypeValueIdentity[T], Set[Int]](), none[TypeValueRange])) {
          case ((newInfo, newLeafIdx, newLeafIdxs, optNewRange), child) =>
            val (newInfo2, newLeafIdxs2, optNewRange2) = fromTypeDisjunctionNode(child, conjTupleTypes, conjParamSets2, disjParamSets, args)(newLeafIdx)(newInfo)
            (newInfo2, newLeafIdx + child.leafCount, newLeafIdxs |+| newLeafIdxs2, (optNewRange |@| optNewRange2) { _ | _ }.orElse(optNewRange2))
        }
        val conjRangeSets2 = info3.conjRangeSets
        val otherTupleTypes = some(tupleTypes.toList ++ conjTupleTypes)
        val conjRangeSets3 = conjRangeSets2 ++ leafIdxs.map { 
          case (ident, idxs) => 
            val value = TypeValueRangeValue[T](UnionSet.fromIterable(idxs), UnionSet(), UnionSet(), UnionSet(), otherTupleTypes, UnionSet())
            ident -> (conjRangeSets2.getOrElse(ident, TypeValueRangeSet.empty) | optRange.map { r => TypeValueRangeSet(SortedMap(r -> value)) }.getOrElse(TypeValueRangeSet.empty))
        }
        val conjDepthRangeSet2 = conjDepthRangeSet | optRange.map { r => TypeValueRangeSet(SortedMap(r -> TypeValueRangeValue.empty[T])) }.getOrElse(TypeValueRangeSet.empty)
        val range = TypeValueRange(leafIdx, leafIdx + node.leafCount - 1)
        (info3.copy(
            conjRangeSets = conjRangeSets3, conjDepthRangeSets = conjDepthRangeSet2 :: info3.conjDepthRangeSets,
            conjParams = info3.conjParams + (range -> conjParamSets2), allParams = info3.allParams ++ params.values.flatten), Map(), optRange)
      case TypeValueLeaf(ident, paramAppIdx, leafCount) =>
        val argCount = args.get(ident).map { _.size }.getOrElse(0)
        val (params, paramAppIdxPair) = ident match {
          case ident @ TypeParamAppIdentity(param) => 
            (IntMap(argCount -> SortedSet(param)), some(leafIdx -> paramAppIdx))
          case _                                   =>
            (IntMap(argCount -> SortedSet[Int]()), none)
        }
        val fieldSetTypeIdentPair = fieldSetTypeIdentityPairFromTypeValueIdentity(ident)
        val conjParamSets2 = conjParamSets.map { case (n, ps) => n -> params.get(n).map { _ :: ps }.getOrElse(ps) }
        val range = TypeValueRange(leafIdx, leafIdx + leafCount - 1)
        val pair = TypeValueRangeValue[T](UnionSet(leafIdx), UnionSet(), UnionSet(), UnionSet(), none, UnionSet())
        val conjRangeSets2 = info.conjRangeSets + (ident -> (info.conjRangeSets.getOrElse(ident, TypeValueRangeSet.empty) | TypeValueRangeSet(SortedMap(range -> pair))))
        val conjDepthRangeSet2 = conjDepthRangeSet | TypeValueRangeSet(SortedMap(range -> TypeValueRangeValue.empty))
        (info.copy(
            conjRangeSets = conjRangeSets2, conjDepthRangeSets = conjDepthRangeSet2 :: nextConjDepthRangeSets,
            conjParams = info.conjParams + (range -> conjParamSets2), allParams = info.allParams ++ params.values.flatten, 
            paramAppIdxs = info.paramAppIdxs ++ paramAppIdxPair,
            fieldSetTypeIdents = info.fieldSetTypeIdents ++ fieldSetTypeIdentPair,
            expandedLeafCount = info.expandedLeafCount + 1, unexpandedLeafCount = info.unexpandedLeafCount + 1), Map(ident -> Set(leafIdx)), some(range))
      case _ =>
        val expandedNode = node.typeValueBranchOrTypeValueLeaf(true)
        val unexpandedNode = node.typeValueBranchOrTypeValueLeaf(false)
        val (info2, leafIdxs, optRange) = fromTypeConjunctionNode(expandedNode, conjTupleTypes, conjParamSets, disjParamSets, args)(leafIdx)(info)
        val (info3, leafIdxs2, optRange2) = fromTypeConjunctionNode(unexpandedNode, conjTupleTypes, conjParamSets, disjParamSets, args)(leafIdx)(info2.copy(expandedLeafCount = info.expandedLeafCount, unexpandedLeafCount = info.unexpandedLeafCount))
        (info3.copy(expandedLeafCount = info2.expandedLeafCount), leafIdxs2 |+| leafIdxs, (optRange |@| optRange2) { _ | _ })
    }
  }
  
  private def fromTypeDisjunctionNode[T](node: TypeValueNode[T], conjTupleTypes: List[TupleType[T]], conjParamSets: Map[Int, List[SortedSet[Int]]], disjParamSets: Map[Int, List[SortedSet[Int]]], args: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]])(leafIdx: Int)(info: LogicalTypeValueTermInfo[T]): (LogicalTypeValueTermInfo[T], Map[TypeValueIdentity[T], Set[Int]], Option[TypeValueRange]) = {
    val (disjDepthRangeSet, nextDisjDepthRangeSets) = info.disjDepthRangeSets.headOption.map {
      (_, info.disjDepthRangeSets.tail)
    }.getOrElse(TypeValueRangeSet.empty[T], Nil)
    node match {
      case TypeValueBranch(childs, _, _) =>
        val info2 = info.copy(disjDepthRangeSets = nextDisjDepthRangeSets)
        val params = childs.foldLeft(IntMap[SortedSet[Int]]()) {
          case (newParams, TypeValueLeaf(ident @ TypeParamAppIdentity(param), _, _)) =>
            val argCount = args.get(ident).map { _.size }.getOrElse(0)
            newParams + (argCount -> (newParams.getOrElse(argCount, SortedSet[Int]()) + param))
          case (newParams, _)                                                        =>
            newParams
        }
        val disjParamSets2 = disjParamSets.map { case (n, ps) => n -> params.get(n).map { _ :: ps }.getOrElse(ps) }
        val (info3, _, leafIdxs, optRange) = childs.foldLeft((info2, leafIdx, Map[TypeValueIdentity[T], Set[Int]](), none[TypeValueRange])) {
          case ((newInfo, newLeafIdx, newLeafIdxs, optNewRange), child) =>
            val (newInfo2, newLeafIdxs2, optNewRange2) = fromTypeConjunctionNode(child, conjTupleTypes, conjParamSets, disjParamSets2, args)(newLeafIdx)(newInfo)
            (newInfo2, newLeafIdx + child.leafCount, newLeafIdxs |+| newLeafIdxs2, (optNewRange |@| optNewRange2) { _ | _ }.orElse(optNewRange2))
        }
        val disjRangeSets2 = info3.disjRangeSets
        val disjRangeSets3 = disjRangeSets2 ++ leafIdxs.map { 
          case (ident, idxs) => 
            val value = TypeValueRangeValue[T](UnionSet.fromIterable(idxs), UnionSet(), UnionSet(), UnionSet(), none, UnionSet())
            ident -> (disjRangeSets2.getOrElse(ident, TypeValueRangeSet.empty) | optRange.map { r => TypeValueRangeSet(SortedMap(r -> value)) }.getOrElse(TypeValueRangeSet.empty))
        }
        val disjDepthRangeSet2 = disjDepthRangeSet | optRange.map { r => TypeValueRangeSet(SortedMap(r -> TypeValueRangeValue.empty[T])) }.getOrElse(TypeValueRangeSet.empty)
        val range = TypeValueRange(leafIdx, leafIdx + node.leafCount - 1)
        (info3.copy(
            disjRangeSets = disjRangeSets3, disjDepthRangeSets = disjDepthRangeSet2 :: info3.disjDepthRangeSets,
            disjParams = info3.disjParams + (range -> disjParamSets2), allParams = info3.allParams ++ params.values.flatten), Map(), optRange)
      case TypeValueLeaf(ident, paramAppIdx, leafCount) =>
        val argCount = args.get(ident).map { _.size }.getOrElse(0)
        val (params, paramAppIdxPair) = ident match {
          case ident @ TypeParamAppIdentity(param) => 
            (IntMap(argCount -> SortedSet(param)), some(leafIdx -> paramAppIdx))
          case _                                   =>
            (IntMap(argCount -> SortedSet[Int]()), none)
        }
        val fieldSetTypeIdentPair = fieldSetTypeIdentityPairFromTypeValueIdentity(ident)
        val disjParamSets2 = disjParamSets.map { case (n, ps) => n -> params.get(n).map { _ :: ps }.getOrElse(ps) }
        val range = TypeValueRange(leafIdx, leafIdx + leafCount - 1)
        val pair = TypeValueRangeValue[T](UnionSet(leafIdx), UnionSet(), UnionSet(), UnionSet(), none, UnionSet())
        val disjRangeSets2 = info.disjRangeSets + (ident -> (info.disjRangeSets.getOrElse(ident, TypeValueRangeSet.empty[T]) | TypeValueRangeSet(SortedMap(range -> pair))))
        val disjDepthRangeSet2 = disjDepthRangeSet | TypeValueRangeSet(SortedMap(range -> TypeValueRangeValue.empty))
        (info.copy(disjRangeSets = disjRangeSets2, disjDepthRangeSets = disjDepthRangeSet2 :: nextDisjDepthRangeSets,
            disjParams = info.disjParams + (range -> disjParamSets2), allParams = info.allParams ++ params.values.flatten,
            paramAppIdxs = info.paramAppIdxs ++ paramAppIdxPair,
            fieldSetTypeIdents = info.fieldSetTypeIdents ++ fieldSetTypeIdentPair,
            expandedLeafCount = info.expandedLeafCount + 1, unexpandedLeafCount = info.unexpandedLeafCount + 1), Map(ident -> Set(leafIdx)), some(range))
      case _ =>
        val expandedNode = node.typeValueBranchOrTypeValueLeaf(true)
        val unexpandedNode = node.typeValueBranchOrTypeValueLeaf(false)
        val (info2, leafIdxs, optRange) = fromTypeDisjunctionNode(expandedNode, conjTupleTypes, conjParamSets, disjParamSets, args)(leafIdx)(info)
        val (info3, leafIdxs2, optRange2) = fromTypeDisjunctionNode(unexpandedNode, conjTupleTypes, conjParamSets, disjParamSets, args)(leafIdx)(info2.copy(expandedLeafCount = info.expandedLeafCount, unexpandedLeafCount = info.unexpandedLeafCount))
        (info3.copy(expandedLeafCount = info2.expandedLeafCount), leafIdxs2 |+| leafIdxs, (optRange |@| optRange2) { _ | _ })
    }
  }
  
  def fromTypeValueNodeWithArgs[T](node: TypeValueNode[T], args: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]]) = {
    val (info, varIdxs, optRange) = fromTypeConjunctionNode(node, Nil, IntMap(), IntMap(), args)(0)(LogicalTypeValueTermInfo(Map(), Map(), Nil, Nil, Map(), Map(), SortedSet(), Map(), IntMap(), 0, 0))
    val disjRangeSets2 = info.disjRangeSets ++ varIdxs.map { 
      case (ident, idxs) => 
        val value = TypeValueRangeValue[T](UnionSet.fromIterable(idxs), UnionSet(), UnionSet(), UnionSet(), none, UnionSet())
        ident -> (info.disjRangeSets.getOrElse(ident, TypeValueRangeSet.empty) | optRange.map { r => TypeValueRangeSet(SortedMap(r -> value)) }.getOrElse(TypeValueRangeSet.empty))
    }
    info.copy(disjRangeSets = disjRangeSets2)
  }  
}

sealed class FieldSetTypeIdentityKey
case class FirstArgFieldSetTypeIdentityKey(ident: FieldSetTypeArgIdentity) extends  FieldSetTypeIdentityKey
case class SecondArgFieldSetTypeIdentityKey(ident: FieldSetTypeArgIdentity) extends  FieldSetTypeIdentityKey
case object NoArgFieldSetTypeIdentityKey extends FieldSetTypeIdentityKey

