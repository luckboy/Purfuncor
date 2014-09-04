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
    allParams: SortedMap[Int, SortedSet[Int]],
    fieldSetTypeIdents: Map[FieldSetTypeIdentityKey, BuiltinTypeIdentity[T]],
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
  
  private def fromTypeConjunctionNode[T](node: TypeValueNode[T], conjTupleTypes: List[TupleType[T]], args: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isSupertype: Boolean)(leafIdx: Int)(info: LogicalTypeValueTermInfo[T]): (LogicalTypeValueTermInfo[T], Map[TypeValueIdentity[T], Map[Int, Set[Int]]], Option[TypeValueRange]) = {
	val (conjDepthRangeSet, nextConjDepthRangeSets) = info.conjDepthRangeSets.headOption.map {
     (_, info.conjDepthRangeSets.tail)
    }.getOrElse(TypeValueRangeSet.empty[T], Nil)
    node match {
      case TypeValueBranch(childs, tupleTypes, _) =>
        val info2 = info.copy(conjDepthRangeSets = nextConjDepthRangeSets)
        val params = childs.foldLeft(SortedMap[Int, SortedSet[Int]]()) {
          case (newParams, TypeValueLeaf(ident @ TypeParamAppIdentity(param), _, _)) =>
            val argCount = args.get(ident).map { _.size }.getOrElse(0)
            newParams + (argCount -> (newParams.getOrElse(argCount, SortedSet[Int]()) + param))
          case (newParams, _)                                                        =>
            newParams
        }
        val conjTupleTypes2 = tupleTypes.toList ++ conjTupleTypes
        val (info3, _, paramAppIdxs, optRange) = childs.foldLeft((info2, leafIdx, Map[TypeValueIdentity[T], Map[Int, Set[Int]]](), none[TypeValueRange])) {
          case ((newInfo, newLeafIdx, newParamAppIdxs, optNewRange), child) =>
            val (newInfo2, newParamAppIdxs2, optNewRange2) = fromTypeDisjunctionNode(child, conjTupleTypes2, args, isSupertype)(newLeafIdx)(newInfo)
            (newInfo2, newLeafIdx + child.leafCount, newParamAppIdxs |+| newParamAppIdxs2, (optNewRange |@| optNewRange2) { _ | _ }.orElse(optNewRange2))
        }
        val conjRangeSets2 = info3.conjRangeSets
        val otherTupleTypes = some(conjTupleTypes2)
        val conjRangeSets3 = conjRangeSets2 ++ paramAppIdxs.map { 
          case (ident, idxs) => 
            val value = TypeValueRangeValue[T](UnionSet.fromIterable(idxs.keys), UnionSet(), UnionSet.fromIterable(idxs.flatMap { case (i, pais) => pais.map { (i, _) } }), UnionSet(), UnionSet(), otherTupleTypes, UnionSet())
            ident -> (conjRangeSets2.getOrElse(ident, TypeValueRangeSet.empty) | optRange.map { r => TypeValueRangeSet(SortedMap(r -> value)) }.getOrElse(TypeValueRangeSet.empty))
        }
        val conjDepthRangeSet2 = conjDepthRangeSet | optRange.map { r => TypeValueRangeSet(SortedMap(r -> TypeValueRangeValue.empty[T])) }.getOrElse(TypeValueRangeSet.empty)
        val range = TypeValueRange(leafIdx, leafIdx + node.leafCount - 1)
        (info3.copy(
            conjRangeSets = conjRangeSets3, conjDepthRangeSets = conjDepthRangeSet2 :: info3.conjDepthRangeSets,
            allParams = params ++ info3.allParams.map { case (lac, ps) => lac -> (ps ++ params.getOrElse(lac, Set())) }), Map(), optRange)
      case TypeValueLeaf(ident, paramAppIdx, leafCount) =>
        val argCount = args.get(ident).map { _.size }.getOrElse(0)
        val (params, paramAppIdxPair) = ident match {
          case ident @ TypeParamAppIdentity(param) => 
            (SortedMap(argCount -> SortedSet(param)), some(leafIdx -> paramAppIdx))
          case _                                   =>
            (SortedMap(argCount -> SortedSet[Int]()), none)
        }
        val fieldSetTypeIdentPair = fieldSetTypeIdentityPairFromTypeValueIdentity(ident)
        val range = TypeValueRange(leafIdx, leafIdx + leafCount - 1)
        val paramAppIdxs = if(ident.isTypeParamAppIdentity) Set(paramAppIdx) else Set[Int]()
        val value = TypeValueRangeValue[T](UnionSet(leafIdx), UnionSet(), UnionSet.fromIterable(paramAppIdxs.map(leafIdx ->)), UnionSet(), UnionSet(), none, UnionSet())
        val conjRangeSets2 = info.conjRangeSets + (ident -> (info.conjRangeSets.getOrElse(ident, TypeValueRangeSet.empty) | TypeValueRangeSet(SortedMap(range -> value))))
        val conjDepthRangeSet2 = conjDepthRangeSet | TypeValueRangeSet(SortedMap(range -> TypeValueRangeValue.empty))
        (info.copy(
            conjRangeSets = conjRangeSets2, conjDepthRangeSets = conjDepthRangeSet2 :: nextConjDepthRangeSets,
            allParams = params ++ info.allParams.map { case (lac, ps) => lac -> (ps ++ params.getOrElse(lac, Set())) },
            fieldSetTypeIdents = info.fieldSetTypeIdents ++ fieldSetTypeIdentPair,
            expandedLeafCount = info.expandedLeafCount + 1, unexpandedLeafCount = info.unexpandedLeafCount + 1), Map(ident -> Map(leafIdx -> paramAppIdxs)), some(range))
      case _ =>
        val expandedNode = node.typeValueBranchOrTypeValueLeaf(true, isSupertype, true)
        val unexpandedNode = node.typeValueBranchOrTypeValueLeaf(true, isSupertype, false)
        val (info2, paramAppIdxs, optRange) = fromTypeConjunctionNode(expandedNode, conjTupleTypes, args, isSupertype)(leafIdx)(info)
        val (info3, paramAppIdxs2, optRange2) = fromTypeConjunctionNode(unexpandedNode, conjTupleTypes, args, isSupertype)(leafIdx)(info2.copy(expandedLeafCount = info.expandedLeafCount, unexpandedLeafCount = info.unexpandedLeafCount))
        (info3.copy(expandedLeafCount = info2.expandedLeafCount), paramAppIdxs2 |+| paramAppIdxs, (optRange |@| optRange2) { _ | _ })
    }
  }
  
  private def fromTypeDisjunctionNode[T](node: TypeValueNode[T], conjTupleTypes: List[TupleType[T]], args: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isSupertype: Boolean)(leafIdx: Int)(info: LogicalTypeValueTermInfo[T]): (LogicalTypeValueTermInfo[T], Map[TypeValueIdentity[T], Map[Int, Set[Int]]], Option[TypeValueRange]) = {
    val (disjDepthRangeSet, nextDisjDepthRangeSets) = info.disjDepthRangeSets.headOption.map {
      (_, info.disjDepthRangeSets.tail)
    }.getOrElse(TypeValueRangeSet.empty[T], Nil)
    node match {
      case TypeValueBranch(childs, _, _) =>
        val info2 = info.copy(disjDepthRangeSets = nextDisjDepthRangeSets)
        val params = childs.foldLeft(SortedMap[Int, SortedSet[Int]]()) {
          case (newParams, TypeValueLeaf(ident @ TypeParamAppIdentity(param), _, _)) =>
            val argCount = args.get(ident).map { _.size }.getOrElse(0)
            newParams + (argCount -> (newParams.getOrElse(argCount, SortedSet[Int]()) + param))
          case (newParams, _)                                                        =>
            newParams
        }
        val (info3, _, paramAppIdxs, optRange) = childs.foldLeft((info2, leafIdx, Map[TypeValueIdentity[T], Map[Int, Set[Int]]](), none[TypeValueRange])) {
          case ((newInfo, newLeafIdx, newParamAppIdxs, optNewRange), child) =>
            val (newInfo2, newParamAppIdxs2, optNewRange2) = fromTypeConjunctionNode(child, conjTupleTypes, args, isSupertype)(newLeafIdx)(newInfo)
            (newInfo2, newLeafIdx + child.leafCount, newParamAppIdxs |+| newParamAppIdxs2, (optNewRange |@| optNewRange2) { _ | _ }.orElse(optNewRange2))
        }
        val disjRangeSets2 = info3.disjRangeSets
        val disjRangeSets3 = disjRangeSets2 ++ paramAppIdxs.map { 
          case (ident, idxs) => 
            val value = TypeValueRangeValue[T](UnionSet.fromIterable(idxs.keys), UnionSet(), UnionSet.fromIterable(idxs.flatMap { case (i, pais) => pais.map { (i, _) } }), UnionSet(), UnionSet(), none, UnionSet())
            ident -> (disjRangeSets2.getOrElse(ident, TypeValueRangeSet.empty) | optRange.map { r => TypeValueRangeSet(SortedMap(r -> value)) }.getOrElse(TypeValueRangeSet.empty))
        }
        val disjDepthRangeSet2 = disjDepthRangeSet | optRange.map { r => TypeValueRangeSet(SortedMap(r -> TypeValueRangeValue.empty[T])) }.getOrElse(TypeValueRangeSet.empty)
        val range = TypeValueRange(leafIdx, leafIdx + node.leafCount - 1)
        (info3.copy(
            disjRangeSets = disjRangeSets3, disjDepthRangeSets = disjDepthRangeSet2 :: info3.disjDepthRangeSets,
            allParams = params ++ info3.allParams.map { case (lac, ps) => lac -> (ps ++ params.getOrElse(lac, Set())) }), Map(), optRange)
      case TypeValueLeaf(ident, paramAppIdx, leafCount) =>
        val argCount = args.get(ident).map { _.size }.getOrElse(0)
        val (params, paramAppIdxPair) = ident match {
          case ident @ TypeParamAppIdentity(param) => 
            (SortedMap(argCount -> SortedSet(param)), some(leafIdx -> paramAppIdx))
          case _                                   =>
            (SortedMap(argCount -> SortedSet[Int]()), none)
        }
        val fieldSetTypeIdentPair = fieldSetTypeIdentityPairFromTypeValueIdentity(ident)
        val range = TypeValueRange(leafIdx, leafIdx + leafCount - 1)
        val paramAppIdxs = if(ident.isTypeParamAppIdentity) Set(paramAppIdx) else Set[Int]()
        val value = TypeValueRangeValue[T](UnionSet(leafIdx), UnionSet(), UnionSet.fromIterable(paramAppIdxs.map(leafIdx ->)), UnionSet(), UnionSet(), none, UnionSet())
        val disjRangeSets2 = info.disjRangeSets + (ident -> (info.disjRangeSets.getOrElse(ident, TypeValueRangeSet.empty[T]) | TypeValueRangeSet(SortedMap(range -> value))))
        val disjDepthRangeSet2 = disjDepthRangeSet | TypeValueRangeSet(SortedMap(range -> TypeValueRangeValue.empty))
        (info.copy(disjRangeSets = disjRangeSets2, disjDepthRangeSets = disjDepthRangeSet2 :: nextDisjDepthRangeSets,
            allParams = params ++ info.allParams.map { case (lac, ps) => lac -> (ps ++ params.getOrElse(lac, Set())) },
            fieldSetTypeIdents = info.fieldSetTypeIdents ++ fieldSetTypeIdentPair,
            expandedLeafCount = info.expandedLeafCount + 1, unexpandedLeafCount = info.unexpandedLeafCount + 1), Map(ident -> Map(leafIdx -> paramAppIdxs)), some(range))
      case _ =>
        val expandedNode = node.typeValueBranchOrTypeValueLeaf(false, isSupertype, true)
        val unexpandedNode = node.typeValueBranchOrTypeValueLeaf(false, isSupertype, false)
        val (info2, paramAppIdxs, optRange) = fromTypeDisjunctionNode(expandedNode, conjTupleTypes, args, isSupertype)(leafIdx)(info)
        val (info3, paramAppIdxs2, optRange2) = fromTypeDisjunctionNode(unexpandedNode, conjTupleTypes, args, isSupertype)(leafIdx)(info2.copy(expandedLeafCount = info.expandedLeafCount, unexpandedLeafCount = info.unexpandedLeafCount))
        (info3.copy(expandedLeafCount = info2.expandedLeafCount), paramAppIdxs2 |+| paramAppIdxs, (optRange |@| optRange2) { _ | _ })
    }
  }
  
  def fromTypeValueNodeWithArgs[T](node: TypeValueNode[T], args: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isSupertype: Boolean) = {
    val (info, paramAppIdxs, optRange) = fromTypeConjunctionNode(node, Nil, args, isSupertype)(0)(LogicalTypeValueTermInfo(Map(), Map(), Nil, Nil, SortedMap(), Map(), 0, 0))
    val disjRangeSets2 = info.disjRangeSets ++ paramAppIdxs.map { 
      case (ident, idxs) => 
        val value = TypeValueRangeValue[T](UnionSet.fromIterable(idxs.keys), UnionSet(), UnionSet.fromIterable(idxs.flatMap { case (i, pais) => pais.map { (i, _) } }), UnionSet(), UnionSet(), none, UnionSet())
        ident -> (info.disjRangeSets.getOrElse(ident, TypeValueRangeSet.empty) | optRange.map { r => TypeValueRangeSet(SortedMap(r -> value)) }.getOrElse(TypeValueRangeSet.empty))
    }
    info.copy(disjRangeSets = disjRangeSets2)
  }  
}

sealed class FieldSetTypeIdentityKey
case class FirstArgFieldSetTypeIdentityKey(ident: FieldSetTypeArgIdentity) extends  FieldSetTypeIdentityKey
case class SecondArgFieldSetTypeIdentityKey(ident: FieldSetTypeArgIdentity) extends  FieldSetTypeIdentityKey
case object NoArgFieldSetTypeIdentityKey extends FieldSetTypeIdentityKey

