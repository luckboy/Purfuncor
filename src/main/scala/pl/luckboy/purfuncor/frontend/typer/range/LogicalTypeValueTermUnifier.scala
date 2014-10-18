/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer.range
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend.typer._
import pl.luckboy.purfuncor.util.CollectionUtils._
import pl.luckboy.purfuncor.util.StateUtils._
import TypeValueTermUnifier._

object LogicalTypeValueTermUnifier
{
  private type NodeTupleT[T] = (Map[TypeValueIdentity[T], TypeValueRangeSet[T]], SortedMap[Int, SortedSet[Int]], Map[FieldSetTypeIdentityKey, BuiltinTypeIdentity[T]])
  
  private type IndexTupleT[T] = (Set[Int], Map[Int, Set[Int]], Map[TypeValueRange, Set[Int]], Map[TypeValueRange, Int], Map[Int, Int], Map[Int, Int], Map[Int, Set[Int]])
  
  private def checkOrDistributeSupertypeConjunctionNode[T](node: TypeValueNode[T], nodeTuple: NodeTupleT[T], depthRangeSets: List[TypeValueRangeSet[T]], args: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isSupertype: Boolean, canExpandGlobalType: Boolean, isRoot: Boolean)(leafIdx: Int)(prevParam: Int): (Int, List[(Option[TypeValueRangeSet[T]], TypeValueNode[T])]) = {
    val depthRangeSet = depthRangeSets.headOption.getOrElse(TypeValueRangeSet.empty)
    (node match {
      case TypeValueBranch(childs, tupleTypes, leafCount) =>
        val ((prevParam2, _), pairs7) = stFoldLeftS(childs)(List[(Option[TypeValueRangeSet[T]], TypeValueNode[T])]()) {
          (pairs, child, stPair: (Int, Int)) =>
            val (newPrevParam, newLeafIdx) = stPair
            val (newPrevParam2, pairs2) = checkOrDistributeSupertypeDisjunctionNode(child, nodeTuple, depthRangeSets, args, isSupertype, canExpandGlobalType, false)(newLeafIdx)(newPrevParam)
            if(!pairs.isEmpty) {
              val (pairs6, pairIdxs4) = pairs.foldLeft((List[(Option[TypeValueRangeSet[T]], TypeValueNode[T])](), Set[Int]())) {
                case ((pairs3, pairIdxs), pair @ (optRangeSet, newChild)) =>
                  val (pairs5, pairIdxs3, isIntersected2) = pairs2.zipWithIndex.foldLeft((pairs3, pairIdxs, false)) {
                    case ((pairs4, pairIdxs2, isIntersected), (pair2 @ (optRangeSet2, newChild2), pairIdx)) =>
                      val optRangeSet3 = (optRangeSet |@| optRangeSet2) { _ & _ }
                      if(optRangeSet3.map { rs => !rs.isEmpty }.getOrElse(true))
                        (((optRangeSet3, newChild.withChildAndTupleTypes(newChild2, tupleTypes, isSupertype, canExpandGlobalType))) :: pairs4, pairIdxs2 + pairIdx, true)
                      else 
                        (pairs4, pairIdxs2, isIntersected)
                  }
                  (if(isIntersected2) pairs5 else (pair :: pairs5), pairIdxs3)
              }
              ((newPrevParam2, newLeafIdx + child.leafCount), pairs2.zipWithIndex.flatMap {
                case ((ors, n), pi) => if(!pairIdxs4.contains(pi)) List((ors, TypeValueBranch[T](Vector(n), tupleTypes, n.leafCount))) else Nil
              } ++ pairs6)
            } else {
              ((newPrevParam2, newLeafIdx + child.leafCount), pairs2.map { case (ors, n) => (ors, TypeValueBranch(Vector(n), tupleTypes, n.leafCount)) })
            }
        } ((prevParam, leafIdx))
        val pairs8 = pairs7.map { case (ors, n) => (ors.map { rs => rs.withConds(TypeValueRange(leafIdx, leafIdx + leafCount - 1), rs.ranges.keys, tupleTypes) }, n) }
        if(isRoot)
          (prevParam2, pairs8.headOption.map {
            pair =>
              if(pairs8.size > 1)
                List(pairs8.foldLeft((some(TypeValueRangeSet.empty[T]), TypeValueBranch(Vector(), Nil, 0): TypeValueNode[T])) {
                  case ((_, n), (_, n2)) => (none[TypeValueRangeSet[T]], n.conjOrDisjWithTupleTypes(n2, tupleTypes, isSupertype, canExpandGlobalType))
                })
              else
                List(pair)
          }.getOrElse(List((some(TypeValueRangeSet.empty[T]), TypeValueBranch[T](Vector(), Nil, 0)))))
        else
          (prevParam2, pairs8)
      case leaf: TypeValueLeaf[T] =>
        val (prevParam2, rangeSet) = checkSupertypeConjunctionLeaf(leaf, nodeTuple, depthRangeSets, args, isSupertype)(leafIdx)(prevParam)
        (prevParam2, List((if(!rangeSet.isEmpty) some(rangeSet) else none, node)))
      case globalTypeAppNode: GlobalTypeAppNode[T] =>
        val node2 = globalTypeAppNode.typeValueBranchOrTypeValueLeaf(isSupertype, isSupertype, canExpandGlobalType)
        checkOrDistributeSupertypeConjunctionNode(node2, nodeTuple, depthRangeSets, args, isSupertype, canExpandGlobalType, isRoot)(leafIdx)(prevParam)
    }).mapElements(identity, _.map { case (ors, n) => (ors.map { _.superset(depthRangeSet) }, n.normalizedTypeValueNode) })
  }
  
  private def checkOrDistributeSupertypeDisjunctionNode[T](node: TypeValueNode[T], nodeTuple: NodeTupleT[T], depthRangeSets: List[TypeValueRangeSet[T]], args: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isSupertype: Boolean, canExpandGlobalType: Boolean, isRoot: Boolean)(leafIdx: Int)(prevParam: Int): (Int, List[(Option[TypeValueRangeSet[T]], TypeValueNode[T])]) = {
    val depthRangeSets2 = depthRangeSets.headOption.map { _ => depthRangeSets.tail }.getOrElse(Nil)
    (node match {
      case TypeValueBranch(childs, tupleTypes, _) =>
        val ((prevParam2, _), pairs6) = stFoldLeftS(childs)(List[(Option[TypeValueRangeSet[T]], TypeValueNode[T])]()) {
          (pairs, child, stPair: (Int, Int)) =>
            val (newPrevParam, newLeafIdx) = stPair
            val (newPrevParam2, pairs2) = checkOrDistributeSupertypeConjunctionNode(child, nodeTuple, depthRangeSets2, args, isSupertype, canExpandGlobalType, isRoot)(newLeafIdx)(newPrevParam)
            val pairs5 = if(!pairs.isEmpty)
              pairs.foldLeft(List[(Option[TypeValueRangeSet[T]], TypeValueNode[T])]()) {
                case (pairs3, pair @ (optRangeSet, newChild)) =>
                  pairs2.zipWithIndex.foldLeft(pairs3) {
                    case (pairs4, (pair2 @ (optRangeSet2, newChild2), pairIdx)) =>
                      val optRangeSet3 = if(!isRoot)
                        (optRangeSet |@| optRangeSet2) { _ | _ }.orElse(optRangeSet).orElse(optRangeSet2)
                      else
                        (optRangeSet |@| optRangeSet2) { _ | _ }
                      ((optRangeSet3, newChild.withChildAndTupleTypes(newChild2, tupleTypes, !isSupertype, canExpandGlobalType))) :: pairs4
                  }
              }
            else
              pairs2.map { case (ors, n) => (ors, TypeValueBranch(Vector(n), tupleTypes, n.leafCount)) }
            ((newPrevParam2, newLeafIdx + child.leafCount), pairs5)
        } ((prevParam, leafIdx))
        (prevParam2, pairs6)
      case leaf: TypeValueLeaf[T] =>
        val (prevParam2, rangeSet) = checkSupertypeDisjunctionNode(leaf, nodeTuple, depthRangeSets2, args, isSupertype, canExpandGlobalType)(leafIdx)(prevParam)
        (prevParam2, List((if(!rangeSet.isEmpty) some(rangeSet) else none, node)))
      case globalTypeAppNode: GlobalTypeAppNode[T] =>
        val node2 = globalTypeAppNode.typeValueBranchOrTypeValueLeaf(!isSupertype, isSupertype, canExpandGlobalType)
        checkOrDistributeSupertypeDisjunctionNode(node2, nodeTuple, depthRangeSets, args, isSupertype, canExpandGlobalType, isRoot)(leafIdx)(prevParam)
    }).mapElements(identity, _.map { case (ors, n) => (ors, n.normalizedTypeValueNode) })
  }
    
  private def checkSupertypeConjunctionNode[T](node: TypeValueNode[T], nodeTuple: NodeTupleT[T], depthRangeSets: List[TypeValueRangeSet[T]], args: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isSupertype: Boolean, canExpandGlobalType: Boolean)(leafIdx: Int)(prevParam: Int): (Int, TypeValueRangeSet[T]) = {
    val depthRangeSet = depthRangeSets.headOption.getOrElse(TypeValueRangeSet.empty)
    (node match {
      case TypeValueBranch(childs, tupleTypes, leafCount) =>
        val ((prevParam2, _), rangeSet3) = stFoldLeftS(childs)(TypeValueRangeSet.full[T]) {
          (rangeSet, child, stTuple: (Int, Int)) =>
            val (newPrevParam, newLeafIdx) = stTuple
            val (newPrevParam2, rangeSet2) = checkSupertypeDisjunctionNode(child, nodeTuple, depthRangeSets, args, isSupertype, canExpandGlobalType)(newLeafIdx)(newPrevParam)
            ((newPrevParam2, newLeafIdx + child.leafCount), rangeSet & rangeSet2)
        } ((prevParam, leafIdx))
        (prevParam2, rangeSet3.withConds(TypeValueRange(leafIdx, leafIdx + leafCount - 1), rangeSet3.ranges.keys, tupleTypes))
      case leaf: TypeValueLeaf[T] =>
        checkSupertypeConjunctionLeaf(leaf, nodeTuple, depthRangeSets, args, isSupertype)(leafIdx)(prevParam)
      case globalTypeAppNode: GlobalTypeAppNode[T] =>
        val node2 = globalTypeAppNode.typeValueBranchOrTypeValueLeaf(isSupertype, isSupertype, canExpandGlobalType)
        checkSupertypeDisjunctionNode(node2, nodeTuple, depthRangeSets, args, isSupertype, canExpandGlobalType)(leafIdx)(prevParam)
    }).mapElements(identity, _.superset(depthRangeSet))
  }
  
  private def checkSupertypeDisjunctionNode[T](node: TypeValueNode[T], nodeTuple: NodeTupleT[T], depthRangeSets: List[TypeValueRangeSet[T]], args: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isSupertype: Boolean, canExpandGlobalType: Boolean)(leafIdx: Int)(prevParam: Int): (Int, TypeValueRangeSet[T]) = {
    val depthRangeSets2 = depthRangeSets.headOption.map { _ => depthRangeSets.tail }.getOrElse(Nil)
    node match {
      case TypeValueBranch(childs, _, _) =>
        val ((prevParam2, _), rangeSet3) = stFoldLeftS(childs)(TypeValueRangeSet.empty[T]) {
          (rangeSet, child, stPair: (Int, Int)) =>
            val (newPrevParam, newLeafIdx) = stPair
            val (newPrevParam2, rangeSet2) = checkSupertypeConjunctionNode(child, nodeTuple, depthRangeSets2, args, isSupertype, canExpandGlobalType)(newLeafIdx)(newPrevParam)
            ((newPrevParam2, newLeafIdx + child.leafCount), rangeSet | rangeSet2)
        } ((prevParam, leafIdx))
        (prevParam2, rangeSet3)
      case leaf: TypeValueLeaf[T] =>
        checkSupertypeDisjunctionLeaf(leaf, nodeTuple, depthRangeSets2, args, isSupertype)(leafIdx)(prevParam)
      case globalTypeAppNode: GlobalTypeAppNode[T] =>
        val node2 = globalTypeAppNode.typeValueBranchOrTypeValueLeaf(!isSupertype, isSupertype, canExpandGlobalType)
        checkSupertypeDisjunctionNode(node2, nodeTuple, depthRangeSets, args, isSupertype, canExpandGlobalType)(leafIdx)(prevParam)
    }
  }
  
  private def typeParamPairFromTypeValueLeaf[T](leaf: TypeValueLeaf[T]) =
    leaf match {
      case TypeValueLeaf(TypeParamAppIdentity(param), paramAppIdx, _) => some((param, paramAppIdx))
      case _                                                          => none
    }
  
  private def findAndCheckTypeValueRangeSetFromAllParams[T](leaf: TypeValueLeaf[T], nodeTuple: NodeTupleT[T], depthRangeSets2: List[TypeValueRangeSet[T]], args: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], myLeafParamPair: Option[(Int, Int)], isSupertype: Boolean)(leafIdx: Int)(prevParam: Int) = {
    val (rangeSets, allParams, fieldSetTypeIdents) = nodeTuple
    args.get(leaf.ident).flatMap {
      leafArgs =>
        val leafArgCount = leaf.ident match {
          case (_: BuiltinTypeIdentity[T]) | (_: UnittypeIdentity[T]) | (_: GrouptypeIdentity[T]) => 0
          case _                                                                                  => leafArgs.size
        }
        allParams.to(leafArgCount).foldLeft(none[Int]) {
          case (None, (_, paramSet)) => paramSet.from(prevParam + 1).headOption.orElse { paramSet.headOption }
          case (Some(param), _)      => Some(param)
        }.flatMap {
          param =>
            checkSupertypeValueLeaf(TypeValueLeaf(TypeParamAppIdentity(param), 0, 1), rangeSets, depthRangeSets2, myLeafParamPair, isSupertype)(leafIdx).map {
              (param, _)
            }
        }
    }
  }
    
  private def checkSupertypeConjunctionLeaf[T](leaf: TypeValueLeaf[T], nodeTuple: NodeTupleT[T], depthRangeSets: List[TypeValueRangeSet[T]], args: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isSupertype: Boolean)(leafIdx: Int)(prevParam: Int) = {
    val depthRangeSets2 = depthRangeSets.headOption.map { _ => depthRangeSets.tail }.getOrElse(Nil)
    val (rangeSets, allParams, fieldSetTypeIdents) = nodeTuple
    val myLeafParamPair = typeParamPairFromTypeValueLeaf(leaf)
    checkSupertypeValueLeaf(leaf, rangeSets, depthRangeSets2, myLeafParamPair, isSupertype)(leafIdx).map { (prevParam, _) }.orElse {
      if(!leaf.ident.isTupleTypeIdentity)
        findAndCheckTypeValueRangeSetFromAllParams(leaf, nodeTuple, depthRangeSets2, args, myLeafParamPair, isSupertype)(leafIdx)(prevParam)
      else
        none
    }.orElse {
      leaf match {
        case TypeValueLeaf(TypeParamAppIdentity(param), paramAppIdx, _) =>
          val bf = if(isSupertype) TypeBuiltinFunction.Any else TypeBuiltinFunction.Nothing
          checkSupertypeValueLeaf(TypeValueLeaf(BuiltinTypeIdentity(bf, Nil), 0, 1), rangeSets, depthRangeSets2, some((param, paramAppIdx)), isSupertype)(leafIdx).map {
              (param, _)
            }
        case _                                                          =>
          none
      }
    }.getOrElse((prevParam, TypeValueRangeSet.empty[T]))
  }
  
  private def checkSupertypeDisjunctionLeaf[T](leaf: TypeValueLeaf[T], nodeTuple: NodeTupleT[T], depthRangeSets2: List[TypeValueRangeSet[T]], args: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isSupertype: Boolean)(leafIdx: Int)(prevParam: Int) = {
    val (rangeSets, allParams, fieldSetTypeIdents) = nodeTuple
    val myLeafParamPair = typeParamPairFromTypeValueLeaf(leaf)
    checkSupertypeValueLeaf(leaf, rangeSets, depthRangeSets2, myLeafParamPair, isSupertype)(leafIdx).map { (prevParam, _) }.orElse {
      if(!leaf.ident.isTupleTypeIdentity)
        findAndCheckTypeValueRangeSetFromAllParams(leaf, nodeTuple, depthRangeSets2, args, myLeafParamPair, isSupertype)(leafIdx)(prevParam)
      else
        none
    }.getOrElse {
      args.get(leaf.ident).map {
        leafArgs =>
          leaf match {
            case TypeValueLeaf(TypeParamAppIdentity(param), paramAppIdx, _) =>
              val bf = if(isSupertype) TypeBuiltinFunction.Any else TypeBuiltinFunction.Nothing
              checkSupertypeValueLeaf(TypeValueLeaf(BuiltinTypeIdentity(bf, Nil), 0, 1), rangeSets, depthRangeSets2, some((param, paramAppIdx)), isSupertype)(leafIdx).map { (prevParam, _) }.getOrElse((prevParam, TypeValueRangeSet.empty[T]))
            case TypeValueLeaf(BuiltinTypeIdentity(TypeBuiltinFunction.FieldSet1 | TypeBuiltinFunction.FieldSet2, Seq(argIdent1, argIdent2)), _, _) =>
              // Exception for #FieldSet1 and #FieldSet2.
              fieldSetTypeIdents.get(FirstArgFieldSetTypeIdentityKey(argIdent1)).orElse { 
                fieldSetTypeIdents.get(SecondArgFieldSetTypeIdentityKey(argIdent2))
              }.orElse {
                fieldSetTypeIdents.get(NoArgFieldSetTypeIdentityKey) 
              }.flatMap {
                ident =>
                  checkSupertypeValueLeaf(TypeValueLeaf(ident, 0, 1), rangeSets, depthRangeSets2, none, isSupertype)(leafIdx).map {
                    (prevParam, _)
                  }
              }.getOrElse((prevParam, TypeValueRangeSet.empty[T]))
            case _ =>
              (prevParam, TypeValueRangeSet.empty[T])
          }
      }.getOrElse((prevParam, TypeValueRangeSet.empty[T]))
    }
  }
  
  private def checkSupertypeValueLeaf[T](leaf: TypeValueLeaf[T], rangeSets: Map[TypeValueIdentity[T], TypeValueRangeSet[T]], depthRangeSets2: List[TypeValueRangeSet[T]], myLeafParamPair: Option[(Int, Int)], isSupertype: Boolean)(leafIdx: Int) = {
    leaf match {
      case TypeValueLeaf(ident, _, _) =>
        rangeSets.get(ident).map {
          rs =>
            val rangeSet = depthRangeSets2.headOption.map(rs.withMyLeafIdx(leafIdx).superset).getOrElse(rs.withMyLeafIdx(leafIdx))
            val rangeSet2 = ident match {
              case TypeParamAppIdentity(identParam) => rangeSet.withMyParam(leafIdx, identParam)
              case _                                => rangeSet
            }
            myLeafParamPair.map { p => rangeSet2.withMyLeafParamAppIdx(p._1, p._2) }.getOrElse(rangeSet2)
        }
    }
  }
  
  private def generateCounterGraphForTypeConjunction[T](indexTuple: IndexTupleT[T], node: TypeValueNode[T], isSupertype: Boolean, canExpandGlobalType: Boolean)(leafIdx: Int)(tuple: (CounterGraph[CounterGraphLocation], Set[(CounterGraphLocation, CounterGraphLocation)], Map[Int, TypeValueIdentity[T]], Map[TypeValueRange, Set[Int]], Map[Int, Seq[TypeParamCondition[T]]], Map[Int, TypeValueIdentity[T]], Set[Int])): (CounterGraph[CounterGraphLocation], Set[(CounterGraphLocation, CounterGraphLocation)], Map[Int, TypeValueIdentity[T]], Map[TypeValueRange, Set[Int]], Map[Int, Seq[TypeParamCondition[T]]], Map[Int, TypeValueIdentity[T]], Set[Int]) = {
    val (otherLeafIdxs, otherLeafIdxSets, myCondIdxs, otherCondIdxs, myParams, myParamAppIdxs, myLeafParamAppIdxs) = indexTuple
    node match {
      case TypeValueBranch(childs, _, _) =>
        val uRange = TypeValueRange(leafIdx, leafIdx + node.leafCount - 1)
        val uLoc = CounterGraphLocation(uRange, isSupertype)
        val tuple4 = stFoldLeftS(childs)(tuple) {
          (tuple2, child, newLeafIdx: Int) =>
            val tuple3 = generateCounterGraphForTypeDisjunction(indexTuple, child, isSupertype, canExpandGlobalType)(newLeafIdx)(tuple2)
            val vLoc = CounterGraphLocation(TypeValueRange(newLeafIdx, newLeafIdx + child.leafCount - 1), isSupertype)
            (newLeafIdx + child.leafCount, tuple3.copy(_1 = tuple3._1.withTwoEdges(vLoc, uLoc)))
        } (leafIdx)._2
        tuple4.copy(
            _1 = if(childs.size > 0) tuple4._1.withCount(uLoc, 1) else tuple4._1,
            _4 = tuple4._4 |+| myCondIdxs.get(uRange).map { mcis => Map(uRange -> mcis) }.getOrElse(Map()))
      case TypeValueLeaf(_, _, _) =>
        generateCounterGraphForTypeDisjunction(indexTuple, node, isSupertype, canExpandGlobalType)(leafIdx)(tuple)
      case globalTypeAppNode: GlobalTypeAppNode[T] =>
        val node2 = globalTypeAppNode.typeValueBranchOrTypeValueLeaf(isSupertype, isSupertype, canExpandGlobalType)
        generateCounterGraphForTypeConjunction(indexTuple, node2, isSupertype, canExpandGlobalType)(leafIdx)(tuple)
    }
  }

  private def generateCounterGraphForTypeDisjunction[T](indexTuple: IndexTupleT[T], node: TypeValueNode[T], isSupertype: Boolean, canExpandGlobalType: Boolean)(leafIdx: Int)(tuple: (CounterGraph[CounterGraphLocation], Set[(CounterGraphLocation, CounterGraphLocation)], Map[Int, TypeValueIdentity[T]], Map[TypeValueRange, Set[Int]], Map[Int, Seq[TypeParamCondition[T]]], Map[Int, TypeValueIdentity[T]], Set[Int])): (CounterGraph[CounterGraphLocation], Set[(CounterGraphLocation, CounterGraphLocation)], Map[Int, TypeValueIdentity[T]], Map[TypeValueRange, Set[Int]], Map[Int, Seq[TypeParamCondition[T]]], Map[Int, TypeValueIdentity[T]], Set[Int]) = {
    val (otherLeafIdxs, otherLeafIdxSets, myCondIdxs, otherCondIdxs, myParams, myParamAppIdxs, myLeafParamAppIdxs) = indexTuple
    node match {
      case TypeValueBranch(childs, _, _) =>
        val uRange = TypeValueRange(leafIdx, leafIdx + node.leafCount - 1)
        val uLoc = CounterGraphLocation(uRange, isSupertype)
        val tuple4 = stFoldLeftS(childs)(tuple) {
          (tuple2, child, newLeafIdx: Int) =>
            val tuple3 = generateCounterGraphForTypeConjunction(indexTuple, child, isSupertype, canExpandGlobalType)(newLeafIdx)(tuple2)
            val vLoc = CounterGraphLocation(TypeValueRange(newLeafIdx, newLeafIdx + child.leafCount - 1), isSupertype)
            (newLeafIdx + child.leafCount, tuple3.copy(_1 = tuple3._1.withTwoEdges(vLoc, uLoc)))
        } (leafIdx)._2
        tuple4.copy(
            _1 = if(childs.size > 0) tuple4._1.withCount(uLoc, childs.size) else tuple4._1,
            _4 = tuple4._4 |+| myCondIdxs.get(uRange).map { mcis => Map(uRange -> mcis) }.getOrElse(Map()))
      case leaf @ TypeValueLeaf(ident, _, _) =>
        val myParam = myParams.get(leafIdx).filter { p => myParamAppIdxs.get(leafIdx).map { pai => myLeafParamAppIdxs.getOrElse(p, Set()).contains(pai) }.getOrElse(false) }
        val isOtherLeaf = otherLeafIdxs.contains(leafIdx)
        val vLoc = CounterGraphLocation(TypeValueRange(leafIdx, leafIdx), isSupertype)
        if(otherLeafIdxSets.contains(leafIdx) && (isOtherLeaf || myParam.isDefined || ident.isTypeParamAppIdentity)) {
          val canAddIdent = isOtherLeaf && !ident.isTypeParamAppIdentity && !myParam.isDefined
          val edges2 = otherLeafIdxSets.get(leafIdx).toSet.flatMap {
            _.map { i => (vLoc, CounterGraphLocation(TypeValueRange(i, i), !isSupertype)) }
          }
          tuple.copy(
              _1 = tuple._1.withCount(vLoc, 1),
              _2 = tuple._2 ++ edges2,
              _3 = if(canAddIdent) tuple._3 + (leafIdx -> ident) else tuple._3,
              _5 = tuple._5 + (leafIdx -> (tuple._5.getOrElse(leafIdx, Seq()) ++ ((myParam |@| myParamAppIdxs.get(leafIdx)) { TypeParamCondition(_, _, leaf, if(isSupertype) TypeMatching.TypeWithSupertype else TypeMatching.SupertypeWithType) }))),
              _6 = tuple._6 + (leafIdx -> ident),
              _7 = tuple._7 | (if(ident.isTypeParamAppIdentity || myParam.isDefined) Set[Int](leafIdx) else Set[Int]()))
        } else
          tuple.copy(_1 = tuple._1.withCount(vLoc, 0))
      case globalTypeAppNode: GlobalTypeAppNode[T] =>
        val node2 = globalTypeAppNode.typeValueBranchOrTypeValueLeaf(isSupertype, isSupertype, canExpandGlobalType)
        generateCounterGraphForTypeDisjunction(indexTuple, node2, isSupertype, canExpandGlobalType)(leafIdx)(tuple)
    }
  }
  
  private def counterGraphWithTwoLeafEdgeSets(graph: CounterGraph[CounterGraphLocation], edges1: Set[(CounterGraphLocation, CounterGraphLocation)], edges2: Set[(CounterGraphLocation, CounterGraphLocation)], conjMyParamIdxs: Set[Int], disjMyParamIdxs: Set[Int]) = {
    val swappedEdges2 = edges2.map { _.swap }
    val edges = edges1 | swappedEdges2
    val myParamEdges = edges.flatMap {
      case edge @ (CounterGraphLocation(vRange, _), CounterGraphLocation(uRange, _)) =>
        if((vRange.minIdx == vRange.maxIdx && conjMyParamIdxs.contains(vRange.minIdx)) || (uRange.minIdx == uRange.maxIdx && disjMyParamIdxs.contains(uRange.minIdx)))
          Set(edge)
        else
          Set[(CounterGraphLocation, CounterGraphLocation)]()
    }
    val intersectedEdges = (edges1 & swappedEdges2) | myParamEdges
    val otherEdges = edges &~ intersectedEdges
    val otherVLocs = otherEdges.flatMap { p => Set(p._1, p._2) } -- intersectedEdges.flatMap { p => Set(p._1, p._2) }
    val graph2 = intersectedEdges.foldLeft(graph) {
      (newGraph, edge) => newGraph.withTwoEdges(edge._1, edge._2)
    }
    otherVLocs.foldLeft(graph2) { _.withCount(_, 0) }
  }
  
  private def fullyCheckOrDistributeSupertypeConjunctionNode[T](node: TypeValueNode[T], nodeTuple: NodeTupleT[T], depthRangeSets: List[TypeValueRangeSet[T]], args: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isSupertype: Boolean, canExpandGlobalType: Boolean) =
    checkOrDistributeSupertypeConjunctionNode(node, nodeTuple, depthRangeSets, args, isSupertype, true, canExpandGlobalType)(0)(-1)._2 match {
      case pairs @ List((optRangeSet, newNode)) => some((optRangeSet, LogicalTypeValueTerm(newNode, args)))
      case _                                    => none
    }
  
  private def fullyCheckOrDistributeSupertypeDisjunctionNode[T](node: TypeValueNode[T], nodeTuple: NodeTupleT[T], depthRangeSets: List[TypeValueRangeSet[T]], args: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isSupertype: Boolean, canExpandGlobalType: Boolean) =
    checkOrDistributeSupertypeDisjunctionNode(node, nodeTuple, depthRangeSets, args, isSupertype, true, canExpandGlobalType)(0)(-1)._2 match {
      case pairs @ List((optRangeSet, newNode)) => some((optRangeSet, LogicalTypeValueTerm(newNode, args)))
      case _                                    => none
    }
  
  private def checkOrDistributeTypeValueNodesFromLogicalTypeValueTerms[T](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T], isFirstTry: Boolean, canExpandGlobalType: Boolean) =
    (term1.conjNode, term2.conjNode) match {
      case (TypeValueBranch(childs1, tupleTypes1, _), TypeValueBranch(childs2, tupleTypes2, _)) if childs1.size > 1 && childs2.size === 1 && !isFirstTry =>
        val tmpTerm1 = LogicalTypeValueTerm(TypeValueBranch(Vector(TypeValueBranch(Vector(term1.conjNode), tupleTypes1, term1.conjNode.leafCount)), Nil, term1.conjNode.leafCount), term1.args)
        val tmpTerm2 = term2
        val normalizedTerm1 = tmpTerm1.normalizedTypeValueNodeForChecking(true, canExpandGlobalType)
        val normalizedTerm2 = tmpTerm2.normalizedTypeValueNodeForChecking(false, canExpandGlobalType)
        val conjDepthRangeSets = TypeValueRangeSet.full[T] :: normalizedTerm2.typeInfo.conjDepthRangeSets
        val disjDepthRangeSets = TypeValueRangeSet.full[T] :: TypeValueRangeSet.full[T] :: normalizedTerm1.supertypeInfo.disjDepthRangeSets
        val nodeTuple2 = (normalizedTerm2.typeInfo.conjRangeSets, normalizedTerm2.typeInfo.allParams, normalizedTerm2.typeInfo.fieldSetTypeIdents)
        val nodeTuple1 = (normalizedTerm1.supertypeInfo.disjRangeSets, normalizedTerm1.supertypeInfo.allParams, normalizedTerm1.supertypeInfo.fieldSetTypeIdents)
        for {
          pair1 <- fullyCheckOrDistributeSupertypeConjunctionNode(normalizedTerm1.conjNode, nodeTuple2, conjDepthRangeSets, normalizedTerm1.args, true, canExpandGlobalType)
          pair2 <- fullyCheckOrDistributeSupertypeDisjunctionNode(normalizedTerm2.conjNode, nodeTuple1, disjDepthRangeSets, normalizedTerm2.args, false, canExpandGlobalType)
        } yield ((pair1, normalizedTerm1), (pair2, normalizedTerm2))
      case _ =>
        val normalizedTerm1 = term1.normalizedTypeValueNodeForChecking(true, canExpandGlobalType)
        val normalizedTerm2 = term2.normalizedTypeValueNodeForChecking(false, canExpandGlobalType)
        val conjDepthRangeSets = TypeValueRangeSet.full[T] :: normalizedTerm2.typeInfo.conjDepthRangeSets
        val disjDepthRangeSets = TypeValueRangeSet.full[T] :: TypeValueRangeSet.full[T] :: normalizedTerm1.supertypeInfo.disjDepthRangeSets
        val nodeTuple2 = (normalizedTerm2.typeInfo.conjRangeSets, normalizedTerm2.typeInfo.allParams, normalizedTerm2.typeInfo.fieldSetTypeIdents)
        val nodeTuple1 = (normalizedTerm1.supertypeInfo.disjRangeSets, normalizedTerm1.supertypeInfo.allParams, normalizedTerm1.supertypeInfo.fieldSetTypeIdents)
        for {
          pair1 <- fullyCheckOrDistributeSupertypeConjunctionNode(normalizedTerm1.conjNode, nodeTuple2, conjDepthRangeSets, normalizedTerm1.args, true, canExpandGlobalType)
          pair2 <- fullyCheckOrDistributeSupertypeDisjunctionNode(normalizedTerm2.conjNode, nodeTuple1, disjDepthRangeSets, normalizedTerm2.args, false, canExpandGlobalType)
        } yield ((pair1, normalizedTerm1), (pair2, normalizedTerm2))
    }
  
  private def checkTypeValueNodesFromLogicalTypeValueTerms[T](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T], isFirstTry: Boolean, canExpandGlobalType: Boolean) =
    checkOrDistributeTypeValueNodesFromLogicalTypeValueTerms(term1, term2, isFirstTry, canExpandGlobalType: Boolean).map {
      case (((optRangeSet1, distributedTerm1), undistributedTerm1), ((optRangeSet2, distributedTerm2), undistributedTerm2)) =>
        val conjDepthRangeSets = TypeValueRangeSet.full[T] :: distributedTerm2.typeInfo.conjDepthRangeSets
        val disjDepthRangeSets = TypeValueRangeSet.full[T] :: TypeValueRangeSet.full[T] :: distributedTerm1.supertypeInfo.disjDepthRangeSets
        val nodeTuple2 = (distributedTerm2.typeInfo.conjRangeSets, distributedTerm2.typeInfo.allParams, distributedTerm2.typeInfo.fieldSetTypeIdents)
        val nodeTuple1 = (distributedTerm1.supertypeInfo.disjRangeSets, distributedTerm1.supertypeInfo.allParams, distributedTerm1.supertypeInfo.fieldSetTypeIdents)
        val optRangeSetPair = (optRangeSet1 |@| optRangeSet2) { (_, _) }
        val conjPair = optRangeSetPair.map { p => (undistributedTerm1, p._1) }.getOrElse {
          (distributedTerm1, checkSupertypeConjunctionNode(distributedTerm1.conjNode, nodeTuple2, conjDepthRangeSets, distributedTerm1.args, true, canExpandGlobalType)(0)(-1)._2)
        }
        val disjPair = optRangeSetPair.map { p => (undistributedTerm2, p._2) }.getOrElse {
          (distributedTerm2, checkSupertypeDisjunctionNode(distributedTerm2.conjNode, nodeTuple1, disjDepthRangeSets, distributedTerm2.args, false, canExpandGlobalType)(0)(-1)._2)
        }
        (conjPair, disjPair)
    }
    
  private def morePartiallyMatchesSupertypeValueTermWithTypeValueTerm[T](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T], isFirstTry: Boolean, canExpandGlobalType: Boolean) =
    checkTypeValueNodesFromLogicalTypeValueTerms(term1, term2, isFirstTry, canExpandGlobalType).flatMap {
      case ((distributedTerm1, conjRangeSet), (distributedTerm2, disjRangeSet)) =>
        if(!conjRangeSet.isEmpty && !disjRangeSet.isEmpty) {
          val disjOtherLeafIdxs = disjRangeSet.value.otherLeafIdxs.toSet
          val conjOtherLeafIdxSets = conjRangeSet.value.leafIdxPairs.toSet.foldLeft(Map[Int, Set[Int]]()) {
            case (is, (i, j)) => is |+| Map(i -> Set(j))
          }
          val conjMyParamAppIdxs = conjRangeSet.value.myParamAppIdxs.toSeq.toMap
          val conjMyParams = conjRangeSet.value.myParams.toSeq.toMap
          val disjMyLeafParamAppIdxs = disjRangeSet.value.myLeafParamAppIdxs.foldLeft(Map[Int, Set[Int]]()) { case (pais, (p, pai)) => pais |+| Map(p -> Set(pai)) }
          val conjOtherLeafIdxs = conjRangeSet.value.otherLeafIdxs.toSet
          val disjOtherLeafIdxSets = disjRangeSet.value.leafIdxPairs.toSet.foldLeft(Map[Int, Set[Int]]()) {
            case (is, (i, j)) => is |+| Map(i -> Set(j))
          }
          val disjMyParamAppIdxs = disjRangeSet.value.myParamAppIdxs.toSeq.toMap
          val disjMyParams = disjRangeSet.value.myParams.toSeq.toMap
          val conjMyLeafParamAppIdxs = conjRangeSet.value.myLeafParamAppIdxs.foldLeft(Map[Int, Set[Int]]()) { case (pais, (p, pai)) => pais |+| Map(p -> Set(pai)) }
          val pairs = conjRangeSet.value.conds.toVector
          val conjMyCondIdxs = pairs.zipWithIndex.foldLeft(Map[TypeValueRange, Set[Int]]()) { 
            case (condIdxs, (((myRange, _), _), i)) => condIdxs |+| Map(myRange -> Set(i))
          }
          val conjOtherCondIdxs = pairs.zipWithIndex.foldLeft(Map[TypeValueRange, Int]()) {
            case (condIdxs, (((_, otherRanges), _), i)) => condIdxs ++ otherRanges.map { _ -> i }.toMap
          }
          val (graph, conjEdges, conjIdents, conjCondIdxSets, conjParamCondLists, conjIdents2, conjMyParamIdxs) = generateCounterGraphForTypeConjunction((disjOtherLeafIdxs, conjOtherLeafIdxSets, conjMyCondIdxs, Map(), conjMyParams, conjMyParamAppIdxs, disjMyLeafParamAppIdxs), distributedTerm1.conjNode, true, canExpandGlobalType)(0)((CounterGraph.empty, Set(), Map(), Map(), Map(), Map(), Set()))
          val (graph2, disjEdges, disjIdents, disjCondIdxSets, disjParamCondLists, disjIdents2, disjMyParamIdxs) = generateCounterGraphForTypeDisjunction((conjOtherLeafIdxs, disjOtherLeafIdxSets, Map(), conjOtherCondIdxs, disjMyParams, disjMyParamAppIdxs, conjMyLeafParamAppIdxs), distributedTerm2.conjNode, false, canExpandGlobalType)(0)((graph, Set(), Map(), Map(), Map(), Map(), Set()))
          val graph3 = counterGraphWithTwoLeafEdgeSets(graph2, conjEdges, disjEdges, conjMyParamIdxs, disjMyParamIdxs)
          graph3.decreaseCounters.flatMap {
            graph4 =>
              val vLoc = CounterGraphLocation(TypeValueRange(0, distributedTerm1.conjNode.leafCount - 1), true)
              val uLoc = CounterGraphLocation(TypeValueRange(0, distributedTerm2.conjNode.leafCount - 1), false)
              (for(v <- graph4.vertices.get(vLoc); u <- graph4.vertices.get(uLoc)) yield (v, u)).flatMap {
                case (v, u) =>
                  if(v.count > 0 && u.count > 0) {
                    val conjIdents3 = conjIdents.flatMap {
                      case (idx, ident) =>
                        val tLoc = CounterGraphLocation(TypeValueRange(idx, idx), true)
                        if(graph4.vertices.get(tLoc).map { _.count > 0 }.getOrElse(false)) Set(ident) else Set[TypeValueIdentity[T]]()
                    }.toSet
                    val disjIdents3 = disjIdents.flatMap {
                      case (idx, ident) =>
                        val tLoc = CounterGraphLocation(TypeValueRange(idx, idx), false)
                        if(graph4.vertices.get(tLoc).map { _.count > 0 }.getOrElse(false)) Set(ident) else Set[TypeValueIdentity[T]]()
                    }.toSet
                    val conjCondIdxs = conjCondIdxSets.flatMap {
                      case (range, condIdxs) =>
                        val tLoc = CounterGraphLocation(range, true)
                        if(graph4.vertices.get(tLoc).map { _.count > 0 }.getOrElse(false)) condIdxs else Set[Int]()
                    }.toSet
                    val disjCondIdxs = disjCondIdxSets.flatMap {
                      case (range, condIdxs) =>
                        val tLoc = CounterGraphLocation(range, false)
                        if(graph4.vertices.get(tLoc).map { _.count > 0 }.getOrElse(false)) condIdxs else Set[Int]()
                    }.toSet
                    val conjParamConds = conjParamCondLists.flatMap {
                      case (idx, paramConds) =>
                        val tLoc = CounterGraphLocation(TypeValueRange(idx, idx), true)
                        if(graph4.vertices.get(tLoc).map { _.count > 0 }.getOrElse(false)) paramConds else Seq()
                    }.toSeq
                    val disjParamConds = disjParamCondLists.flatMap {
                      case (idx, paramConds) =>
                        val tLoc = CounterGraphLocation(TypeValueRange(idx, idx), false)
                        if(graph4.vertices.get(tLoc).map { _.count > 0 }.getOrElse(false)) paramConds else Seq()
                    }.toSeq
                    val conjIdents4 = conjIdents2.flatMap {
                      case (idx, ident) =>
                        val tLoc = CounterGraphLocation(TypeValueRange(idx, idx), true)
                        if(graph4.vertices.get(tLoc).map { _.count > 0 }.getOrElse(false)) Set(ident) else Set[TypeValueIdentity[T]]()
                    }.toSet
                    val disjIdents4 = disjIdents2.flatMap {
                      case (idx, ident) =>
                        val tLoc = CounterGraphLocation(TypeValueRange(idx, idx), false)
                        if(graph4.vertices.get(tLoc).map { _.count > 0 }.getOrElse(false)) Set(ident) else Set[TypeValueIdentity[T]]()
                    }.toSet
                    val idents = conjIdents3 | disjIdents3
                    val conds = (conjCondIdxs | disjCondIdxs).toSeq.flatMap { pairs.lift(_).map { _._2 } }
                    val idents2 = conjIdents4 | disjIdents4
                    val paramConds1 = conjParamConds ++ disjParamConds
                    val params1 = paramConds1.flatMap {
                      case TypeParamCondition(param1, _, TypeValueLeaf(TypeParamAppIdentity(param2), _, _), _) => Set(param1, param2) 
                      case TypeParamCondition(param1, _, _, _) => Set(param1)               
                    }
                    val conjParamConds3 = (disjMyLeafParamAppIdxs -- params1).flatMap {
                      case (p, pais) => pais.headOption.flatMap { pai => if(idents2.contains(TypeParamAppIdentity(p))) some(TypeParamCondition[T](p, pai, TypeValueLeaf(BuiltinTypeIdentity(TypeBuiltinFunction.Any, Nil), 0, 1), TypeMatching.SupertypeWithType)) else none }
                    }
                    val disjParamConds3 = (conjMyLeafParamAppIdxs -- params1).flatMap {
                      case (p, pais) => pais.headOption.flatMap { pai => if(idents2.contains(TypeParamAppIdentity(p))) some(TypeParamCondition[T](p, pai, TypeValueLeaf(BuiltinTypeIdentity(TypeBuiltinFunction.Nothing, Nil), 0, 1), TypeMatching.TypeWithSupertype)) else none }
                    }
                    val paramConds2 = (conjParamConds3 ++ disjParamConds3).toSeq
                    some((idents, conds, (paramConds1, paramConds2)))
                  } else
                    none
              }
          }
        } else
          none
    }

  private def partiallyMatchesSupertypeValueTermWithTypeValueTerm[T](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T], isFirstTry: Boolean) =
    if(true || term1.statistics.unexpandedLeafCount * 2 > term1.statistics.expandedLeafCount && term2.statistics.unexpandedLeafCount * 2 > term2.statistics.expandedLeafCount)
      morePartiallyMatchesSupertypeValueTermWithTypeValueTerm(term1, term2, isFirstTry, true)
    else
      morePartiallyMatchesSupertypeValueTermWithTypeValueTerm(term1, term2, isFirstTry, false).orElse {
        morePartiallyMatchesSupertypeValueTermWithTypeValueTerm(term1, term2, isFirstTry, true)
      }

  private def matchesSupertypeValueTermWithTypeValueTerm[T](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T]) =
    (term1.conjNode.typeValueNodeWithoutTupleTypeValueLeaf, term2.conjNode.typeValueNodeWithoutTupleTypeValueLeaf) match {
      case (leaf1 @ TypeValueLeaf(ident1 @ BuiltinTypeIdentity(TypeBuiltinFunction.Any | TypeBuiltinFunction.Nothing, _), _, _), TypeValueLeaf(ident2 @ TypeParamAppIdentity(param2), paramAppIdx2, _)) =>
        some((Set[TypeValueIdentity[T]](), Seq(), (Seq(TypeParamCondition(param2, paramAppIdx2, leaf1, TypeMatching.TypeWithSupertype)), Nil)))
      case (leaf1 @ TypeValueLeaf(ident1 @ BuiltinTypeIdentity(TypeBuiltinFunction.Any | TypeBuiltinFunction.Nothing, _), _, _), TypeValueBranch(Seq(TypeValueLeaf(ident2 @ TypeParamAppIdentity(param2), paramAppIdx2, _)), _, _)) =>
        some((Set[TypeValueIdentity[T]](), Seq(), (Seq(TypeParamCondition(param2, paramAppIdx2, leaf1, TypeMatching.TypeWithSupertype)), Nil)))
      case (TypeValueBranch(Seq(leaf1 @ TypeValueLeaf(ident1 @ BuiltinTypeIdentity(TypeBuiltinFunction.Any | TypeBuiltinFunction.Nothing, _), _, _)), tupleTypes1, _), TypeValueBranch(Seq(TypeValueLeaf(ident2 @ TypeParamAppIdentity(param2), paramAppIdx2, _)), tupleTypes2, _)) =>
        some((Set[TypeValueIdentity[T]](), Seq(TypeValueRangeCondition(tupleTypes1, tupleTypes2.toList)), (Seq(TypeParamCondition(param2, paramAppIdx2, leaf1, TypeMatching.TypeWithSupertype)), Nil)))
      case (TypeValueLeaf(ident1 @ TypeParamAppIdentity(param1), paramAppIdx1, _), leaf2 @ TypeValueLeaf(ident2 @ BuiltinTypeIdentity(TypeBuiltinFunction.Any | TypeBuiltinFunction.Nothing, _), _, _)) =>
        some((Set[TypeValueIdentity[T]](), Seq(), (Seq(TypeParamCondition(param1, paramAppIdx1, leaf2, TypeMatching.SupertypeWithType)), Nil)))
      case (TypeValueLeaf(ident1 @ TypeParamAppIdentity(param1), paramAppIdx1, _), TypeValueBranch(Seq(leaf2 @ TypeValueLeaf(ident2 @ BuiltinTypeIdentity(TypeBuiltinFunction.Any | TypeBuiltinFunction.Nothing, _), _, _)), _, _)) =>
        some((Set[TypeValueIdentity[T]](), Seq(), (Seq(TypeParamCondition(param1, paramAppIdx1, leaf2, TypeMatching.SupertypeWithType)), Nil)))
      case (TypeValueBranch(Seq(TypeValueLeaf(ident1 @ TypeParamAppIdentity(param1), paramAppIdx1, _)), tupleTypes1, _), TypeValueBranch(Seq(leaf2 @ TypeValueLeaf(ident2 @ BuiltinTypeIdentity(TypeBuiltinFunction.Any | TypeBuiltinFunction.Nothing, _), _, _)), tupleTypes2, _)) =>
        some((Set[TypeValueIdentity[T]](), Seq(TypeValueRangeCondition(tupleTypes1, tupleTypes2.toList)), (Seq(TypeParamCondition(param1, paramAppIdx1, leaf2, TypeMatching.SupertypeWithType)), Nil)))
      case _ =>
        (term1.conjNode, term2.conjNode) match {
          case (TypeValueBranch(childs1, _, _), TypeValueBranch(childs2, _, _)) if (childs1.size > 1 && childs2.size === 1) || (childs1.size === 1 && childs2.size > 1) =>
            partiallyMatchesSupertypeValueTermWithTypeValueTerm(term1, term2, true).orElse(partiallyMatchesSupertypeValueTermWithTypeValueTerm(term1, term2, false))
          case _ =>
            partiallyMatchesSupertypeValueTermWithTypeValueTerm(term1, term2, true)
        }
    }
  
  private def matchesLocigalTypeValueTermsWithoutArgs[T, U, E](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T], typeMatching: TypeMatching.Value) =
    typeMatching match {
      case TypeMatching.Types             =>
        for {
          tuple1 <- matchesSupertypeValueTermWithTypeValueTerm(term1, term2)
          tuple2 <- matchesSupertypeValueTermWithTypeValueTerm(term2, term1)
        } yield {
          (tuple1._1 | tuple2._1, tuple1._2 ++ tuple2._2, (tuple1._3._1 ++ tuple2._3._2, tuple2._3._1 ++ tuple1._3._2))
        }
      case TypeMatching.SupertypeWithType =>
        matchesSupertypeValueTermWithTypeValueTerm(term1, term2).map { _.mapElements(identity, identity, identity) }
      case TypeMatching.TypeWithSupertype =>
        matchesSupertypeValueTermWithTypeValueTerm(term2, term1).map { _.mapElements(identity, identity, identity) }
    }

  private def checkTypeValueRangeConditionS[T, U, V, E](cond: TypeValueRangeCondition[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]) =
    stFoldLeftValidationS(cond.myTupleTypes)(z.success[NoType[T]]) {
      (x, myTupleType, newEnv: E) =>
        val (newEnv2, savedDelayedErrs) = envSt.delayedErrorsFromEnvironmentS(newEnv)
        val (newEnv4, (newRes, _)) = (0 until cond.otherTupleTypes.size).foldLeft(unifier.mismatchedTermErrorS(newEnv2).mapElements(identity, nt => (nt.failure[U], false))) {
          case ((newEnv3, (Failure(_), _) | (Success(_), false)), i) =>
            val otherTupleType = cond.otherTupleTypes(i)
            if(i < cond.otherTupleTypes.size - 1)
               envSt.withDelayedErrorRestoringOrSavingS(savedDelayedErrs) {
                 matchesTypeValueTermsS(myTupleType, otherTupleType)(x)(f)(_: E)
               } (newEnv3)
            else
              // last other tuple type
              matchesTypeValueTermsS(myTupleType, otherTupleType)(x)(f)(newEnv3).mapElements(identity, (_, true))
           case ((newEnv3, (newRes2, areRestoredDelayedErrs)), _)      =>
             (newEnv3, (newRes2, areRestoredDelayedErrs))
        }
        (newEnv4, newRes)
    } (env)
  
  private def checkTypeValueRangeConditionsS[T, U, V, E](conds: Seq[TypeValueRangeCondition[T]])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]) = {
    val (env2, savedTypeMatching) = envSt.currentTypeMatchingFromEnvironmentS(env)
    val (env3, _) = envSt.setCurrentTypeMatchingS(TypeMatching.SupertypeWithType)(env2)
    val (env4, res) = stFoldLeftValidationS(conds)(z.success[NoType[T]]) {
      (x, cond, newEnv: E) => checkTypeValueRangeConditionS(cond)(x)(f)(newEnv)
    } (env3)
    val (env5, _) = envSt.setCurrentTypeMatchingS(savedTypeMatching)(env4)
    (env5, res)
  }
  
  private def checkTypeParamConditionsS[T, U, V, E](paramConds: Seq[TypeParamCondition[T]], supertypeArgs: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], typeArgs: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]) = {
    stFoldLeftValidationS(paramConds)((z, Set[(Int, Int, TypeMatching.Value)]()).success[NoType[T]]) {
      (pair, paramCond, newEnv: E) =>
        val (x, paramCondTuples) = pair
        val (isChecked, optParamPair) = paramCond match {
          case TypeParamCondition(param, _, TypeValueLeaf(TypeParamAppIdentity(param2), _, _), typeMatching) =>
            val (tmpParam, tmpParam2) = if(param < param2) (param, param2) else (param2, param)
            (paramCondTuples.contains((tmpParam, tmpParam2, typeMatching)), some((param, param2)))
          case _                                                                        =>
            (false, none)
        }
        if(!isChecked) {
          val (newEnv2, savedTypeMatching) = envSt.currentTypeMatchingFromEnvironmentS(newEnv)
          val (newEnv3, _) = envSt.setCurrentTypeMatchingS(paramCond.typeMatching)(newEnv2)
          val (args1, args2) = paramCond.typeMatching match {
            case TypeMatching.SupertypeWithType => (supertypeArgs, typeArgs)
            case _                              => (typeArgs, supertypeArgs)
          }
          val (newEnv4, res) = (args1.get(TypeParamAppIdentity(paramCond.param)) |@| args2.get(paramCond.leaf.ident).orElse {
            paramCond.leaf.ident match {
              case BuiltinTypeIdentity(TypeBuiltinFunction.Any | TypeBuiltinFunction.Nothing, Nil) => some(Vector())
              case _                                                                               => none
            }
          }) {
            (paramArgs, leafArgs) =>
              val typeParamApp = TypeParamApp(paramCond.param, paramArgs, paramCond.paramAppIdx)
              paramCond.leaf.typeValueTerm(leafArgs).map {
                _.map { matchesTypeValueTermsS(typeParamApp, _)(x)(f)(newEnv3) }.getOrElse((newEnv3, x.success))
              }.getOrElse((newEnv3, NoType.fromError[T](FatalError("can't convert type value leaf to type value term", none, NoPosition)).failure))
          }.getOrElse((newEnv3, NoType.fromError[T](FatalError("not found arguments", none, NoPosition)).failure))
          val (newEnv5, _) = envSt.setCurrentTypeMatchingS(savedTypeMatching)(newEnv4)
          (newEnv5, res.map { (_, paramCondTuples ++ optParamPair.map { case (p1, p2) => (p1, p2, paramCond.typeMatching) })})
        } else
          (newEnv, pair.success)
    } (env).mapElements(identity, _.map { _._1 })
  }
  
  private def partiallyInstantiateTypeValueNodesS[T, U, E](nodes: Iterable[TypeValueNode[T]], argMap: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isConj: Boolean)(markedParams: Set[Int])(newOptNodes: Map[TypeValueIdentity[T], Option[TypeValueNode[T]]], newArgMap: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T], locEqual: Equal[T]) =
    stFoldLeftValidationS(nodes)((newOptNodes, newArgMap, Vector[TypeValueNode[T]](), Set[Int](), false).success[NoType[T]]) {
      (tuple, node, newEnv: E) =>
        val (newOptNodeMap2, newArgMap2, newNodes, newInstantiatedParams, isInstantiation) = tuple
        val (newEnv2, newRes) = partiallyInstantiateTypeValueNodeS(node, argMap, isConj, false)(markedParams)(newOptNodeMap2, newArgMap2)(newEnv)
        (newEnv2, newRes.map { 
          _ match { 
            case (newOptNodes3, newArgMap3, Some((newNode, newInstantiatedParams2))) =>
              (newOptNodes3, newArgMap3, newNodes :+ newNode, newInstantiatedParams2, true)
            case (newOptNodes3, newArgMap3, None)                                    =>
              (newOptNodes3, newArgMap3, newNodes :+ node, newInstantiatedParams, isInstantiation)
          }
        })
    } (env)
  
  private def partiallyInstantiateTypeValueNodeS[T, U, E](node: TypeValueNode[T], argMap: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isConj: Boolean, isExpandedGlobalAppIdent: Boolean)(markedParams: Set[Int])(newOptNodeMap: Map[TypeValueIdentity[T], Option[TypeValueNode[T]]], newArgMap: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T], locEqual: Equal[T]): (E, Validation[NoType[T], (Map[TypeValueIdentity[T], Option[TypeValueNode[T]]], Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], Option[(TypeValueNode[T], Set[Int])])]) =
    node match {
      case TypeValueBranch(childs, tupleTypes, leafCount) =>
        val (env2, res) = partiallyInstantiateTypeValueNodesS(childs, argMap, !isConj)(markedParams)(newOptNodeMap, newArgMap)(env)
        (env2, res.map {
          case (newOptNodeMap2, newArgMap2, childs2, instantiatedParams, isInstantiation) =>
            (newOptNodeMap2, newArgMap2, if(isInstantiation) some((TypeValueBranch(childs2, tupleTypes, childs2.foldLeft(0) { _ + _.leafCount }), instantiatedParams)) else none)
        })
      case leaf @ TypeValueLeaf(ident @ TypeParamAppIdentity(param), paramAppIdx, leafCount) =>
        newOptNodeMap.get(ident) match {
          case Some(optNode) =>
            (env, (newOptNodeMap, newArgMap, optNode.map { (_, Set[Int]()) }).success[NoType[T]])
          case None       =>
            val (env2, rootParamRes) = unifier.findRootParamS(param)(env)
            val (env6, res3) = rootParamRes match {
              case Success(rootParam) =>
                val (env3, optParamTerm) = unifier.getParamTermS(rootParam)(env2)
                optParamTerm match {
                  case Some(paramTerm) =>
                    argMap.get(ident).flatMap(leaf.typeValueTerm) match {
                      case Some(Some(term)) =>
                        val (env4, res) = partiallyInstantiateTypeValueTermForMarkedParamsS(term)(Set())(unifier.mismatchedTermErrorS)(env)
                        res match {
                          case Success((term2, optInstantiatedParam)) =>
                            val (env5, res2) = logicalTypeValueTermFromTypeValueTermS(term2)(env4)
                            res2 match {
                              case Success(LogicalTypeValueTerm(conjNode2 @ TypeValueLeaf(ident2, _, _), argMap2)) =>
                                (env5, (newOptNodeMap + (ident -> some(conjNode2)), newArgMap ++ argMap2, some((conjNode2, optInstantiatedParam))).success)
                              case Success(LogicalTypeValueTerm(conjNode2, argMap2))                               =>
                                val leafIdents = newArgMap.keySet & argMap2.keySet
                                if(leafIdents.forall { i => (newArgMap.get(i) |@| argMap2.get(i)) { TypeValueLambda.simplyMatchesTypeValueLambdaLists(_, _) }.getOrElse(false) })
                                  (env5, (newOptNodeMap + (ident -> some(conjNode2)), newArgMap ++ argMap2, some((conjNode2, optInstantiatedParam))).success)
                                else
                                  (env5, NoType.fromError[T](Error("same type functions don't have same arguments at logical type expression", none, NoPosition)).failure)
                              case Failure(noType) =>
                                (env5, noType.failure)
                            }
                          case Failure(noType) =>
                            (env4, noType.failure)
                        }
                      case Some(None) | None =>
                        (env3, NoType.fromError[T](FatalError("not found arguments", none, NoPosition)).failure)
                    }
                  case None =>
                    (env3, argMap.get(ident).map {
                      args =>
                        (newOptNodeMap + (ident -> none), newArgMap + (ident -> args), none).success
                    }.getOrElse(NoType.fromError[T](FatalError("no type arguments", none, NoPosition)).failure))
                }
              case Failure(noType) =>
                (env2, noType.failure)
            }
            val res4 = res3.map { 
              case (newNodes2, newArgMap2, Some((branch: TypeValueBranch[T], optInstantiatedParam))) if !isConj =>
                (newNodes2, newArgMap2, some((TypeValueBranch(Vector(branch), Vector(), branch.leafCount).normalizedTypeValueNode, optInstantiatedParam)))
              case tuple                                                                                        =>
                tuple
            }
            res4 match {
              case Success((newNodeMap2, newArgMap2, Some((node2, optInstantiatedParam)))) =>
                val (env7, res5) = partiallyInstantiateTypeValueNodeS(node2, newArgMap2, isConj, false)(markedParams ++ optInstantiatedParam)(newNodeMap2, newArgMap2)(env6)
                (env7, res5.map {
                  case (newNodes3, newArgMap3, optPair) =>
                    (newNodes3, newArgMap3, optPair.orElse(some((node2, optInstantiatedParam.toSet))))
                })
              case Success((newNodes2, newArgMap2, None))                                  =>
                (env6, (newNodes2, newArgMap2, none).success)
              case Failure(noType)                                                         =>
                (env6, noType.failure)
            }
        }
      case TypeValueLeaf(ident, _, _) =>
        val optIdent2 = ident match {
          case UnexpandedGlobalTypeAppIdentity(loc, sym) if isExpandedGlobalAppIdent =>
            some(ExpandedGlobalTypeAppIdentity(loc, sym))
          case _                                                                     =>
            none
        }
        (env, argMap.get(ident).map {
          args =>
            (newOptNodeMap + (ident -> none), (newArgMap + (ident -> args)) ++ optIdent2.map { _ -> args }, none).success
        }.getOrElse(NoType.fromError[T](FatalError("no type arguments", none, NoPosition)).failure))
      case GlobalTypeAppNode(loc, childs, tupleTypes, leafCount, sym) =>
        val (env2, res) = partiallyInstantiateTypeValueNodeS(TypeValueLeaf(UnexpandedGlobalTypeAppIdentity(loc, sym), 0, leafCount), argMap, isConj, true)(markedParams)(newOptNodeMap, newArgMap)(env)
        res match {
          case Success((newOptNodeMap2, newArgMap2, Some((TypeValueLeaf(UnexpandedGlobalTypeAppIdentity(loc2, sym2), _, _), instantiatedParams2)))) =>
            val (env3, res2) = partiallyInstantiateTypeValueNodeS(TypeValueBranch(childs, tupleTypes, leafCount - 1), argMap, true, false)(markedParams ++ instantiatedParams2)(newOptNodeMap2, newArgMap2)(env2)
            (env3, res2.flatMap {
              case (newOptNodeMap3, newArgMap3, Some((TypeValueBranch(childs2, tupleTypes2, leafCount2), instantiatedParams3))) =>
                (newOptNodeMap3, newArgMap3, some((GlobalTypeAppNode(loc2, childs2, tupleTypes2, leafCount2 + 1, sym2), instantiatedParams3))).success
              case tuple @ (newOptNodeMap3, newArgMap3, None)                                                                   =>
                tuple.success
              case _                                                                                                            =>
                NoType.fromError(FatalError("incorrect type value node", none, NoPosition)).failure
            })
          case Success((newOptNodeMap2, newArgMap2, optPair @ None)) =>
            val (env3, res2) = partiallyInstantiateTypeValueNodeS(TypeValueBranch(childs, tupleTypes, leafCount - 1), argMap, true, false)(markedParams)(newOptNodeMap2, newArgMap2)(env2)
            (env3, res2.flatMap {
              case (newOptNodeMap3, newArgMap3, Some((TypeValueBranch(childs2, tupleTypes2, leafCount2), instantiatedParams2))) =>
                (newOptNodeMap3, newArgMap3, some((GlobalTypeAppNode(loc, childs2, tupleTypes2, leafCount2 + 1, sym), instantiatedParams2))).success
              case tuple @ (newOptNodeMap3, newArgMap3, None)                                                                   =>
                tuple.success
              case _                                                                                                            =>
                NoType.fromError(FatalError("incorrect type value node", none, NoPosition)).failure
            })
          case Success(_)                                                                                                                           =>
            (env2, NoType.fromError(FatalError("incorrect type value node", none, NoPosition)).failure)
          case Failure(noType)                                                                                                                      =>
            (env2, noType.failure)
        }
    }
    
  private def partiallyInstantiateLogicalTypeValueTermS[T, U, E](term: LogicalTypeValueTerm[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T], locEqual: Equal[T]) =
    if(term.args.keys.exists { _.isInstanceOf[TypeParamAppIdentity[T]] }) {
      val (env2, res) = partiallyInstantiateTypeValueNodeS(term.conjNode, term.args, true, false)(Set())(Map(), Map())(env)
      (env2, res.map { t => t._3.map { case (n, ips) => (LogicalTypeValueTerm(n, t._2), ips) }.getOrElse((term, Set[Int]())) })
    } else
      (env, (term, Set[Int]()).success)
  
  private def matchesLogicalTypeValueTermsWithoutInstantationS[T, U, V, E](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]) = {
    val (env2, typeMatching) = envSt.currentTypeMatchingFromEnvironmentS(env)
    val (env3, termKindRes1) = envSt.inferTypeValueTermKindS(term1)(env2)
    val (env4, termKindRes2) = envSt.inferTypeValueTermKindS(term2)(env3)
    val (env5, retKindRes) = envSt.unifyKindsS(termKindRes1.valueOr { _.toNoKind }, termKindRes2.valueOr { _.toNoKind })(env4)
    retKindRes.map {
      retKind =>
        matchesLocigalTypeValueTermsWithoutArgs(term1, term2, typeMatching).map {
          case (idents, conds, (paramConds1, paramConds2)) =>
            val idents2 = (idents - BuiltinTypeIdentity(TypeBuiltinFunction.Any, Nil)) - BuiltinTypeIdentity(TypeBuiltinFunction.Nothing, Nil)
            st(for {
              x3 <- steS({
                stFoldLeftValidationS(idents2)(z.success[NoType[T]]) {
                  (x, ident, newEnv: E) =>
                    (term1.args.get(ident) |@| term2.args.get(ident)) {
                      (args1, args2) =>
                        if(args1.size === args2.size)
                          stFoldLeftValidationS(args1.zip(args2))(x.success[NoType[T]]) {
                            (x2, argPair, newEnv2: E) =>
                              val (arg1, arg2) = argPair
                              matchesTypeValueLambdasS(arg1, arg2)(x2)(f)(newEnv2)
                          } (newEnv)
                        else
                          unifier.mismatchedTermErrorS(newEnv).mapElements(identity, _.failure)
                    }.getOrElse(unifier.mismatchedTermErrorS(newEnv).mapElements(identity, _.failure))
                }
              })
              x4 <- steS(checkTypeValueRangeConditionsS(conds)(x3)(f)(_: E))
              x5 <- steS(checkTypeParamConditionsS(paramConds1, term1.args, term2.args)(x4)(f)(_: E))
              y <- steS(checkTypeParamConditionsS(paramConds2, term2.args, term1.args)(x5)(f)(_: E))
              _ <- rsteS(envSt.setReturnKindS(retKind)(_: E))
            } yield y).run(env5)
        }.getOrElse(unifier.mismatchedTermErrorS(env5).mapElements(identity, _.failure))
    }.valueOr { nt => (env5, nt.failure) }
  }
  
  private def matchesLogicalTypeValueTermsWithoutInstantationForTypeParamAppWithTupleTypesS[T, U, V, E](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]): (E, Validation[NoType[T], U]) = {
    val (env2, typeMatching) = envSt.currentTypeMatchingFromEnvironmentS(env) 
    typeMatching match {
      case TypeMatching.Types             =>
        val (env3, _) = envSt.setCurrentTypeMatchingS(TypeMatching.SupertypeWithType)(env2)
        matchesLogicalTypeValueTermsWithoutInstantationForTypeParamAppWithTupleTypesS(term1, term2)(z)(f)(env3) match {
          case (env4, Success(x))      =>
            val (env5, _) = envSt.setCurrentTypeMatchingS(TypeMatching.TypeWithSupertype)(env4)
            matchesLogicalTypeValueTermsWithoutInstantationForTypeParamAppWithTupleTypesS(term1, term2)(x)(f)(env5)
          case (env4, Failure(noType)) =>
            (env4, noType.failure)
        }
      case TypeMatching.SupertypeWithType =>
        (term1, term2) match {
          case (LogicalTypeValueTerm(conjNode1, _), LogicalTypeValueTerm(conjNode2, args2)) =>
            (conjNode1.typeValueNodeWithoutTupleTypeValueLeaf, conjNode2.typeValueNodeWithoutTupleTypeValueLeaf) match {
              case (TypeValueLeaf(_, _, _) | TypeValueBranch(_, Seq(), _), TypeValueBranch(Seq(TypeValueLeaf(ident2 @ TypeParamAppIdentity(param2), paramAppIdx2, _)), tupleTypes, _)) if !tupleTypes.isEmpty =>
                term1.normalizedTypeValueTerm match {
                  case Some(normalizedTerm1) =>
                    args2.get(ident2) match {
                      case Some(argLambdas) =>
                        matchesTypeValueTermsS(normalizedTerm1, TypeParamApp(param2, argLambdas, paramAppIdx2))(z)(f)(env2)
                      case None             =>
                        (env2, NoType.fromError[T](FatalError("not found arguments", none, NoPosition)).failure)
                    }
                  case None                  =>
                    (env2, NoType.fromError(FatalError("can't normalize type value term", none, NoPosition)).failure)
                }
              case _ =>
                matchesLogicalTypeValueTermsWithoutInstantationS(term1, term2)(z)(f)(env2)
            }
        }
      case TypeMatching.TypeWithSupertype =>
        val (env3, _) = reverseTypeMatchingS(env2)
        matchesLogicalTypeValueTermsWithoutInstantationForTypeParamAppWithTupleTypesS(term2, term1)(z)(f)(env3)
    }
  }

  def matchesLogicalTypeValueTermsS[T, U, V, E](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]) =
    st(for {
      pair1 <- steS(partiallyInstantiateLogicalTypeValueTermS(term1)(_: E))
      pair2 <- steS(partiallyInstantiateLogicalTypeValueTermS(term2)(_: E))
      y <- steS({
        (env2: E) =>
          val (instantiatedTerm1, instantiatedParams1) = pair1
          val (instantiatedTerm2, instantiatedParams2) = pair2
          envSt.withInfinityCheckingS(instantiatedParams1 ++ instantiatedParams2) {
            matchesLogicalTypeValueTermsWithoutInstantationForTypeParamAppWithTupleTypesS(instantiatedTerm1, instantiatedTerm2)(z)(f)(_: E)
          } (env2)
      })
    } yield y).run(env)
  
  private def replaceTypeParamsFromTypeValueNodesS[T, U, E](nodes: Iterable[TypeValueNode[T]], argMap: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isConj: Boolean)(newNodeMap: Map[TypeValueIdentity[T], TypeValueNode[T]], newArgMap: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]])(f: (Int, E) => (E, Validation[NoType[T], Either[Int, TypeValueTerm[T]]]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T], locEqual: Equal[T]) =
    stFoldLeftValidationS(nodes)((newNodeMap, newArgMap, Vector[TypeValueNode[T]]()).success[NoType[T]]) {
      (tuple, node, newEnv: E) =>
        val (newNodeMap2, newArgMap2, newNodes) = tuple
        val (newEnv2, newRes) = replaceTypeValueNodeParamsS(node, argMap, isConj, false)(newNodeMap2, newArgMap2)(f)(newEnv)
        (newEnv2, newRes.map { _.mapElements(identity, identity, newNodes :+ _) })
    } (env)
    
  def replaceTypeValueNodeParamsS[T, U, E](node: TypeValueNode[T], argMap: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isConj: Boolean, isExpandedGlobalAppIdent: Boolean)(newNodeMap: Map[TypeValueIdentity[T], TypeValueNode[T]], newArgMap: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]])(f: (Int, E) => (E, Validation[NoType[T], Either[Int, TypeValueTerm[T]]]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T], locEqual: Equal[T]): (E, Validation[NoType[T], (Map[TypeValueIdentity[T], TypeValueNode[T]], Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], TypeValueNode[T])]) =
    node match {
      case TypeValueBranch(childs, tupleTypes, leafCount) =>
        val (env2, res) = replaceTypeParamsFromTypeValueNodesS(childs, argMap, !isConj)(newNodeMap, newArgMap)(f)(env)
        res match {
          case Success((newNodeMap2, newArgMap2, childs2)) =>
            val (env3, res2) = replaceTypeParamsFromTupleTypesS(tupleTypes)(f)(env2)
            (env3, res2.map { tts => (newNodeMap2, newArgMap2, TypeValueBranch(childs2, tts, childs2.foldLeft(0) { _ + _.leafCount })) })
          case Failure(noType)                             =>
            (env2, noType.failure)
        }
      case leaf @ TypeValueLeaf(ident, _, _) =>
        val (env4, res3) = newNodeMap.get(ident) match {
          case Some(node) =>
            (env, (newNodeMap, newArgMap, node).success)
          case None       =>
            ident match {
              case UnexpandedGlobalTypeAppIdentity(loc, sym) =>
                argMap.get(ident) match {
                  case Some(args) =>
                    val (env2, res) = replaceTypeParamsFromTypeValueLambdasS(args)(f)(env)
                    val optIdent2 = if(isExpandedGlobalAppIdent) some(ExpandedGlobalTypeAppIdentity(loc, sym)) else none
                    (env2, res.map {
                      args2 => (newNodeMap + (ident -> leaf), (newArgMap + (ident -> args2)) ++ optIdent2.map { _ -> args2 }, leaf)
                    })
                  case None       =>
                    (env, NoType.fromError[T](FatalError("not found arguments", none, NoPosition)).failure)
                }
              case _                                     =>
                argMap.get(ident).flatMap(leaf.typeValueTerm) match {
                  case Some(Some(term)) =>
                    val (env2, res) = replaceTypeValueTermParamsS(term)(f)(env)
                    res match {
                      case Success(term2) =>
                        val (env3, res2) = logicalTypeValueTermFromTypeValueTermS(term2)(env2)
                        res2 match {
                          case Success(LogicalTypeValueTerm(conjNode2 @ TypeValueLeaf(ident2, _, _), argMap2)) =>
                            (env3, (newNodeMap + (ident -> conjNode2), newArgMap ++ argMap2, conjNode2).success)
                          case Success(LogicalTypeValueTerm(conjNode2, argMap2))                               =>
                            val leafIdents = newArgMap.keySet & argMap2.keySet
                            if(leafIdents.forall { i => (newArgMap.get(i) |@| argMap2.get(i)) { TypeValueLambda.simplyMatchesTypeValueLambdaLists(_, _) }.getOrElse(false) })
                              (env3, (newNodeMap + (ident -> conjNode2), newArgMap ++ argMap2, conjNode2).success)
                            else
                              (env3, NoType.fromError[T](Error("same type functions don't have same arguments at logical type expression", none, NoPosition)).failure)
                          case Failure(noType) =>
                            (env3, noType.failure)
                        }
                      case Failure(noType) =>
                        (env2, noType.failure)
                    }
                  case Some(None) =>
                    (env, (newNodeMap + (ident -> node), newArgMap + (ident -> Vector()), node).success)
                  case None =>
                    (env, NoType.fromError[T](FatalError("not found arguments", none, NoPosition)).failure)
                }
            }
        }
        (env4, res3.map { 
          case (newNodeMap2, newArgMap2, branch: TypeValueBranch[T]) if !isConj =>
            (newNodeMap2, newArgMap2, TypeValueBranch(Vector(branch), Vector(), branch.leafCount).normalizedTypeValueNode)
          case tuple                                                            =>
            tuple
        })
      case GlobalTypeAppNode(loc, childs, tupleTypes, leafCount, sym) =>
        val (env2, res) = replaceTypeValueNodeParamsS(TypeValueLeaf(UnexpandedGlobalTypeAppIdentity(loc, sym), 0, leafCount), argMap, isConj, true)(newNodeMap, newArgMap)(f)(env)
        res match {
          case Success((newNodeMap2, newArgMap2, TypeValueLeaf(UnexpandedGlobalTypeAppIdentity(loc2, sym2), _, _))) =>
            val (env3, res2) = replaceTypeValueNodeParamsS(TypeValueBranch(childs, tupleTypes, leafCount - 1), argMap, true, false)(newNodeMap2, newArgMap2)(f)(env2)
            (env3, res2.flatMap { 
              case (newNodeMap3, newArgMap3, TypeValueBranch(childs2, tupleTypes2, leafCount2)) =>
                (newNodeMap3, newArgMap3, GlobalTypeAppNode(loc2, childs2, tupleTypes2, leafCount2 + 1, sym2)).success
              case _                                                                            =>
                NoType.fromError(FatalError("incorrect type value node", none, NoPosition)).failure
            })
          case Success(_)                                                                                        =>
            (env2, NoType.fromError(FatalError("incorrect type value node", none, NoPosition)).failure)
          case Failure(noType)                                                                                   =>
            (env2, noType.failure)
        }
    }
  
  def replaceLogicalTypeValueTermParamsS[T, U, E](term: LogicalTypeValueTerm[T])(f: (Int, E) => (E, Validation[NoType[T], Either[Int, TypeValueTerm[T]]]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T], locEqual: Equal[T]): (E, Validation[NoType[T], TypeValueTerm[T]]) = {
    val (env2, res) = replaceTypeValueNodeParamsS(term.conjNode, term.args, true, false)(Map(), Map())(f)(env)
    (env2, res.map { case (_, args2, conjNode2) => LogicalTypeValueTerm(conjNode2, args2) })
  }
  
  private def unsafeAllocateTypeParamsFromTypeValueNodesS[T, U, E](nodes: Iterable[TypeValueNode[T]])(allocatedParams: Map[Int, Int], unallocatedParamAppIdx: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    stFoldLeftValidationS(nodes)((allocatedParams, Set[Int](), Set[Int](), Vector[TypeValueNode[T]]()).success[NoType[T]]) {
      (tuple, node, newEnv: E) =>
        val (newAllocatedParams, newAllocatedArgParams, allocatedParamAppIdxs, newNodes) = tuple
        val (newEnv2, newRes) = unsafeAllocateTypeValueNodeParamsS(node)(newAllocatedParams, unallocatedParamAppIdx)(newEnv)
        (newEnv2, newRes.map { 
          _.mapElements(identity, newAllocatedArgParams | _, allocatedParamAppIdxs | _, newNodes :+ _)
        })
    } (env)
  
  def unsafeAllocateTypeValueNodeParamsS[T, U, E](node: TypeValueNode[T])(allocatedParams: Map[Int, Int], unallocatedParamAppIdx: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]): (E, Validation[NoType[T], (Map[Int, Int], Set[Int], Set[Int], TypeValueNode[T])]) =
    node match {
      case TypeValueBranch(childs, tupleTypes, leafCount) =>
        val (env2, res) = unsafeAllocateTypeParamsFromTypeValueNodesS(childs)(allocatedParams, unallocatedParamAppIdx)(env)
        res match {
          case Success((allocatedParams2, allocatedArgParams2, allocatedParamAppIdxs, childs2)) =>
            val (env3, res2) = unsafeAllocateTypeParamsFromTupleTypesS(tupleTypes)(allocatedParams2, unallocatedParamAppIdx)(env2)
            (env3, res2.map { _.mapElements(identity, identity, identity, tts => TypeValueBranch(childs2, tts, leafCount))})        
          case Failure(noType)                                                                  =>
            (env2, noType.failure)
        }
      case TypeValueLeaf(TypeParamAppIdentity(param), paramAppIdx, leafCount) =>
        val (env2, res) = unsafeAllocateTypeValueTermParamsS(TypeParamApp(param, Nil, paramAppIdx))(allocatedParams, unallocatedParamAppIdx)(env)
        (env2, res.flatMap {
          case (allocatedParams2, allocatedArgParams, allocatedParamAppIdxs, TypeParamApp(param2, _, paramAppIdx2)) =>
            (allocatedParams2, allocatedArgParams, allocatedParamAppIdxs, TypeValueLeaf[T](TypeParamAppIdentity(param2), paramAppIdx2, leafCount)).success
          case _ =>
            NoType.fromError(FatalError("no type parameter application", none, NoPosition)).failure
        })
      case _: TypeValueLeaf[T] =>
        (env, (allocatedParams, Set[Int](), Set[Int](), node).success)
      case GlobalTypeAppNode(loc, childs, tupleTypes, leafCount, sym) =>
        val (env2, res) = unsafeAllocateTypeParamsFromTypeValueNodesS(childs)(allocatedParams, unallocatedParamAppIdx)(env)
        res match {
          case Success((allocatedParams2, allocatedArgParams2, allocatedParamAppIdxs, childs2)) =>
            val (env3, res2) = unsafeAllocateTypeParamsFromTupleTypesS(tupleTypes)(allocatedParams2, unallocatedParamAppIdx)(env2)
            (env3, res2.map { _.mapElements(identity, identity, identity, tts => GlobalTypeAppNode(loc, childs2, tts, leafCount, sym))})        
          case Failure(noType)                                                                  =>
            (env2, noType.failure)
        }
    }
    
  def unsafeAllocateTypeParamsFromLogicalTypeValueTermS[T, U, E](term: LogicalTypeValueTerm[T])(allocatedParams: Map[Int, Int], unallocatedParamAppIdx: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]): (E, Validation[NoType[T], (Map[Int, Int], Set[Int], Set[Int], TypeValueTerm[T])]) = {
    val (env2, res) = unsafeAllocateTypeValueNodeParamsS(term.conjNode)(allocatedParams, unallocatedParamAppIdx)(env)
    res match {
      case Success((allocatedParams2, allocatedArgParams, allocatedParamAppIdxs, conjNode2)) =>
        val (env3, res2) = stFoldLeftValidationS(term.args)((allocatedParams2, Set[Int](), Set[Int](), Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]]()).success[NoType[T]]) {
          (tmpTuple, pair, newEnv: E) =>
            val (newAllocatedParams, newAllocatedArgParams, newAllocatedParamAppIdxs, newArgs) = tmpTuple
            val (ident, args) = pair
            val optIdent2 = ident match {
              case TypeParamAppIdentity(param) => newAllocatedParams.get(param).map { TypeParamAppIdentity(_) }
              case _                           => some(ident)
            }
            optIdent2.map {
              ident2 =>
                val (newEnv2, newRes) = unsafeAllocateTypeParamsFromTypeValueLambdasS(args)(newAllocatedParams, unallocatedParamAppIdx)(newEnv)
                (newEnv2, newRes.map { 
                  _.mapElements(identity, newAllocatedArgParams | _, newAllocatedParamAppIdxs | _, as => newArgs + (ident2 -> as))
                })
            }.getOrElse((newEnv, NoType.fromError[T](FatalError("unallocated parameter", none, NoPosition)).failure))
        } (env2)
        (env3, res2.map { _.mapElements(identity, allocatedArgParams | _, allocatedParamAppIdxs | _, LogicalTypeValueTerm(conjNode2, _)) })
      case Failure(noType) =>
        (env2, noType.failure)
    }
  }
  
  private def checkTypeParamsFromTypeValueNodesS[T, U, E](nodes: Seq[TypeValueNode[T]])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    stFoldLeftValidationS(nodes)(().success[NoType[T]]) {
      (_, n, newEnv: E) => checkTypeParamsFromTypeValueNodeS(n)(newEnv)
    } (env)

  private def checkTypeParamsFromTypeValueNodeS[T, U, E](node: TypeValueNode[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]): (E, Validation[NoType[T], Unit]) =
    node match {
      case TypeValueBranch(childs, tupleTypes, _) =>
        val (env2, res) = checkTypeParamsFromTypeValueNodesS(childs)(env)
        res.map { _ => checkTypeParamsFromTupleTypesS(tupleTypes)(env2) }.valueOr { nt => (env2, nt.failure) }
      case TypeValueLeaf(TypeParamAppIdentity(param), _, _) =>
        val (env2, isLambdaArgParam) = envSt.isTypeLambdaArgParamS(param)(env)
        if(!isLambdaArgParam)
          (env2, ().success)
        else
          unifier.mismatchedTermErrorS(env2).mapElements(identity, _.failure)
      case _: TypeValueLeaf[T] =>
        (env, ().success)
      case GlobalTypeAppNode(_, childs, tupleTypes, _, _) =>
        val (env2, res) = checkTypeParamsFromTypeValueNodesS(childs)(env)
        res.map { _ => checkTypeParamsFromTupleTypesS(tupleTypes)(env2) }.valueOr { nt => (env2, nt.failure) }
    }
  
  def checkTypeParamsFromLogicalTypeValueTermS[T, U, E](term: LogicalTypeValueTerm[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]): (E, Validation[NoType[T], Unit]) =
    term match {
      case LogicalTypeValueTerm(conjNode, args) =>
        val (env2, res) = checkTypeParamsFromTypeValueNodeS(conjNode)(env)
        res.map {
          _ =>
            stFoldLeftValidationS(args)(().success[NoType[T]]) {
              (_, p, newEnv: E) => checkTypeParamsFromTypeValueLambdasS(p._2)(newEnv)
            } (env)
        }.valueOr { nt => (env2, nt.failure) }
    }
}
