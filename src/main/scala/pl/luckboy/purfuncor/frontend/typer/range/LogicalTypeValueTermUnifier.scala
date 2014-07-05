/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer.range
import scala.collection.immutable.SortedSet
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend.typer._
import pl.luckboy.purfuncor.util.CollectionUtils._
import pl.luckboy.purfuncor.util.StateUtils._

object LogicalTypeValueTermUnifier
{
  private type NodeTupleT[T] = (Map[TypeValueIdentity[T], TypeValueRangeSet[T]], Map[TypeValueRange, List[SortedSet[Int]]], SortedSet[Int], Map[FieldSetTypeIdentityKey, BuiltinTypeIdentity[T]])
  
  private type IndexTupleT[T] = (Set[Int], Set[Int], Map[TypeValueRange, Set[Int]], Map[TypeValueRange, Int], Map[Int, Int], Map[Int, Int], Set[Int])
  
  private def checkOrDistributeSupertypeConjunctionNode[T](node: TypeValueNode[T], nodeTuple: NodeTupleT[T], depthRangeSets: List[TypeValueRangeSet[T]], isSupertype: Boolean, canExpandGlobalType: Boolean, isRoot: Boolean)(leafIdx: Int)(prevParam: Int): (Int, List[(Option[TypeValueRangeSet[T]], TypeValueNode[T])]) = {
    val depthRangeSet = depthRangeSets.headOption.getOrElse(TypeValueRangeSet.empty)
    (node match {
      case TypeValueBranch(childs, tupleTypes, leafCount) =>
        val ((prevParam2, _), pairs7) = stFoldLeftS(childs)(List[((Option[TypeValueRangeSet[T]], TypeValueNode[T]), Vector[(Int, TypeValueLeaf[T])])]()) {
          (pairs, child, stPair: (Int, Int)) =>
            val (newPrevParam, newLeafIdx) = stPair
            val (newPrevParam2, pairs2) = checkOrDistributeSupertypeDisjunctionNode(child, nodeTuple, depthRangeSets, isSupertype, canExpandGlobalType, isRoot)(newLeafIdx)(newPrevParam).mapElements(identity, _.map { (_, Vector[(Int, TypeValueLeaf[T])]()) })
            if(!pairs.isEmpty) {
              val (pairs6, pairIdxs4) = pairs.foldLeft((List[((Option[TypeValueRangeSet[T]], TypeValueNode[T]), Vector[(Int, TypeValueLeaf[T])])](), Set[Int]())) {
                case ((pairs3, pairIdxs), pair @ ((optRangeSet, newChild), newLeafs)) =>
                  val (pairs5, pairIdxs3, isIntersected2) = pairs2.zipWithIndex.foldLeft((pairs3, pairIdxs, false)) {
                    case ((pairs4, pairIdxs2, isIntersected), (pair2 @ ((optRangeSet2, newChild2), newLeafs2), pairIdx)) =>
                      newChild match {
                        case newLeaf: TypeValueLeaf[T] if optRangeSet.map { _.isEmpty }.getOrElse(true) =>
                          // If range set of previous child is empty.
                          child match {
                            case leaf: TypeValueLeaf[T] =>
                              if(optRangeSet2.map { rs => !rs.isEmpty }.getOrElse(false))
                                ((pair2._1, (newLeafs2 ++ newLeafs) :+ (leafIdx -> newLeaf)) :: pairs4, pairIdxs2, true)
                              else
                                ((pair._1, newLeafs :+ (newLeafIdx -> leaf)) :: pairs4, pairIdxs2, true)
                            case _ =>
                              ((pair2._1, (newLeafs2 ++ newLeafs) :+ (leafIdx -> newLeaf)) :: pairs4, pairIdxs2, true)
                          }
                        case _ =>
                          val optRangeSet3 = (optRangeSet |@| optRangeSet2) { _ & _ }
                          child match {
                            case leaf: TypeValueLeaf[T] =>
                              if(optRangeSet3.map { rs => !rs.isEmpty }.getOrElse(false))
                                ((((optRangeSet3, newChild.withChildAndTupleTypes(newChild2, tupleTypes, canExpandGlobalType))), newLeafs ++ newLeafs2) :: pairs4, pairIdxs2 + pairIdx, true)
                              else
                                ((pair._1, newLeafs :+ (newLeafIdx -> leaf)) :: pairs4, pairIdxs2, true)
                            case _ =>
                              if(optRangeSet3.map { rs => !rs.isEmpty }.getOrElse(true))
                                ((((optRangeSet3, newChild.withChildAndTupleTypes(newChild2, tupleTypes, canExpandGlobalType))), newLeafs ++ newLeafs2) :: pairs4, pairIdxs2 + pairIdx, true)
                              else 
                                (pairs4, pairIdxs2, isIntersected)
                          }
                      }
                  }
                  (if(isIntersected2 || !child.isTypeValueLeaf) pairs5 else (pair :: pairs5), pairIdxs3)
              }
              ((newPrevParam2, newLeafIdx + child.leafCount), pairs2.zipWithIndex.flatMap {
                case (((ors, n), ls), pi) => if(!pairIdxs4.contains(pi)) List(((ors, TypeValueBranch[T](Vector(n), tupleTypes, n.leafCount)), ls)) else Nil
              } ++ pairs6)
            } else {
              ((newPrevParam2, newLeafIdx + child.leafCount), pairs2.map { case ((ors, n), ls) => ((ors, TypeValueBranch(Vector(n), tupleTypes, n.leafCount)), ls) })
            }
        } ((prevParam, leafIdx))
        // Check mismatched leafs.
        val (prevParam3, pairs8) = stFoldLeftS(pairs7)(List[(Option[TypeValueRangeSet[T]], TypeValueNode[T])]()) {
          (pairs, tmpPair, newPrevParam: Int) =>
            val (pair, leafs) = tmpPair
            val (newPrevParam2, (pair2, pairs2)) = stFoldLeftS(leafs)((pair, List[(Option[TypeValueRangeSet[T]], TypeValueNode[T])]())) {
              (tmpPair2, leafPair, newPrevParam2: Int) =>
                val (newPair @ (optRangeSet, newChild), newPairs) = tmpPair2
                val (leafIdx2, leaf2) = leafPair
                val ranges = optRangeSet.map { _.ranges.keys }.getOrElse(Nil)
                val (newPrevParam3, newPair2 @ (optRangeSet2, newChild2)) = checkOrDistributeSupertypeDisjunctionLeafForTypeParams(ranges, leaf2, nodeTuple, depthRangeSets, isSupertype)(leafIdx2)(newPrevParam2)
                val newOptRangeSet3 = (optRangeSet |@| optRangeSet2) { _ & _ }
                if(newOptRangeSet3.map { rs => !rs.isEmpty }.getOrElse(false))
                  (newPrevParam3, ((newOptRangeSet3, newChild.withChildAndTupleTypes(newChild2, tupleTypes, canExpandGlobalType)), newPairs))
                else
                  (newPrevParam3, (newPair, newPair2 :: newPairs))
            } (newPrevParam)
            (newPrevParam, (pair2 :: pairs2) ++ pairs)
        } (prevParam2)
        val pairs9 = pairs8.map { case (ors, n) => (ors.map { rs => rs.withConds(TypeValueRange(leafIdx, leafIdx + leafCount), rs.ranges.keys, tupleTypes) }, n) }
        if(isRoot)
          (prevParam3, pairs9.headOption.map {
            pair =>
              if(pairs9.size > 1)
                List(pairs9.foldLeft((some(TypeValueRangeSet.empty[T]), TypeValueBranch(Vector(), Nil, 0): TypeValueNode[T])) {
                  case ((_, n), (_, n2)) => (none[TypeValueRangeSet[T]], n.conjOrDisjWithTupleTypes(n2, tupleTypes, canExpandGlobalType))
                })
              else
                List(pair)
          }.getOrElse(List((some(TypeValueRangeSet.empty[T]), TypeValueBranch[T](Vector(), Nil, 0)))))
        else
          (prevParam3, pairs8)
      case leaf: TypeValueLeaf[T] =>
        val (prevParam2, rangeSet) = checkSupertypeConjunctionLeaf(leaf, nodeTuple, depthRangeSets, isSupertype)(leafIdx)(prevParam)
        (prevParam2, List((if(!rangeSet.isEmpty) some(rangeSet) else none, node)))
      case globalTypeAppNode: GlobalTypeAppNode[T] =>
        val node2 = globalTypeAppNode.typeValueBranchOrTypeValueLeaf(canExpandGlobalType)
        checkOrDistributeSupertypeConjunctionNode(node2, nodeTuple, depthRangeSets, isSupertype, canExpandGlobalType, isRoot)(leafIdx)(prevParam)
    }).mapElements(identity, _.map { case (ors, n) => (ors.map { _.superset(depthRangeSet) }, n.normalizedTypeValueNode) })
  }
  
  private def checkOrDistributeSupertypeDisjunctionNode[T](node: TypeValueNode[T], nodeTuple: NodeTupleT[T], depthRangeSets: List[TypeValueRangeSet[T]], isSupertype: Boolean, canExpandGlobalType: Boolean, isRoot: Boolean)(leafIdx: Int)(prevParam: Int): (Int, List[(Option[TypeValueRangeSet[T]], TypeValueNode[T])]) = {
    val depthRangeSets2 = depthRangeSets.headOption.map { _ => depthRangeSets.tail }.getOrElse(Nil)
    (node match {
      case TypeValueBranch(childs, tupleTypes, _) =>
        val ((prevParam2, _), pairs5) = stFoldLeftS(childs)(List[(Option[TypeValueRangeSet[T]], TypeValueNode[T])]()) {
          (pairs, child, stPair: (Int, Int)) =>
            val (newPrevParam, newLeafIdx) = stPair
            val (newPrevParam2, pairs2) = checkOrDistributeSupertypeConjunctionNode(child, nodeTuple, depthRangeSets, isSupertype, canExpandGlobalType, isRoot)(newLeafIdx)(newPrevParam)
            val pairs4 = if(!pairs2.isEmpty)
              pairs.foldLeft(List[(Option[TypeValueRangeSet[T]], TypeValueNode[T])]()) {
                case (pairs3, pair @ (optRangeSet, newChild)) =>
                  pairs2.zipWithIndex.foldLeft(pairs3) {
                    case (pairs4, (pair2 @ (optRangeSet2, newChild2), pairIdx)) =>
                      val optRangeSet3 = (optRangeSet |@| optRangeSet2) { _ | _ }.orElse(optRangeSet).orElse(optRangeSet2)
                      ((optRangeSet3, newChild.withChildAndTupleTypes(newChild2, tupleTypes, canExpandGlobalType))) :: pairs4
                  }
              }
            else
              pairs2.map { case (ors, n) => (ors, TypeValueBranch(Vector(n), tupleTypes, n.leafCount)) }
            ((newPrevParam2, newLeafIdx + child.leafCount), pairs4)
        } ((prevParam, leafIdx))
        (prevParam2, pairs5)
      case leaf: TypeValueLeaf[T] =>
        val (prevParam2, rangeSet) = checkSupertypeDisjunctionNode(leaf, nodeTuple, depthRangeSets2, isSupertype, canExpandGlobalType)(leafIdx)(prevParam)
        (prevParam2, List((if(!rangeSet.isEmpty) some(rangeSet) else none, node)))
      case globalTypeAppNode: GlobalTypeAppNode[T] =>
        val node2 = globalTypeAppNode.typeValueBranchOrTypeValueLeaf(canExpandGlobalType)
        checkOrDistributeSupertypeDisjunctionNode(node2, nodeTuple, depthRangeSets, isSupertype, canExpandGlobalType, isRoot)(leafIdx)(prevParam)
    }).mapElements(identity, _.map { case (ors, n) => (ors, n.normalizedTypeValueNode) })
  }
  
  private def checkOrDistributeSupertypeDisjunctionLeafForTypeParams[T](ranges: Iterable[TypeValueRange], node: TypeValueNode[T], nodeTuple: NodeTupleT[T], depthRangeSets: List[TypeValueRangeSet[T]], isSupertype: Boolean)(leafIdx: Int)(prevParam: Int) = {
    val (prevParam2, rangeSet) = checkSupertypeDisjunctionLeafForTypeParams(ranges, node, nodeTuple, depthRangeSets, isSupertype)(leafIdx)(prevParam)
    (prevParam2, (if(!rangeSet.isEmpty) some(rangeSet) else none, node))
  }
  
  private def checkSupertypeConjunctionNode[T](node: TypeValueNode[T], nodeTuple: NodeTupleT[T], depthRangeSets: List[TypeValueRangeSet[T]], isSupertype: Boolean, canExpandGlobalType: Boolean)(leafIdx: Int)(prevParam: Int): (Int, TypeValueRangeSet[T]) = {
    val depthRangeSet = depthRangeSets.headOption.getOrElse(TypeValueRangeSet.empty)
    (node match {
      case TypeValueBranch(childs, tupleTypes, leafCount) =>
        val ((prevParam2, _, leafs), rangeSet4) = stFoldLeftS(childs)(TypeValueRangeSet.full[T]) {
          (rangeSet, child, stTuple: (Int, Int, Vector[(Int, TypeValueLeaf[T])])) =>
            val (newPrevParam, newLeafIdx, newLeafs) = stTuple
            val (newPrevParam2, rangeSet2) = checkSupertypeDisjunctionNode(child, nodeTuple, depthRangeSets, isSupertype, canExpandGlobalType)(newLeafIdx)(newPrevParam)
            val rangeSet3 = rangeSet & rangeSet2
            child match {
              case leaf: TypeValueLeaf[T] =>
                if(!rangeSet3.isEmpty)
                  ((newPrevParam2, newLeafIdx + child.leafCount, newLeafs), rangeSet3)
                else
                  ((newPrevParam2, newLeafIdx + child.leafCount, newLeafs :+ (newLeafIdx -> leaf)), rangeSet)
              case _                      =>
                ((newPrevParam2, newLeafIdx + child.leafCount, newLeafs), rangeSet3)
            }
        } ((prevParam, leafIdx, Vector[(Int, TypeValueLeaf[T])]()))
        // Check mismatched leafs.
        val (prevParam3, rangeSet5) = stFoldLeftS(leafs)(rangeSet4) {
          (rangeSet, leafPair, newPrevParam: Int) =>
            val (leafIdx2, leaf2) = leafPair
            val (newPrevParam2, rangeSet2) = checkSupertypeDisjunctionLeafForTypeParams(rangeSet.ranges.keys, leaf2, nodeTuple, depthRangeSets, isSupertype)(leafIdx2)(newPrevParam)
            (newPrevParam2, rangeSet & rangeSet2)
        } (prevParam2)
        (prevParam2, rangeSet4.withConds(TypeValueRange(leafIdx, leafIdx + leafCount), rangeSet4.ranges.keys, tupleTypes))
      case leaf: TypeValueLeaf[T] =>
        checkSupertypeConjunctionLeaf(leaf, nodeTuple, depthRangeSets, isSupertype)(leafIdx)(prevParam)
      case globalTypeAppNode: GlobalTypeAppNode[T] =>
        val node2 = globalTypeAppNode.typeValueBranchOrTypeValueLeaf(canExpandGlobalType)
        checkSupertypeDisjunctionNode(node2, nodeTuple, depthRangeSets, isSupertype, canExpandGlobalType)(leafIdx)(prevParam)
    }).mapElements(identity, _.superset(depthRangeSet))
  }
  
  private def checkSupertypeDisjunctionNode[T](node: TypeValueNode[T], nodeTuple: NodeTupleT[T], depthRangeSets: List[TypeValueRangeSet[T]], isSupertype: Boolean, canExpandGlobalType: Boolean)(leafIdx: Int)(prevParam: Int): (Int, TypeValueRangeSet[T]) = {
    val depthRangeSets2 = depthRangeSets.headOption.map { _ => depthRangeSets.tail }.getOrElse(Nil)
    node match {
      case TypeValueBranch(childs, _, _) =>
        val ((prevParam2, _), rangeSet3) = stFoldLeftS(childs)(TypeValueRangeSet.full[T]) {
          (rangeSet, child, stPair: (Int, Int)) =>
            val (newPrevParam, newLeafIdx) = stPair
            val (newPrevParam2, rangeSet2) = checkSupertypeConjunctionNode(child, nodeTuple, depthRangeSets2, isSupertype, canExpandGlobalType)(newLeafIdx)(newPrevParam)
            ((newPrevParam2, newLeafIdx + child.leafCount), rangeSet | rangeSet2)
        } ((prevParam, leafIdx))
        (prevParam2, rangeSet3)
      case leaf: TypeValueLeaf[T] =>
        checkSupertypeDisjunctionLeaf(leaf, nodeTuple, depthRangeSets2, isSupertype)(leafIdx)(prevParam)
      case globalTypeAppNode: GlobalTypeAppNode[T] =>
        val node2 = globalTypeAppNode.typeValueBranchOrTypeValueLeaf(canExpandGlobalType)
        checkSupertypeDisjunctionNode(node2, nodeTuple, depthRangeSets, isSupertype, canExpandGlobalType)(leafIdx)(prevParam)
    }
  }

  private def checkSupertypeDisjunctionBranch[T](branch: TypeValueBranch[T], nodeTuple: NodeTupleT[T], depthRangeSets2: List[TypeValueRangeSet[T]], isSupertype: Boolean, canExpandGlobalType: Boolean)(leafIdx: Int)(prevParam: Int): (Int, (TypeValueRangeSet[T], Seq[TupleType[T]])) =
    branch match {
      case TypeValueBranch(childs, _, _) =>
        val ((prevParam2, _), rangeSet3) = stFoldLeftS(childs)(TypeValueRangeSet.full[T]) {
          (rangeSet, child, stPair: (Int, Int)) =>
            val (newPrevParam, newLeafIdx) = stPair
            val (newPrevParam2, rangeSet2) = checkSupertypeConjunctionNode(child, nodeTuple, depthRangeSets2, isSupertype, canExpandGlobalType)(newLeafIdx)(newPrevParam)
            ((newPrevParam2, newLeafIdx + child.leafCount), rangeSet | rangeSet2)
        } ((prevParam, leafIdx))
        (prevParam2, (rangeSet3, Vector()))
    }
  
  private def checkSupertypeDisjunctionLeafForTypeParams[T](ranges: Iterable[TypeValueRange], node: TypeValueNode[T], nodeTuple: NodeTupleT[T], depthRangeSets: List[TypeValueRangeSet[T]], isSupertype: Boolean)(leafIdx: Int)(prevParam: Int) = {
    val (rangeSets, params, allParams, fieldSetTypeIdents) = nodeTuple
    val depthRangeSets2 = depthRangeSets.headOption.map { _ => depthRangeSets.tail }.getOrElse(Nil)
    ranges.headOption.flatMap {
      firstRange =>
        if(ranges.size == 1 && firstRange.minIdx == 0 && firstRange.maxIdx == Integer.MAX_VALUE)
          allParams.from(prevParam + 1).headOption.orElse { allParams.headOption }.flatMap {
            param =>
              checkSupertypeValueLeaf(TypeValueLeaf(TypeParamAppIdentity(param), 0, 1), rangeSets, depthRangeSets2, some(param), false, isSupertype)(leafIdx).map {
                (param, _)
              }
          }
        else
         ranges.foldLeft(none[(Int, TypeValueRangeSet[T])]) {
           case (None, range) =>
             params.get(range).flatMap {
               paramSets =>
                 paramSets.foldLeft(none[Int]) {
                   case (None, paramSet) => paramSet.from(prevParam + 1).headOption.orElse { paramSet.headOption }
                   case (Some(param), _) => Some(param)
                 }.flatMap {
                   param =>
                     checkSupertypeValueLeaf(TypeValueLeaf(TypeParamAppIdentity(param), 0, 1), rangeSets, depthRangeSets2, some(param), false, isSupertype)(leafIdx).map {
                       (param, _)
                     }
                 }
             }
           case (Some(pair), _) =>
             Some(pair)
         }
    }.getOrElse {
      node match {
        case TypeValueLeaf(TypeParamAppIdentity(_), _, _) =>
          val bf = if(isSupertype) TypeBuiltinFunction.Any else TypeBuiltinFunction.Nothing
          checkSupertypeValueLeaf(TypeValueLeaf(BuiltinTypeIdentity(bf, Nil), 0, 1), rangeSets, depthRangeSets2, none, true, isSupertype)(leafIdx).map { (prevParam, _) }.getOrElse((prevParam, TypeValueRangeSet.empty[T]))
        case TypeValueLeaf(BuiltinTypeIdentity(TypeBuiltinFunction.FieldSet1 | TypeBuiltinFunction.FieldSet2, Seq(argIdent1, argIdent2)), _, _) =>
          // Exception for #FieldSet1 and #FieldSet2.
          fieldSetTypeIdents.get(FirstArgFieldSetTypeIdentityKey(argIdent1)).orElse { 
            fieldSetTypeIdents.get(SecondArgFieldSetTypeIdentityKey(argIdent2))
          }.orElse {
            fieldSetTypeIdents.get(NoArgFieldSetTypeIdentityKey) 
          }.flatMap {
            ident =>
              checkSupertypeValueLeaf(TypeValueLeaf(ident, 0, 1), rangeSets, depthRangeSets2, none, false, isSupertype)(leafIdx).map {
                (prevParam, _)
              }
          }.getOrElse((prevParam, TypeValueRangeSet.empty[T]))
        case _ =>
          (prevParam, TypeValueRangeSet.empty[T])
      }
    }
  }
  
  private def checkSupertypeConjunctionLeaf[T](leaf: TypeValueLeaf[T], nodeTuple: NodeTupleT[T], depthRangeSets: List[TypeValueRangeSet[T]], isSupertype: Boolean)(leafIdx: Int)(prevParam: Int) = {
    val depthRangeSets2 = depthRangeSets.headOption.map { _ => depthRangeSets.tail }.getOrElse(Nil)
    val (rangeSets, params, allParams, fieldSetTypeIdents) = nodeTuple
    checkSupertypeValueLeaf(leaf, rangeSets, depthRangeSets2, none, false, isSupertype)(leafIdx).map { (prevParam, _) }.orElse {
      allParams.from(prevParam + 1).headOption.orElse { allParams.headOption }.flatMap {
        param =>
          checkSupertypeValueLeaf(TypeValueLeaf(TypeParamAppIdentity(param), 0, 1), rangeSets, depthRangeSets2, some(param), false, isSupertype)(leafIdx).map {
            (param, _)
          }
      }
    }.getOrElse((prevParam, TypeValueRangeSet.empty[T]))
  }
  
  private def checkSupertypeDisjunctionLeaf[T](leaf: TypeValueLeaf[T], nodeTuple: NodeTupleT[T], depthRangeSets2: List[TypeValueRangeSet[T]], isSupertype: Boolean)(leafIdx: Int)(prevParam: Int) = {
    val (rangeSets, params, allParams, fieldSetTypeIdents) = nodeTuple
    checkSupertypeValueLeaf(leaf, rangeSets, depthRangeSets2, none, false, isSupertype)(leafIdx).orElse {
      val bf = if(isSupertype) TypeBuiltinFunction.Nothing else TypeBuiltinFunction.Any
      checkSupertypeValueLeaf(TypeValueLeaf(BuiltinTypeIdentity(bf, Nil), 0, 1), rangeSets, depthRangeSets2, none, true, isSupertype)(leafIdx)
    }.map { (prevParam, _) }.getOrElse {
      (prevParam, TypeValueRangeSet.empty[T])
    }
  }
  
  private def checkSupertypeValueLeaf[T](leaf: TypeValueLeaf[T], rangeSets: Map[TypeValueIdentity[T], TypeValueRangeSet[T]], depthRangeSets2: List[TypeValueRangeSet[T]], param: Option[Int], isNothing: Boolean, isSupertype: Boolean)(leafIdx: Int) = {
    leaf match {
      case TypeValueLeaf(ident, _, _) =>
        rangeSets.get(ident).map {
          rs =>
            val rangeSet = depthRangeSets2.headOption.map(rs.swapLeafIdxPairsWithMyLeafIdx(leafIdx).superset).getOrElse(rs.swapLeafIdxPairsWithMyLeafIdx(leafIdx))
            val rangeSet2 = param.map { rangeSet.withMyParam(leafIdx, _) }.getOrElse(rangeSet)
            if(isNothing) rangeSet2.withMyNothingIdx(leafIdx) else rangeSet2
        }
    }
  }
  
  private def checkLeafIndexSetsForTypeConjunction[T](indexTuple: IndexTupleT[T], node: TypeValueNode[T], isSupertype: Boolean, canExpandGlobalType: Boolean)(leafIdx: Int, tuple: (Set[TypeValueIdentity[T]], Set[Int], Seq[TypeParamCondition[T]])): Option[(Set[TypeValueIdentity[T]], Set[Int], Seq[TypeParamCondition[T]])] = {
    val (myLeafIdxs, otherLeafIdxs, myCondIdxs, otherCondIdxs, myParams, myParamAppIdxs, myNothingIdxs) = indexTuple
    node match {
      case TypeValueBranch(childs, _, _) =>
        val optTuple3 = stFoldLeftS(childs)(some(tuple)) {
          (optTuple2, child, newLeafIdx: Int) =>
            optTuple2 match {
              case Some(tuple2) => (newLeafIdx + child.leafCount, checkLeafIndexSetsForTypeDisjunction(indexTuple, child, isSupertype, canExpandGlobalType)(newLeafIdx, tuple2))
              case None         => (newLeafIdx + child.leafCount, none)
            }
        } (leafIdx)._2
        optTuple3.map {
          tuple3 => tuple3.copy(_2 = myCondIdxs.get(TypeValueRange(leafIdx, leafIdx + node.leafCount)).map(tuple3._2 |).getOrElse(tuple3._2))
        }
      case TypeValueLeaf(ident, paramAppIdx, _) =>
        checkLeafIndexSetsForTypeDisjunction(indexTuple, node, isSupertype, canExpandGlobalType)(leafIdx, tuple)
      case globalTypeAppNode: GlobalTypeAppNode[T] =>
        val node2 = globalTypeAppNode.typeValueBranchOrTypeValueLeaf(canExpandGlobalType)
        checkLeafIndexSetsForTypeConjunction(indexTuple, node2, isSupertype, canExpandGlobalType)(leafIdx, tuple)
    }
  }

  private def checkLeafIndexSetsForTypeDisjunction[T](indexTuple: IndexTupleT[T], node: TypeValueNode[T], isSupertype: Boolean, canExpandGlobalType: Boolean)(leafIdx: Int, tuple: (Set[TypeValueIdentity[T]], Set[Int], Seq[TypeParamCondition[T]])): Option[(Set[TypeValueIdentity[T]], Set[Int], Seq[TypeParamCondition[T]])] = {
    val (myLeafIdxs, otherLeafIdxs, myCondIdxs, otherCondIdxs, myParams, myParamAppIdxs, myNothingIdxs) = indexTuple
    node match {
      case TypeValueBranch(childs, _, _) =>
        val optTuple3 = stFoldLeftS(childs)(some(tuple)) {
          (optTuple2, child, newLeafIdx: Int) =>
            optTuple2 match {
              case None => (newLeafIdx + child.leafCount, checkLeafIndexSetsForTypeConjunction(indexTuple, child, isSupertype, canExpandGlobalType)(newLeafIdx, tuple))
              case _    => (newLeafIdx + child.leafCount, optTuple2)
            }
        } (leafIdx)._2
        optTuple3.map {
          tuple3 => tuple3.copy(_2 = otherCondIdxs.get(TypeValueRange(leafIdx, leafIdx + node.leafCount)).map(tuple3._2 +).getOrElse(tuple3._2))
        }
      case leaf @ TypeValueLeaf(ident, _, _) =>
        val myParam = myParams.get(leafIdx)
        if(myLeafIdxs.contains(leafIdx) && (otherLeafIdxs.contains(leafIdx) || myParam.isDefined || myNothingIdxs.contains(leafIdx)))
          some(tuple.copy(_1 = tuple._1 + ident, _3 = tuple._3 ++ (myParam |@| myParamAppIdxs.get(leafIdx)) { TypeParamCondition(_, _, leaf, if(isSupertype) TypeMatching.TypeWithSupertype else TypeMatching.SupertypeWithType) }))
        else
          none
      case globalTypeAppNode: GlobalTypeAppNode[T] =>
        val node2 = globalTypeAppNode.typeValueBranchOrTypeValueLeaf(canExpandGlobalType)
        checkLeafIndexSetsForTypeDisjunction(indexTuple, node2, isSupertype, canExpandGlobalType)(leafIdx, tuple)
    }  
  }
  
  private def fullyCheckOrDistributeSupertypeConjunctionNode[T](node: TypeValueNode[T], nodeTuple: NodeTupleT[T], depthRangeSets: List[TypeValueRangeSet[T]], args: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isSupertype: Boolean, canExpandGlobalType: Boolean) = {
    val normalizedNode = node.normalizedTypeValueNodeForChecking(canExpandGlobalType)
    checkOrDistributeSupertypeConjunctionNode(normalizedNode, nodeTuple, depthRangeSets, isSupertype, true, canExpandGlobalType)(0)(-1)._2 match {
      case List((optRangeSet, newNode)) => some((optRangeSet, LogicalTypeValueTerm(newNode, args)))
      case _                            => none
    }
  }
  
  private def fullyCheckOrDistributeSupertypeDisjunctionNode[T](node: TypeValueNode[T], nodeTuple: NodeTupleT[T], depthRangeSets: List[TypeValueRangeSet[T]], args: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isSupertype: Boolean, canExpandGlobalType: Boolean) = {
    val normalizedNode = node.normalizedTypeValueNodeForChecking(canExpandGlobalType)
    checkOrDistributeSupertypeDisjunctionNode(normalizedNode, nodeTuple, depthRangeSets, isSupertype, true, canExpandGlobalType)(0)(-1)._2 match {
      case List((optRangeSet, newNode)) => some((optRangeSet, LogicalTypeValueTerm(newNode, args)))
      case _                            => none
    }
  }
  
  private def checkOrDistributeTypeValueNodesFromLogicalTypeValueTerms[T](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T], isSupertype: Boolean, isFirstTry: Boolean, canExpandGlobalType: Boolean) = {
    val nodeTuple2 = (term2.info.conjRangeSets, term2.info.conjParams, term2.info.allParams, term2.info.fieldSetTypeIdents)
    val nodeTuple1 = (term1.info.disjRangeSets, term1.info.disjParams, term1.info.allParams, term1.info.fieldSetTypeIdents)
    (term1.conjNode, term2.conjNode) match {
      case (TypeValueBranch(childs1, tupleTypes1, _), TypeValueBranch(childs2, tupleTypes2, _)) if childs1.size > 1 && childs2.size === 1 && !isFirstTry =>
        val conjDepthRangeSets = TypeValueRangeSet.full[T] :: term2.info.conjDepthRangeSets
        val disjDepthRangeSet = term2.info.conjDepthRangeSets.headOption.getOrElse(TypeValueRangeSet.full)
        val disjDepthRangeSets = TypeValueRangeSet.full[T] :: TypeValueRangeSet.full[T] :: disjDepthRangeSet :: term1.info.conjDepthRangeSets
        for {
          pair1 <- fullyCheckOrDistributeSupertypeConjunctionNode(TypeValueBranch(Vector(TypeValueBranch(Vector(term1.conjNode), tupleTypes1, term1.conjNode.leafCount)), Nil, term1.conjNode.leafCount), nodeTuple2, conjDepthRangeSets, term1.args, isSupertype, canExpandGlobalType)
          pair2 <- fullyCheckOrDistributeSupertypeDisjunctionNode(term2.conjNode, nodeTuple1, disjDepthRangeSets, term2.args, !isSupertype, canExpandGlobalType)
        } yield (pair1, pair2)
      case (TypeValueBranch(childs1, tupleTypes1, _), TypeValueBranch(childs2, tupleTypes2, _)) if childs1.size === 1 && childs2.size > 1 && isFirstTry =>
        val conjDepthRangeSet = term2.info.conjDepthRangeSets.headOption.getOrElse(TypeValueRangeSet.full)
        val conjDepthRangeSets = TypeValueRangeSet.full[T] :: conjDepthRangeSet :: term2.info.conjDepthRangeSets
        val disjDepthRangeSets = TypeValueRangeSet.full[T] :: TypeValueRangeSet.full[T] :: term1.info.disjDepthRangeSets
        for {
          pair1 <- fullyCheckOrDistributeSupertypeConjunctionNode(term1.conjNode, nodeTuple2, conjDepthRangeSets, term1.args, isSupertype, canExpandGlobalType)
          pair2 <- fullyCheckOrDistributeSupertypeDisjunctionNode(TypeValueBranch(Vector(TypeValueBranch(Vector(term2.conjNode), tupleTypes2, term2.conjNode.leafCount)), Nil, term2.conjNode.leafCount), nodeTuple1, disjDepthRangeSets, term2.args, !isSupertype, canExpandGlobalType)
        } yield (pair1, pair2)
      case _ =>
        val conjDepthRangeSets = TypeValueRangeSet.full[T] :: term2.info.conjDepthRangeSets
        val disjDepthRangeSets = TypeValueRangeSet.full[T] :: TypeValueRangeSet.full[T] :: term1.info.disjDepthRangeSets
        for {
          pair1 <- fullyCheckOrDistributeSupertypeConjunctionNode(term1.conjNode, nodeTuple2, conjDepthRangeSets, term1.args, isSupertype, canExpandGlobalType)
          pair2 <- fullyCheckOrDistributeSupertypeDisjunctionNode(term2.conjNode, nodeTuple1, disjDepthRangeSets, term2.args, !isSupertype, canExpandGlobalType)
        } yield (pair1, pair2)
    }
  }
  
  private def checkTypeValueNodesFromLogicalTypeValueTerms[T](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T], isSupertype: Boolean, isFirstTry: Boolean, canExpandGlobalType: Boolean) =
    checkOrDistributeTypeValueNodesFromLogicalTypeValueTerms(term1, term2, isSupertype, isFirstTry, canExpandGlobalType: Boolean).map {
      case ((optRangeSet1, distributedTerm1), (optRangeSet2, distributedTerm2)) =>
        val conjDepthRangeSets = TypeValueRangeSet.full[T] :: distributedTerm2.info.conjDepthRangeSets
        val disjDepthRangeSets = TypeValueRangeSet.full[T] :: TypeValueRangeSet.full[T] :: distributedTerm1.info.disjDepthRangeSets
        val nodeTuple2 = (distributedTerm2.info.conjRangeSets, distributedTerm2.info.conjParams, distributedTerm2.info.allParams, distributedTerm2.info.fieldSetTypeIdents)
        val nodeTuple1 = (distributedTerm1.info.disjRangeSets, distributedTerm1.info.disjParams, distributedTerm1.info.allParams, distributedTerm1.info.fieldSetTypeIdents)
        val optRangeSetPair = (optRangeSet1 |@| optRangeSet2) { (_, _) }
        val conjRangeSet = optRangeSetPair.map { _._1 }.getOrElse {
          checkSupertypeConjunctionNode(distributedTerm1.conjNode, nodeTuple2, conjDepthRangeSets, isSupertype, canExpandGlobalType)(0)(-1)._2
        }
        val disjRangeSet = optRangeSetPair.map { _._2 }.getOrElse {
          checkSupertypeDisjunctionNode(distributedTerm2.conjNode, nodeTuple1, disjDepthRangeSets, !isSupertype, canExpandGlobalType)(0)(-1)._2
        }
        ((distributedTerm1, conjRangeSet), (distributedTerm2, disjRangeSet))
    }
    
  private def morePartiallyMatchesSupertypeValueTermWithTypeValueTerm[T](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T], isFirstTry: Boolean, canExpandGlobalType: Boolean) =
    checkTypeValueNodesFromLogicalTypeValueTerms(term1, term2, true, isFirstTry, canExpandGlobalType).flatMap {
      case ((distributedTerm1, conjRangeSet), (distributedTerm2, disjRangeSet)) =>
        if(!conjRangeSet.isEmpty && !disjRangeSet.isEmpty) {
          val conjMyLeafIdxs = conjRangeSet.value.myLeafIdxs.toSet
          val disjOtherLeafIdxs = disjRangeSet.value.otherLeafIdxs.toSet
          val conjMyParams = conjRangeSet.value.myParams.toSeq.toMap
          val conjMyNothingIdxs = conjRangeSet.value.myNothingIdxs.toSet
          val disjMyLeafIdxs = disjRangeSet.value.myLeafIdxs.toSet
          val conjOtherLeafIdxs = conjRangeSet.value.otherLeafIdxs.toSet
          val disjMyParams = disjRangeSet.value.myParams.toSeq.toMap
          val disjMyAnyIdxs = conjRangeSet.value.myNothingIdxs.toSet
          val pairs = conjRangeSet.value.conds.toVector
          val conjMyCondIdxs = pairs.zipWithIndex.foldLeft(Map[TypeValueRange, Set[Int]]()) { 
            case (condIdxs, (((myRange, _), _), i)) => condIdxs |+| Map(myRange -> Set(i))
          }
          val conjOtherCondIdxs = pairs.zipWithIndex.foldLeft(Map[TypeValueRange, Int]()) {
            case (condIdxs, (((_, otherRanges), _), i)) => condIdxs |+| otherRanges.map { _ -> i }.toMap
          }
          for {
            conjTuple <- checkLeafIndexSetsForTypeConjunction((conjMyLeafIdxs, disjOtherLeafIdxs, conjMyCondIdxs, Map(), conjMyParams, distributedTerm2.info.paramAppIdxs, conjMyNothingIdxs), distributedTerm1.conjNode, true, canExpandGlobalType)(0, (Set(), Set(), Seq()))
            disjTuple <- checkLeafIndexSetsForTypeDisjunction((disjMyLeafIdxs, conjOtherLeafIdxs, Map(), conjOtherCondIdxs, disjMyParams, distributedTerm1.info.paramAppIdxs, disjMyAnyIdxs), distributedTerm2.conjNode, false, canExpandGlobalType)(0, (Set(), Set(), Seq()))
          } yield {
            val conds = (conjTuple._2 | disjTuple._2).toSeq.flatMap { pairs.lift(_).map { _._2 } }
            (conjTuple._1 | disjTuple._1, conds, conjTuple._3 ++ disjTuple._3)
          }
        } else
          none
    }

  private def partiallyMatchesSupertypeValueTermWithTypeValueTerm[T](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T], isFirstTry: Boolean) =
    if(term1.info.unexpandedLeafCount * 2 > term1.info.expandedLeafCount && term2.info.unexpandedLeafCount * 2 > term2.info.expandedLeafCount)
      morePartiallyMatchesSupertypeValueTermWithTypeValueTerm(term1, term2, isFirstTry, true)
    else
      morePartiallyMatchesSupertypeValueTermWithTypeValueTerm(term1, term2, isFirstTry, false).orElse {
        morePartiallyMatchesSupertypeValueTermWithTypeValueTerm(term1, term2, isFirstTry, true)
      }

  private def matchesSupertypeValueTermWithTypeValueTerm[T](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T]) =
    (term1.conjNode, term2.conjNode) match {
      case (TypeValueBranch(childs1, _, _), TypeValueBranch(childs2, _, _)) if (childs1.size > 1 && childs2.size === 1) || (childs1.size === 1 && childs2.size > 1) =>
        partiallyMatchesSupertypeValueTermWithTypeValueTerm(term1, term2, true).orElse(partiallyMatchesSupertypeValueTermWithTypeValueTerm(term1, term2, false))
      case _ =>
        partiallyMatchesSupertypeValueTermWithTypeValueTerm(term1, term2, true)
    }
  
  private def matchesLocigalTypeValueTermsWithoutArgs[T, U, E](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T], typeMatching: TypeMatching.Value) =
    typeMatching match {
      case TypeMatching.Types             =>
        for {
          tuple1 <- matchesSupertypeValueTermWithTypeValueTerm(term1, term2)
          tuple2 <- matchesSupertypeValueTermWithTypeValueTerm(term2, term1)
        } yield {
          (tuple1._1 | tuple2._1, tuple1._2 ++ tuple2._2, (tuple1._3, tuple2._3))
        }
      case TypeMatching.SupertypeWithType =>
        matchesSupertypeValueTermWithTypeValueTerm(term1, term2).map { _.mapElements(identity, identity, (_, Nil)) }
      case TypeMatching.TypeWithSupertype =>
        matchesSupertypeValueTermWithTypeValueTerm(term2, term1).map { _.mapElements(identity, identity, (Nil, _)) }
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
                 TypeValueTermUnifier.matchesTypeValueTermsS(myTupleType, otherTupleType)(x)(f)(_: E)
               } (newEnv3)
            else
              // last other tuple type
              TypeValueTermUnifier.matchesTypeValueTermsS(myTupleType, otherTupleType)(x)(f)(newEnv3).mapElements(identity, (_, true))
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
    val paramCondTuples = Set[(Int, Int, TypeMatching.Value)]()
    stFoldLeftValidationS(paramConds)((z, paramCondTuples).success[NoType[T]]) {
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
          val (newEnv4, res) = (args1.get(TypeParamAppIdentity(paramCond.param)) |@| args2.get(paramCond.leaf.ident)) {
            (paramArgs, leafArgs) =>
              val typeParamApp = TypeParamApp(paramCond.param, paramArgs, paramCond.paramAppIdx)
              paramCond.leaf.typeValueTerm(leafArgs).map {
                TypeValueTermUnifier.matchesTypeValueTermsS(typeParamApp, _)(x)(f)(newEnv3)
              }.getOrElse((newEnv3, NoType.fromError[T](FatalError("can't convert type value leaf to type value term", none, NoPosition)).failure))
          }.getOrElse((newEnv3, NoType.fromError[T](FatalError("not found arguments", none, NoPosition)).failure))
          val (newEnv5, _) = envSt.setCurrentTypeMatchingS(savedTypeMatching)(newEnv4)
          (newEnv5, res.map { (_, paramCondTuples ++ optParamPair.map { case (p1, p2) => (p1, p2, paramCond.typeMatching) })})
        } else
          (newEnv, pair.success)
    } (env).mapElements(identity, _.map { _._1 })
  }
  
  def matchesLocigalTypeValueTermsS[T, U, V, E](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T], typeMatching: TypeMatching.Value)(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]) = {
    matchesLocigalTypeValueTermsWithoutArgs(term1, term2, typeMatching).map {
      case (idents, conds, (paramConds1, paramConds2)) =>
        st(for {
          x2 <- steS({
            stFoldLeftValidationS(idents)(z.success[NoType[T]]) {
              (x, ident, newEnv: E) =>
                (term1.args.get(ident) |@| term2.args.get(ident)) {
                  (args1, args2) =>
                    if(args1.size === args2.size) {
                      stFoldLeftValidationS(args1.zip(args2))(x.success[NoType[T]]) {
                        (x2, argPair, newEnv2: E) =>
                          val (arg1, arg2) = argPair
                          TypeValueTermUnifier.matchesTypeValueLambdasS(arg1, arg2)(x2)(f)(newEnv2)
                      } (newEnv)
                   } else
                     unifier.mismatchedTermErrorS(env).mapElements(identity, _.failure)
                  }.getOrElse(unifier.mismatchedTermErrorS(env).mapElements(identity, _.failure))
                }
          })
          x3 <- steS(checkTypeValueRangeConditionsS(conds)(x2)(f)(_: E))
          x4 <- steS(checkTypeParamConditionsS(paramConds1, term1.args, term2.args)(x3)(f)(_: E))
          y <- steS(checkTypeParamConditionsS(paramConds2, term2.args, term1.args)(x4)(f)(_: E))
        } yield y).run(env)
    }.getOrElse(unifier.mismatchedTermErrorS(env).mapElements(identity, _.failure))
  }
  
  private def unsafeAllocateTypeParamsFromTypeValueNodesS[T, U, E](nodes: Iterable[TypeValueNode[T]])(allocatedParams: Map[Int, Int], unallocatedParamAppIdx: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    stFoldLeftValidationS(nodes)((allocatedParams, Set[Int](), Set[Int](), Vector[TypeValueNode[T]]()).success[NoType[T]]) {
      (tmpTuple, node, newEnv: E) =>
        val (newAllocatedParams, newAllocatedArgParams, allocatedParamAppIdxs, newNodes) = tmpTuple
        val (newEnv2, newRes) = unsafeAllocateTypeValueNodeParamsS(node)(newAllocatedParams, unallocatedParamAppIdx)(newEnv)
        (newEnv2, newRes.map { 
          _.mapElements(identity, newAllocatedArgParams | _, allocatedParamAppIdxs | _, newNodes :+ _)
        })
    } (env)

  private def unsafeAllocateTypeParamsFromTupleTypesS[T, U, E](tupleTypes: Iterable[TupleType[T]])(allocatedParams: Map[Int, Int], unallocatedParamAppIdx: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    stFoldLeftValidationS(tupleTypes)((allocatedParams, Set[Int](), Set[Int](), Vector[TupleType[T]]()).success[NoType[T]]) {
      (tmpTuple, tupleType, newEnv: E) =>
        val (newAllocatedParams, newAllocatedArgParams, allocatedParamAppIdxs, newTupleTypes) = tmpTuple
        val (newEnv2, newRes) = TypeValueTermUnifier.unsafeAllocateTypeValueTermParamsS(tupleType)(newAllocatedParams, unallocatedParamAppIdx)(newEnv)
        (newEnv2, newRes.flatMap {
          case tmpTuple2 @ (_, _, _, newTypeTuple: TupleType[T]) =>
            tmpTuple2.mapElements(identity, newAllocatedArgParams | _, allocatedParamAppIdxs | _, _ => newTupleTypes :+ newTypeTuple).success
          case _                                                 =>
            NoType.fromError[T](FatalError("type value term isn't tuple type", none, NoPosition)).failure
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
        val (env3, res2) = allocatedParams.get(param).map { param2 => (env, (allocatedParams, param2).success) }.getOrElse {
          val (env2, res) = unifier.allocateParamS(env)
          res.map { param2 => (env2, (allocatedParams + (param -> param2), param2).success) }.valueOr {
            nt => (env2, nt.failure)
          }
        }
        res2 match {
          case Success((allocatedParams2, param2)) =>
            val (env4, res3) = if(paramAppIdx === unallocatedParamAppIdx)
              envSt.allocateTypeParamAppIdxS(env3)
            else
              (env3, paramAppIdx.success)
            res3 match {
              case Success(paramAppIdx2) =>
                (env4, (allocatedParams2, Set[Int](), Set(paramAppIdx2), TypeValueLeaf[T](TypeParamAppIdentity(param2), paramAppIdx2, leafCount)).success)
              case Failure(noType)       =>
                (env4, noType.failure)
            }
          case Failure(noType)                     =>
            (env3, noType.failure)
        }
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
        val (env3, res2) = stFoldLeftValidationS(term.args)((allocatedParams, Set[Int](), Set[Int](), Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]]()).success[NoType[T]]) {
          (tmpTuple, pair, newEnv: E) =>
            val (ident, args) = pair
            val (newAllocatedParams, newAllocatedArgParams, allocatedParamAppIdxs, newArgs) = tmpTuple
            val (newEnv2, newRes) = TypeValueTermUnifier.unsafeAllocateTypeParamsFromTypeValueLambdasS(args)(newAllocatedParams, unallocatedParamAppIdx)(newEnv)
            (newEnv2, newRes.map { 
              _.mapElements(identity, newAllocatedArgParams | _, allocatedParamAppIdxs | _, as => newArgs + (ident -> as))
            })
        } (env2)
        (env3, res2.map { _.mapElements(identity, allocatedArgParams | _, allocatedParamAppIdxs | _, LogicalTypeValueTerm(conjNode2, _)) })
      case Failure(noType) =>
        (env2, noType.failure)
    }
  }
}
