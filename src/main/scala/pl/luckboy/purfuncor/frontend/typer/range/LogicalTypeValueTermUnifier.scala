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
import TypeValueTermUnifier._

object LogicalTypeValueTermUnifier
{
  private type NodeTupleT[T] = (Map[TypeValueIdentity[T], TypeValueRangeSet[T]], Map[TypeValueRange, Map[Int, List[SortedSet[Int]]]], SortedSet[Int], Map[FieldSetTypeIdentityKey, BuiltinTypeIdentity[T]])
  
  private type IndexTupleT[T] = (Set[Int], Set[Int], Map[TypeValueRange, Set[Int]], Map[TypeValueRange, Int], Map[Int, Int], Map[Int, Int], Set[Int])
  
  private def checkOrDistributeSupertypeConjunctionNode[T](node: TypeValueNode[T], nodeTuple: NodeTupleT[T], depthRangeSets: List[TypeValueRangeSet[T]], args: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isSupertype: Boolean, canExpandGlobalType: Boolean, isRoot: Boolean)(leafIdx: Int)(prevParam: Int): (Int, List[(Option[TypeValueRangeSet[T]], TypeValueNode[T])]) = {
    val depthRangeSet = depthRangeSets.headOption.getOrElse(TypeValueRangeSet.empty)
    (node match {
      case TypeValueBranch(childs, tupleTypes, leafCount) =>
        val ((prevParam2, _), pairs7) = stFoldLeftS(childs)(List[((Option[TypeValueRangeSet[T]], TypeValueNode[T]), Vector[(Int, TypeValueLeaf[T])])]()) {
          (pairs, child, stPair: (Int, Int)) =>
            val (newPrevParam, newLeafIdx) = stPair
            val (newPrevParam2, pairs2) = checkOrDistributeSupertypeDisjunctionNode(child, nodeTuple, depthRangeSets, args, isSupertype, canExpandGlobalType, isRoot)(newLeafIdx)(newPrevParam).mapElements(identity, _.map { (_, Vector[(Int, TypeValueLeaf[T])]()) })
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
                val (newPrevParam3, newPair2 @ (optRangeSet2, newChild2)) = checkOrDistributeSupertypeDisjunctionLeafForTypeParams(ranges, leaf2, nodeTuple, depthRangeSets, args, isSupertype)(leafIdx2)(newPrevParam2)
                val newOptRangeSet3 = (optRangeSet |@| optRangeSet2) { _ & _ }
                if(newOptRangeSet3.map { rs => !rs.isEmpty }.getOrElse(false))
                  (newPrevParam3, ((newOptRangeSet3, newChild.withChildAndTupleTypes(newChild2, tupleTypes, canExpandGlobalType)), newPairs))
                else
                  (newPrevParam3, (newPair, newPair2 :: newPairs))
            } (newPrevParam)
            (newPrevParam, (pair2 :: pairs2) ++ pairs)
        } (prevParam2)
        val pairs9 = pairs8.map { case (ors, n) => (ors.map { rs => rs.withConds(TypeValueRange(leafIdx, leafIdx + leafCount - 1), rs.ranges.keys, tupleTypes) }, n) }
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
            val (newPrevParam2, pairs2) = checkOrDistributeSupertypeConjunctionNode(child, nodeTuple, depthRangeSets, args, isSupertype, canExpandGlobalType, isRoot)(newLeafIdx)(newPrevParam)
            val pairs5 = if(!pairs.isEmpty)
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
            ((newPrevParam2, newLeafIdx + child.leafCount), pairs5)
        } ((prevParam, leafIdx))
        (prevParam2, pairs6)
      case leaf: TypeValueLeaf[T] =>
        val (prevParam2, rangeSet) = checkSupertypeDisjunctionNode(leaf, nodeTuple, depthRangeSets2, args, isSupertype, canExpandGlobalType)(leafIdx)(prevParam)
        (prevParam2, List((if(!rangeSet.isEmpty) some(rangeSet) else none, node)))
      case globalTypeAppNode: GlobalTypeAppNode[T] =>
        val node2 = globalTypeAppNode.typeValueBranchOrTypeValueLeaf(canExpandGlobalType)
        checkOrDistributeSupertypeDisjunctionNode(node2, nodeTuple, depthRangeSets, args, isSupertype, canExpandGlobalType, isRoot)(leafIdx)(prevParam)
    }).mapElements(identity, _.map { case (ors, n) => (ors, n.normalizedTypeValueNode) })
  }
  
  private def checkOrDistributeSupertypeDisjunctionLeafForTypeParams[T](ranges: Iterable[TypeValueRange], leaf: TypeValueLeaf[T], nodeTuple: NodeTupleT[T], depthRangeSets: List[TypeValueRangeSet[T]], args: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isSupertype: Boolean)(leafIdx: Int)(prevParam: Int) = {
    val (prevParam2, rangeSet) = checkSupertypeDisjunctionLeafForTypeParams(ranges, leaf, nodeTuple, depthRangeSets, args, isSupertype)(leafIdx)(prevParam)
    (prevParam2, (if(!rangeSet.isEmpty) some(rangeSet) else none, leaf))
  }
  
  private def checkSupertypeConjunctionNode[T](node: TypeValueNode[T], nodeTuple: NodeTupleT[T], depthRangeSets: List[TypeValueRangeSet[T]], args: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isSupertype: Boolean, canExpandGlobalType: Boolean)(leafIdx: Int)(prevParam: Int): (Int, TypeValueRangeSet[T]) = {
    val depthRangeSet = depthRangeSets.headOption.getOrElse(TypeValueRangeSet.empty)
    (node match {
      case TypeValueBranch(childs, tupleTypes, leafCount) =>
        val ((prevParam2, _, leafs), rangeSet4) = stFoldLeftS(childs)(TypeValueRangeSet.full[T]) {
          (rangeSet, child, stTuple: (Int, Int, Vector[(Int, TypeValueLeaf[T])])) =>
            val (newPrevParam, newLeafIdx, newLeafs) = stTuple
            val (newPrevParam2, rangeSet2) = checkSupertypeDisjunctionNode(child, nodeTuple, depthRangeSets, args, isSupertype, canExpandGlobalType)(newLeafIdx)(newPrevParam)
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
            val (newPrevParam2, rangeSet2) = checkSupertypeDisjunctionLeafForTypeParams(rangeSet.ranges.keys, leaf2, nodeTuple, depthRangeSets, args, isSupertype)(leafIdx2)(newPrevParam)
            (newPrevParam2, rangeSet & rangeSet2)
        } (prevParam2)
        (prevParam2, rangeSet4.withConds(TypeValueRange(leafIdx, leafIdx + leafCount - 1), rangeSet4.ranges.keys, tupleTypes))
      case leaf: TypeValueLeaf[T] =>
        checkSupertypeConjunctionLeaf(leaf, nodeTuple, depthRangeSets, isSupertype)(leafIdx)(prevParam)
      case globalTypeAppNode: GlobalTypeAppNode[T] =>
        val node2 = globalTypeAppNode.typeValueBranchOrTypeValueLeaf(canExpandGlobalType)
        checkSupertypeDisjunctionNode(node2, nodeTuple, depthRangeSets, args, isSupertype, canExpandGlobalType)(leafIdx)(prevParam)
    }).mapElements(identity, _.superset(depthRangeSet))
  }
  
  private def checkSupertypeDisjunctionNode[T](node: TypeValueNode[T], nodeTuple: NodeTupleT[T], depthRangeSets: List[TypeValueRangeSet[T]], args: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isSupertype: Boolean, canExpandGlobalType: Boolean)(leafIdx: Int)(prevParam: Int): (Int, TypeValueRangeSet[T]) = {
    val depthRangeSets2 = depthRangeSets.headOption.map { _ => depthRangeSets.tail }.getOrElse(Nil)
    node match {
      case TypeValueBranch(childs, _, _) =>
        val ((prevParam2, _), rangeSet3) = stFoldLeftS(childs)(TypeValueRangeSet.full[T]) {
          (rangeSet, child, stPair: (Int, Int)) =>
            val (newPrevParam, newLeafIdx) = stPair
            val (newPrevParam2, rangeSet2) = checkSupertypeConjunctionNode(child, nodeTuple, depthRangeSets2, args, isSupertype, canExpandGlobalType)(newLeafIdx)(newPrevParam)
            ((newPrevParam2, newLeafIdx + child.leafCount), rangeSet | rangeSet2)
        } ((prevParam, leafIdx))
        (prevParam2, rangeSet3)
      case leaf: TypeValueLeaf[T] =>
        checkSupertypeDisjunctionLeaf(leaf, nodeTuple, depthRangeSets2, isSupertype)(leafIdx)(prevParam)
      case globalTypeAppNode: GlobalTypeAppNode[T] =>
        val node2 = globalTypeAppNode.typeValueBranchOrTypeValueLeaf(canExpandGlobalType)
        checkSupertypeDisjunctionNode(node2, nodeTuple, depthRangeSets, args, isSupertype, canExpandGlobalType)(leafIdx)(prevParam)
    }
  }
  
  private def checkSupertypeDisjunctionLeafForTypeParams[T](ranges: Iterable[TypeValueRange], leaf: TypeValueLeaf[T], nodeTuple: NodeTupleT[T], depthRangeSets: List[TypeValueRangeSet[T]], args: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isSupertype: Boolean)(leafIdx: Int)(prevParam: Int) = {
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
        else {
          args.get(leaf.ident).flatMap {
            leafArgs =>
              val leafArgCount = leafArgs.size 
              ranges.foldLeft(none[(Int, TypeValueRangeSet[T])]) {
                case (None, range) =>
                  params.get(range).flatMap {
                    paramSets =>
                      paramSets.get(leafArgCount).flatMap {
                        _.foldLeft(none[Int]) {
                          case (None, paramSet) => paramSet.from(prevParam + 1).headOption.orElse { paramSet.headOption }
                          case (Some(param), _) => Some(param)
                        }.flatMap {
                          param =>
                            checkSupertypeValueLeaf(TypeValueLeaf(TypeParamAppIdentity(param), 0, 1), rangeSets, depthRangeSets2, some(param), false, isSupertype)(leafIdx).map {
                              (param, _)
                            }
                        }
                      }
                  }
                case (Some(pair), _) =>
                  Some(pair)
              }
          }
       }
    }.getOrElse {
      args.get(leaf.ident).map {
        leafArgs =>
          leaf match {
            case TypeValueLeaf(ident @ TypeParamAppIdentity(_), _, _) if leafArgs.isEmpty =>
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
      }.getOrElse((prevParam, TypeValueRangeSet.empty[T]))
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
  
  private def checkSupertypeValueLeaf[T](leaf: TypeValueLeaf[T], rangeSets: Map[TypeValueIdentity[T], TypeValueRangeSet[T]], depthRangeSets2: List[TypeValueRangeSet[T]], param: Option[Int], isNothing: Boolean, isSupertype: Boolean)(leafIdx: Int) =
    leaf match {
      case TypeValueLeaf(ident, _, _) =>
        rangeSets.get(ident).map {
          rs =>
            val param2 = param.orElse {
              ident match {
                case TypeParamAppIdentity(identParam) => some(identParam)
                case _                                => none
              }
            }
            val rangeSet = depthRangeSets2.headOption.map(rs.swapLeafIdxPairsWithMyLeafIdx(leafIdx).superset).getOrElse(rs.swapLeafIdxPairsWithMyLeafIdx(leafIdx))
            val rangeSet2 = param2.map { rangeSet.withMyParam(leafIdx, _) }.getOrElse(rangeSet)
            if(isNothing) rangeSet2.withMyNothingIdx(leafIdx) else rangeSet2
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
          tuple3 => tuple3.copy(_2 = myCondIdxs.get(TypeValueRange(leafIdx, leafIdx + node.leafCount - 1)).map(tuple3._2 |).getOrElse(tuple3._2))
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
        val optTuple3 = stFoldLeftS(childs)(none[(Set[TypeValueIdentity[T]], Set[Int], Seq[TypeParamCondition[T]])]) {
          (optTuple2, child, newLeafIdx: Int) =>
            optTuple2 match {
              case None => (newLeafIdx + child.leafCount, checkLeafIndexSetsForTypeConjunction(indexTuple, child, isSupertype, canExpandGlobalType)(newLeafIdx, tuple))
              case _    => (newLeafIdx + child.leafCount, optTuple2)
            }
        } (leafIdx)._2
        optTuple3.map {
          tuple3 => tuple3.copy(_2 = otherCondIdxs.get(TypeValueRange(leafIdx, leafIdx + node.leafCount - 1)).map(tuple3._2 +).getOrElse(tuple3._2))
        }
      case leaf @ TypeValueLeaf(ident, _, _) =>
        val myParam = myParams.get(leafIdx)
        val isOtherLeaf = otherLeafIdxs.contains(leafIdx)
        if(myLeafIdxs.contains(leafIdx) && (isOtherLeaf || myParam.isDefined || myNothingIdxs.contains(leafIdx))) {
          val canAddIdent = isOtherLeaf && (!ident.isTypeParamAppIdentity || myParam.isDefined)
          some(tuple.copy(_1 = if(canAddIdent) tuple._1 + ident else tuple._1, _3 = tuple._3 ++ (myParam |@| myParamAppIdxs.get(leafIdx)) { TypeParamCondition(_, _, leaf, if(isSupertype) TypeMatching.TypeWithSupertype else TypeMatching.SupertypeWithType) }))
        } else
          none
      case globalTypeAppNode: GlobalTypeAppNode[T] =>
        val node2 = globalTypeAppNode.typeValueBranchOrTypeValueLeaf(canExpandGlobalType)
        checkLeafIndexSetsForTypeDisjunction(indexTuple, node2, isSupertype, canExpandGlobalType)(leafIdx, tuple)
    }  
  }
  
  private def fullyCheckOrDistributeSupertypeConjunctionNode[T](node: TypeValueNode[T], nodeTuple: NodeTupleT[T], depthRangeSets: List[TypeValueRangeSet[T]], args: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isSupertype: Boolean, canExpandGlobalType: Boolean) = {
    checkOrDistributeSupertypeConjunctionNode(node, nodeTuple, depthRangeSets, args, isSupertype, true, canExpandGlobalType)(0)(-1)._2 match {
      case pairs @ List((optRangeSet, newNode)) => some((optRangeSet, LogicalTypeValueTerm(newNode, args)))
      case _                                    => none
    }
  }
  
  private def fullyCheckOrDistributeSupertypeDisjunctionNode[T](node: TypeValueNode[T], nodeTuple: NodeTupleT[T], depthRangeSets: List[TypeValueRangeSet[T]], args: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isSupertype: Boolean, canExpandGlobalType: Boolean) = {
    checkOrDistributeSupertypeDisjunctionNode(node, nodeTuple, depthRangeSets, args, isSupertype, true, canExpandGlobalType)(0)(-1)._2 match {
      case pairs @ List((optRangeSet, newNode)) => some((optRangeSet, LogicalTypeValueTerm(newNode, args)))
      case _                                    => none
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
        val tmpTerm1 = LogicalTypeValueTerm(TypeValueBranch(Vector(TypeValueBranch(Vector(term1.conjNode), tupleTypes1, term1.conjNode.leafCount)), Nil, term1.conjNode.leafCount), term1.args)
        val tmpTerm2 = term2
        val normalizedTerm1 = tmpTerm1.normalizedTypeValueNodeForChecking(canExpandGlobalType)
        val normalizedTerm2 = tmpTerm2.normalizedTypeValueNodeForChecking(canExpandGlobalType)
        val nodeTuple2 = (normalizedTerm2.info.conjRangeSets, normalizedTerm2.info.conjParams, normalizedTerm2.info.allParams, normalizedTerm2.info.fieldSetTypeIdents)
        val nodeTuple1 = (normalizedTerm1.info.disjRangeSets, normalizedTerm1.info.disjParams, normalizedTerm1.info.allParams, normalizedTerm1.info.fieldSetTypeIdents)
        for {
          pair1 <- fullyCheckOrDistributeSupertypeConjunctionNode(normalizedTerm1.conjNode, nodeTuple2, conjDepthRangeSets, normalizedTerm1.args, isSupertype, canExpandGlobalType)
          pair2 <- fullyCheckOrDistributeSupertypeDisjunctionNode(normalizedTerm2.conjNode, nodeTuple1, disjDepthRangeSets, normalizedTerm2.args, !isSupertype, canExpandGlobalType)
        } yield (pair1, pair2)
      case (TypeValueBranch(childs1, tupleTypes1, _), TypeValueBranch(childs2, tupleTypes2, _)) if childs1.size === 1 && childs2.size > 1 && isFirstTry =>
        val conjDepthRangeSet = term2.info.conjDepthRangeSets.headOption.getOrElse(TypeValueRangeSet.full)
        val conjDepthRangeSets = TypeValueRangeSet.full[T] :: conjDepthRangeSet :: term2.info.conjDepthRangeSets
        val disjDepthRangeSets = TypeValueRangeSet.full[T] :: TypeValueRangeSet.full[T] :: term1.info.disjDepthRangeSets
        val tmpTerm1 = term1
        val tmpTerm2 = LogicalTypeValueTerm(TypeValueBranch(Vector(TypeValueBranch(Vector(term2.conjNode), tupleTypes2, term2.conjNode.leafCount)), Nil, term2.conjNode.leafCount), term2.args)
        val normalizedTerm1 = tmpTerm1.normalizedTypeValueNodeForChecking(canExpandGlobalType)
        val normalizedTerm2 = tmpTerm2.normalizedTypeValueNodeForChecking(canExpandGlobalType)
        val nodeTuple2 = (normalizedTerm2.info.conjRangeSets, normalizedTerm2.info.conjParams, normalizedTerm2.info.allParams, normalizedTerm2.info.fieldSetTypeIdents)
        val nodeTuple1 = (normalizedTerm1.info.disjRangeSets, normalizedTerm1.info.disjParams, normalizedTerm1.info.allParams, normalizedTerm1.info.fieldSetTypeIdents)
        for {
          pair1 <- fullyCheckOrDistributeSupertypeConjunctionNode(normalizedTerm1.conjNode, nodeTuple2, conjDepthRangeSets, normalizedTerm1.args, isSupertype, canExpandGlobalType)
          pair2 <- fullyCheckOrDistributeSupertypeDisjunctionNode(normalizedTerm2.conjNode, nodeTuple1, disjDepthRangeSets, normalizedTerm2.args, !isSupertype, canExpandGlobalType)
        } yield (pair1, pair2)
      case _ =>
        val conjDepthRangeSets = TypeValueRangeSet.full[T] :: term2.info.conjDepthRangeSets
        val disjDepthRangeSets = TypeValueRangeSet.full[T] :: TypeValueRangeSet.full[T] :: term1.info.disjDepthRangeSets
        val normalizedTerm1 = term1.normalizedTypeValueNodeForChecking(canExpandGlobalType)
        val normalizedTerm2 = term2.normalizedTypeValueNodeForChecking(canExpandGlobalType)
        val nodeTuple2 = (normalizedTerm2.info.conjRangeSets, normalizedTerm2.info.conjParams, normalizedTerm2.info.allParams, normalizedTerm2.info.fieldSetTypeIdents)
        val nodeTuple1 = (normalizedTerm1.info.disjRangeSets, normalizedTerm1.info.disjParams, normalizedTerm1.info.allParams, normalizedTerm1.info.fieldSetTypeIdents)
        for {
          pair1 <- fullyCheckOrDistributeSupertypeConjunctionNode(normalizedTerm1.conjNode, nodeTuple2, conjDepthRangeSets, normalizedTerm1.args, isSupertype, canExpandGlobalType)
          pair2 <- fullyCheckOrDistributeSupertypeDisjunctionNode(normalizedTerm2.conjNode, nodeTuple1, disjDepthRangeSets, normalizedTerm2.args, !isSupertype, canExpandGlobalType)
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
          checkSupertypeConjunctionNode(distributedTerm1.conjNode, nodeTuple2, conjDepthRangeSets, distributedTerm1.args, isSupertype, canExpandGlobalType)(0)(-1)._2
        }
        val disjRangeSet = optRangeSetPair.map { _._2 }.getOrElse {
          checkSupertypeDisjunctionNode(distributedTerm2.conjNode, nodeTuple1, disjDepthRangeSets, distributedTerm2.args, !isSupertype, canExpandGlobalType)(0)(-1)._2
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
                matchesTypeValueTermsS(typeParamApp, _)(x)(f)(newEnv3)
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
        val (newEnv2, newRes) = partiallyInstantiateTypeValueNodeS(node, argMap, isConj)(markedParams)(newOptNodeMap2, newArgMap2)(newEnv)
        (newEnv2, newRes.map { 
          _ match { 
            case (newOptNodes3, newArgMap3, Some((newNode, newInstantiatedParams2))) =>
              (newOptNodes3, newArgMap3, newNodes :+ newNode, newInstantiatedParams2, true)
            case (newOptNodes3, newArgMap3, None)                                    =>
              (newOptNodes3, newArgMap3, newNodes :+ node, newInstantiatedParams, isInstantiation)
          }
        })
    } (env)
  
  private def partiallyInstantiateTypeValueNodeS[T, U, E](node: TypeValueNode[T], argMap: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isConj: Boolean)(markedParams: Set[Int])(newOptNodeMap: Map[TypeValueIdentity[T], Option[TypeValueNode[T]]], newArgMap: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T], locEqual: Equal[T]): (E, Validation[NoType[T], (Map[TypeValueIdentity[T], Option[TypeValueNode[T]]], Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], Option[(TypeValueNode[T], Set[Int])])]) =
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
                      case Some(term) =>
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
                                  (env5, NoType.fromError[T](Error("same type functions haven't same arguments at logical type expression", none, NoPosition)).failure)
                              case Failure(noType) =>
                                (env5, noType.failure)
                            }
                          case Failure(noType) =>
                            (env4, noType.failure)
                        }
                      case None =>
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
                val (env7, res5) = partiallyInstantiateTypeValueNodeS(node2, newArgMap2, isConj)(markedParams ++ optInstantiatedParam)(newNodeMap2, newArgMap2)(env6)
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
        (env, argMap.get(ident).map {
          args =>
            (newOptNodeMap + (ident -> none), newArgMap + (ident -> args), none).success
        }.getOrElse(NoType.fromError[T](FatalError("no type arguments", none, NoPosition)).failure))
      case GlobalTypeAppNode(loc, childs, tupleTypes, leafCount, sym) =>
        val (env2, res) = partiallyInstantiateTypeValueNodeS(TypeValueLeaf(UnexpandedGlobalTypeAppIdentity(loc, sym), 0, leafCount), argMap, isConj)(markedParams)(newOptNodeMap, newArgMap)(env)
        res match {
          case Success((newOptNodeMap2, newArgMap2, optPair @ (Some((TypeValueLeaf(UnexpandedGlobalTypeAppIdentity(_, _), _, _), _)) | None))) =>
            partiallyInstantiateTypeValueNodeS(TypeValueBranch(childs, tupleTypes, leafCount), argMap, isConj)(markedParams ++ optPair.map { _._2 }.getOrElse(Set()))(newOptNodeMap2, newArgMap2)(env2)
          case Success(_)                                                                                                                      =>
            (env2, NoType.fromError(FatalError("incorrect type value node", none, NoPosition)).failure)
          case Failure(noType)                                                                                                                 =>
            (env2, noType.failure)
        }
    }
    
  private def partiallyInstantiateLogicalTypeValueTermS[T, U, E](term: LogicalTypeValueTerm[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T], locEqual: Equal[T]) =
    if(term.args.keys.exists { _.isInstanceOf[TypeParamAppIdentity[T]] }) {
      val (env2, res) = partiallyInstantiateTypeValueNodeS(term.conjNode, term.args, true)(Set())(Map(), Map())(env)
      (env2, res.map { t => t._3.map { case (n, ips) => (LogicalTypeValueTerm(n, t._2), ips) }.getOrElse((term, Set[Int]())) })
    } else
      (env, (term, Set[Int]()).success)
  
  private def matchesLocigalTypeValueTermsWithoutInstantationS[T, U, V, E](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]) = {
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
                        if(args1.size === args2.size) {
                          stFoldLeftValidationS(args1.zip(args2))(x.success[NoType[T]]) {
                            (x2, argPair, newEnv2: E) =>
                              val (arg1, arg2) = argPair
                              matchesTypeValueLambdasS(arg1, arg2)(x2)(f)(newEnv2)
                          } (newEnv)
                        } else
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

  def matchesLocigalTypeValueTermsS[T, U, V, E](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]) =
    st(for {
      pair1 <- steS(partiallyInstantiateLogicalTypeValueTermS(term1)(_: E))
      pair2 <- steS(partiallyInstantiateLogicalTypeValueTermS(term2)(_: E))
      y <- steS({
        (env2: E) =>
          val (instantiatedTerm1, instantiatedParams1) = pair1
          val (instantiatedTerm2, instantiatedParams2) = pair2
          envSt.withInfinityCheckingS(instantiatedParams1 ++ instantiatedParams2) {
            matchesLocigalTypeValueTermsWithoutInstantationS(instantiatedTerm1, instantiatedTerm2)(z)(f)(_: E)
          } (env2)
      })
    } yield y).run(env)
  
  private def replaceTypeParamsFromTypeValueNodesS[T, U, E](nodes: Iterable[TypeValueNode[T]], argMap: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isConj: Boolean)(newNodeMap: Map[TypeValueIdentity[T], TypeValueNode[T]], newArgMap: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]])(f: (Int, E) => (E, Validation[NoType[T], Either[Int, TypeValueTerm[T]]]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T], locEqual: Equal[T]) =
    stFoldLeftValidationS(nodes)((newNodeMap, newArgMap, Vector[TypeValueNode[T]]()).success[NoType[T]]) {
      (tuple, node, newEnv: E) =>
        val (newNodeMap2, newArgMap2, newNodes) = tuple
        val (newEnv2, newRes) = replaceTypeValueNodeParamsS(node, argMap, isConj)(newNodeMap2, newArgMap2)(f)(newEnv)
        (newEnv2, newRes.map { _.mapElements(identity, identity, newNodes :+ _) })
    } (env)
    
  def replaceTypeValueNodeParamsS[T, U, E](node: TypeValueNode[T], argMap: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], isConj: Boolean)(newNodeMap: Map[TypeValueIdentity[T], TypeValueNode[T]], newArgMap: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]])(f: (Int, E) => (E, Validation[NoType[T], Either[Int, TypeValueTerm[T]]]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T], locEqual: Equal[T]): (E, Validation[NoType[T], (Map[TypeValueIdentity[T], TypeValueNode[T]], Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]], TypeValueNode[T])]) =
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
              case _: UnexpandedGlobalTypeAppIdentity[T] =>
                argMap.get(ident) match {
                  case Some(args) =>
                    val (env2, res) = replaceTypeParamsFromTypeValueLambdasS(args)(f)(env)
                    (env2, res.map {
                      args2 => (newNodeMap + (ident -> leaf), newArgMap + (ident -> args2), leaf)
                    })
                  case None       =>
                    (env, NoType.fromError[T](FatalError("not found arguments", none, NoPosition)).failure)
                }
              case _                                     =>
                argMap.get(ident).flatMap(leaf.typeValueTerm) match {
                  case Some(term) =>
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
                              (env3, NoType.fromError[T](Error("same type functions haven't same arguments at logical type expression", none, NoPosition)).failure)
                          case Failure(noType) =>
                            (env3, noType.failure)
                        }
                      case Failure(noType) =>
                        (env2, noType.failure)
                    }
                  case None =>
                    (env, NoType.fromError[T](FatalError("not found arguments", none, NoPosition)).failure)
                }
            }
        }
        (env4, res3.map { 
          case (newNodeMap2, newArgMap2, branch: TypeValueBranch[T]) if !isConj =>
            (newNodeMap2, newArgMap2, TypeValueBranch(Vector(branch), Vector(), branch.leafCount).normalizedTypeValueNode)
          case tuple                                                          =>
            tuple
        })
      case GlobalTypeAppNode(loc, childs, tupleTypes, leafCount, sym) =>
        val (env2, res) = replaceTypeValueNodeParamsS(TypeValueLeaf(UnexpandedGlobalTypeAppIdentity(loc, sym), 0, leafCount), argMap, isConj)(newNodeMap, newArgMap)(f)(env)
        res match {
          case Success((newNodeMap2, newArgMap2, TypeValueLeaf(UnexpandedGlobalTypeAppIdentity(_, _), _, _))) =>
            replaceTypeValueNodeParamsS(TypeValueBranch(childs, tupleTypes, leafCount), argMap, isConj)(newNodeMap2, newArgMap2)(f)(env2)
          case Success(_)                                                                                     =>
            (env2, NoType.fromError(FatalError("incorrect type value node", none, NoPosition)).failure)
          case Failure(noType)                                                                                =>
            (env2, noType.failure)
        }
    }
  
  def replaceLogicalTypeValueTermParamsS[T, U, E](term: LogicalTypeValueTerm[T])(f: (Int, E) => (E, Validation[NoType[T], Either[Int, TypeValueTerm[T]]]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T], locEqual: Equal[T]): (E, Validation[NoType[T], TypeValueTerm[T]]) = {
    val (env2, res) = replaceTypeValueNodeParamsS(term.conjNode, term.args, true)(Map(), Map())(f)(env)
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
            NoType.fromError(FatalError("no type param application", none, NoPosition)).failure
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
        val (env3, res2) = stFoldLeftValidationS(term.args)((allocatedParams, Set[Int](), Set[Int](), Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]]()).success[NoType[T]]) {
          (tmpTuple, pair, newEnv: E) =>
            val (ident, args) = pair
            val optIdent2 = ident match {
              case TypeParamAppIdentity(param) => allocatedParams2.get(param).map { TypeParamAppIdentity(_) }
              case _                           => some(ident)
            }
            optIdent2.map {
              ident2 =>
                val (newAllocatedParams, newAllocatedArgParams, allocatedParamAppIdxs, newArgs) = tmpTuple
                val (newEnv2, newRes) = unsafeAllocateTypeParamsFromTypeValueLambdasS(args)(newAllocatedParams, unallocatedParamAppIdx)(newEnv)
                  (newEnv2, newRes.map { 
                    _.mapElements(identity, newAllocatedArgParams | _, allocatedParamAppIdxs | _, as => newArgs + (ident2 -> as))
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
