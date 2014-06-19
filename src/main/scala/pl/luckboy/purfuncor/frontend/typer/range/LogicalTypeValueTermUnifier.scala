/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer.range
import scala.collection.immutable.SortedSet
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend.typer._
import pl.luckboy.purfuncor.util.CollectionUtils._

object LogicalTypeValueTermUnifier
{
  private def checkOrDistributeSupertypeConjunctionNode[T](node: TypeValueNode[T], nodeTuple: (Map[TypeValueIdentity[T], TypeValueRangeSet[T]], Map[TypeValueRange, List[SortedSet[Int]]], SortedSet[Int]), depthRangeSets: List[TypeValueRangeSet[T]], isSupertype: Boolean, isRoot: Boolean)(leafIdx: Int)(prevParam: Int): (Int, List[(Option[TypeValueRangeSet[T]], TypeValueNode[T])]) = {
    val (rangeSets, params, allParams) = nodeTuple
    val depthRangeSet = depthRangeSets.headOption.getOrElse(TypeValueRangeSet.empty)
    (node match {
      case TypeValueBranch(childs, tupleTypes, _) =>
        val ((prevParam2, _), pairs6) = stFoldLeftS(childs)(List[(Option[TypeValueRangeSet[T]], TypeValueNode[T])]()) {
          (pairs, child, stPair: (Int, Int)) =>
            val (newPrevParam, newLeafIdx) = stPair
            val ranges = pairs.flatMap { _._1.toIterable.flatMap { _.ranges.keys } }
            val (newPrevParam2, pairs2) = checkOrDistributeSupertypeDisjunctionNode(ranges, child, nodeTuple, depthRangeSets, isSupertype, isRoot)(newLeafIdx)(newPrevParam)
            if(!pairs.isEmpty) {
              val (pairs5, pairIdxs3) = pairs.foldLeft((List[(Option[TypeValueRangeSet[T]], TypeValueNode[T])](), Set[Int]())) {
                case ((pairs3, pairIdxs), pair @ (optRangeSet, newChild)) =>
                  pairs2.zipWithIndex.foldLeft((pairs3, pairIdxs)) {
                    case ((pairs4, pairIdxs2), (pair2 @ (optRangeSet2, newChild2), pairIdx)) =>
                      val optRangeSet3 = (optRangeSet |@| optRangeSet2) { _ & _ }
                      if(optRangeSet3.map { rs => !rs.isEmpty }.getOrElse(true))
                        (((optRangeSet3, newChild.withChildAndTupleTypes(newChild2, tupleTypes))) :: pairs4, pairIdxs2 + pairIdx)
                      else
                        (pair :: pairs4, pairIdxs2)
                  }
              }
              ((newPrevParam2, newLeafIdx + child.leafCount), pairs2.zipWithIndex.flatMap {
                case ((ors, n), pi) => if(!pairIdxs3.contains(pi)) List((ors, TypeValueBranch(Vector(n), tupleTypes, n.leafCount))) else Nil
              } ++ pairs5)
            } else
              ((newPrevParam2, newLeafIdx + child.leafCount), pairs2.map { case (ors, n) => (ors, TypeValueBranch(Vector(n), tupleTypes, n.leafCount)) })
        } ((prevParam, leafIdx))
        val pairs7 = pairs6.map { case (ors, n) => (ors.map { rs => rs.withConds(TypeValueRange(leafIdx, leafIdx + node.leafCount), rs.ranges.keys, tupleTypes) }, n) }
        if(isRoot)
          (prevParam2, pairs7.headOption.map {
            pair =>
              if(pairs7.size > 1)
                List(pairs7.foldLeft((some(TypeValueRangeSet.empty[T]), TypeValueBranch(Vector(), Nil, 0): TypeValueNode[T])) {
                  case ((_, n), (_, n2)) => (none[TypeValueRangeSet[T]], n.conjOrDisjWithTupleTypes(n2, tupleTypes))
                })
              else
                List(pair)
          }.getOrElse(List((some(TypeValueRangeSet.empty[T]), TypeValueBranch[T](Vector(), Nil, 0)))))
        else
          (prevParam2, pairs6)
      case leaf: TypeValueLeaf[T] =>
        val (prevParam2, rangeSet) = checkSupertypeConjunctionLeaf(leaf, nodeTuple, depthRangeSets, isSupertype)(leafIdx)(prevParam)
        (prevParam2, List((if(!rangeSet.isEmpty) some(rangeSet) else none, node)))
    }).mapElements(identity, _.map { case (ors, n) => (ors.map { _.superset(depthRangeSet) }, n.normalizedTypeValueNode) })
  }

  private def checkOrDistributeSupertypeDisjunctionNode[T](ranges: Iterable[TypeValueRange], node: TypeValueNode[T], nodeTuple: (Map[TypeValueIdentity[T], TypeValueRangeSet[T]], Map[TypeValueRange, List[SortedSet[Int]]], SortedSet[Int]), depthRangeSets: List[TypeValueRangeSet[T]], isSupertype: Boolean, isRoot: Boolean)(leafIdx: Int)(prevParam: Int): (Int, List[(Option[TypeValueRangeSet[T]], TypeValueNode[T])]) = {
    val (rangeSets, params, allParams) = nodeTuple
    val depthRangeSets2 = depthRangeSets.headOption.map { _ => depthRangeSets.tail }.getOrElse(Nil)
    (node match {
      case TypeValueBranch(childs, tupleTypes, _) =>
        val ((prevParam2, _), pair3) = stFoldLeftS(childs)(List[(Option[TypeValueRangeSet[T]], TypeValueNode[T])]()) {
          (pairs, child, stPair: (Int, Int)) =>
            val (newPrevParam, newLeafIdx) = stPair
            val (newPrevParam2, pairs2) = checkOrDistributeSupertypeConjunctionNode(child, nodeTuple, depthRangeSets, isSupertype, isRoot)(newLeafIdx)(newPrevParam)
            val pairs5 = if(!pairs2.isEmpty)
              pairs.foldLeft(List[(Option[TypeValueRangeSet[T]], TypeValueNode[T])]()) {
                case (pairs3, pair @ (optRangeSet, newChild)) =>
                  pairs2.zipWithIndex.foldLeft(pairs3) {
                    case (pairs4, (pair2 @ (optRangeSet2, newChild2), pairIdx)) =>
                      val optRangeSet3 = (optRangeSet |@| optRangeSet2) { _ | _ }.orElse(optRangeSet).orElse(optRangeSet2)
                      ((optRangeSet3, newChild.withChildAndTupleTypes(newChild2, tupleTypes))) :: pairs4
                  }
              }
            else
              pairs2.map { case (ors, n) => (ors, TypeValueBranch(Vector(n), tupleTypes, n.leafCount)) }
            ((newPrevParam2, newLeafIdx + child.leafCount), pairs5)
        } ((prevParam, leafIdx))
        (prevParam2, pair3)
      case leaf: TypeValueLeaf[T] =>
        val (prevParam2, rangeSet) = checkSupertypeDisjunctionNode(ranges, leaf, nodeTuple, depthRangeSets2, isSupertype)(leafIdx)(prevParam)
        (prevParam2, List((if(!rangeSet.isEmpty) some(rangeSet) else none, node)))
    }).mapElements(identity, _.map { case (ors, n) => (ors, n.normalizedTypeValueNode) })
  }
  
  private def checkSupertypeConjunctionNode[T](node: TypeValueNode[T], nodeTuple: (Map[TypeValueIdentity[T], TypeValueRangeSet[T]], Map[TypeValueRange, List[SortedSet[Int]]], SortedSet[Int]), depthRangeSets: List[TypeValueRangeSet[T]], isSupertype: Boolean)(leafIdx: Int)(prevParam: Int): (Int, TypeValueRangeSet[T]) = {
    val (raneSets, params, allParams) = nodeTuple
    val depthRangeSet = depthRangeSets.headOption.getOrElse(TypeValueRangeSet.empty)
    (node match {
      case TypeValueBranch(childs, tupleTypes, _) =>
        val ((prevParam2, _), rangeSet3) = stFoldLeftS(childs)(TypeValueRangeSet.full[T]) {
          (rangeSet, child, stPair: (Int, Int)) =>
            val (newPrevParam, newLeafIdx) = stPair
            val (newPrevParam2, rangeSet2) = checkSupertypeDisjunctionNode(rangeSet.ranges.keys,child, nodeTuple, depthRangeSets, isSupertype)(newLeafIdx)(newPrevParam)
            ((newPrevParam2, newLeafIdx + child.leafCount), rangeSet & rangeSet2)
        } ((prevParam, leafIdx))
        (prevParam2, rangeSet3.withConds(TypeValueRange(leafIdx, leafIdx + node.leafCount), rangeSet3.ranges.keys, tupleTypes))
      case leaf: TypeValueLeaf[T] =>
        checkSupertypeConjunctionLeaf(leaf, nodeTuple, depthRangeSets, isSupertype)(leafIdx)(prevParam)
    }).mapElements(identity, _.superset(depthRangeSet))
  }
  
  private def checkSupertypeDisjunctionNode[T](ranges: Iterable[TypeValueRange], node: TypeValueNode[T], nodeTuple: (Map[TypeValueIdentity[T], TypeValueRangeSet[T]], Map[TypeValueRange, List[SortedSet[Int]]], SortedSet[Int]), depthRangeSets: List[TypeValueRangeSet[T]], isSupertype: Boolean)(leafIdx: Int)(prevParam: Int): (Int, TypeValueRangeSet[T]) = {
    val (raneSets, params, allParams) = nodeTuple
    val depthRangeSets2 = depthRangeSets.headOption.map { _ => depthRangeSets.tail }.getOrElse(Nil)
    node match {
      case TypeValueBranch(childs, _, _) =>
        val ((prevParam2, _), rangeSet3) = stFoldLeftS(childs)(TypeValueRangeSet.full[T]) {
          (rangeSet, child, stPair: (Int, Int)) =>
            val (newPrevParam, newLeafIdx) = stPair
            val (newPrevParam2, rangeSet2) = checkSupertypeConjunctionNode(child, nodeTuple, depthRangeSets2, isSupertype)(newLeafIdx)(newPrevParam)
            ((newPrevParam2, newLeafIdx + child.leafCount), rangeSet | rangeSet2)
        } ((prevParam, leafIdx))
        (prevParam2, rangeSet3)
      case leaf: TypeValueLeaf[T] =>
        checkSupertypeDisjunctionLeaf(ranges, leaf, nodeTuple, depthRangeSets2, isSupertype)(leafIdx)(prevParam)
    }
  }
  
  private def checkSupertypeConjunctionLeaf[T](leaf: TypeValueLeaf[T], nodeTuple: (Map[TypeValueIdentity[T], TypeValueRangeSet[T]], Map[TypeValueRange, List[SortedSet[Int]]], SortedSet[Int]), depthRangeSets: List[TypeValueRangeSet[T]], isSupertype: Boolean)(leafIdx: Int)(prevParam: Int) = {
    val depthRangeSets2 = depthRangeSets.headOption.map { _ => depthRangeSets.tail }.getOrElse(Nil)
    val (rangeSets, params, allParams) = nodeTuple
    checkSupertypeValueLeaf(leaf, rangeSets, depthRangeSets2, isSupertype)(leafIdx).map { (prevParam, _) }.orElse {
      allParams.from(prevParam + 1).headOption.orElse { allParams.headOption }.flatMap {
        param =>
          checkSupertypeValueLeaf(TypeValueLeaf(TypeParamAppIdentity(param)), rangeSets, depthRangeSets2, isSupertype)(leafIdx).map {
            (param, _)
          }
      }
    }.getOrElse((prevParam, TypeValueRangeSet.empty[T]))
  }
  
  private def checkSupertypeDisjunctionLeaf[T](ranges: Iterable[TypeValueRange], leaf: TypeValueLeaf[T], nodeTuple: (Map[TypeValueIdentity[T], TypeValueRangeSet[T]], Map[TypeValueRange, List[SortedSet[Int]]], SortedSet[Int]), depthRangeSets2: List[TypeValueRangeSet[T]], isSupertype: Boolean)(leafIdx: Int)(prevParam: Int) = {
    val (rangeSets, params, allParams) = nodeTuple
    checkSupertypeValueLeaf(leaf, rangeSets, depthRangeSets2, isSupertype)(leafIdx).map { (prevParam, _) }.orElse {
      ranges.foldLeft(none[(Int, TypeValueRangeSet[T])]) {
        case (None, range) =>
          params.get(range).flatMap {
            paramSets =>
              paramSets.foldLeft(none[Int]) {
                case (None, paramSet) => paramSet.from(prevParam + 1).headOption.orElse { paramSet.headOption }
                case (Some(param), _) => Some(param)
              }.flatMap {
                param =>
                  checkSupertypeValueLeaf(TypeValueLeaf(TypeParamAppIdentity(param)), rangeSets, depthRangeSets2, isSupertype)(leafIdx).map {
                    (param, _)
                  }
              }
          }
        case (Some(pair), _) =>
          Some(pair)
      }
    }.getOrElse((prevParam, TypeValueRangeSet.empty[T]))
  }
  
  private def checkSupertypeValueLeaf[T](leaf: TypeValueLeaf[T], rangeSets: Map[TypeValueIdentity[T], TypeValueRangeSet[T]], depthRangeSets2: List[TypeValueRangeSet[T]], isSupertype: Boolean)(leafIdx: Int) = {
    leaf match {
      case TypeValueLeaf(ident) =>
        rangeSets.get(ident).map {
          rs =>
            (depthRangeSets2.headOption.map(rs.swapLeafIdxPairsWithMyLeafIdx(leafIdx).superset).getOrElse(rs.swapLeafIdxPairsWithMyLeafIdx(leafIdx)))
        }
    }
  }
  
  private def checkLeafIndexSetsForTypeConjunction[T](myLeafIdxs: Set[Int], otherLeafIdxs: Set[Int], myCondIdxs: Map[TypeValueRange, Set[Int]], otherCondIdxs: Map[TypeValueRange, Int], node: TypeValueNode[T])(leafIdx: Int, tuple: (Set[TypeValueIdentity[T]], Set[Int], Seq[TypeParamCondition[T]])): Option[(Set[TypeValueIdentity[T]], Set[Int], Seq[TypeParamCondition[T]])] =
    node match {
      case TypeValueBranch(childs, _, _) =>
        val optTuple3 = stFoldLeftS(childs)(some(tuple)) {
          (optTuple2, child, newLeafIdx: Int) =>
            optTuple2 match {
              case Some(tuple2) => (newLeafIdx + child.leafCount, checkLeafIndexSetsForTypeDisjunction(myLeafIdxs, otherLeafIdxs, myCondIdxs, otherCondIdxs, child)(newLeafIdx, tuple2))
              case None         => (newLeafIdx + child.leafCount, none)
            }
        } (leafIdx)._2
        optTuple3.map {
          tuple3 => tuple3.copy(_2 = myCondIdxs.get(TypeValueRange(leafIdx, leafIdx + node.leafCount)).map(tuple3._2 |).getOrElse(tuple3._2))
        }
      case TypeValueLeaf(_) =>
        checkLeafIndexSetsForTypeDisjunction(myLeafIdxs, otherLeafIdxs, myCondIdxs, otherCondIdxs, node)(leafIdx, tuple)
    }

  private def checkLeafIndexSetsForTypeDisjunction[T](myLeafIdxs: Set[Int], otherLeafIdxs: Set[Int], myCondIdxs: Map[TypeValueRange, Set[Int]], otherCondIdxs: Map[TypeValueRange, Int], node: TypeValueNode[T])(leafIdx: Int, tuple: (Set[TypeValueIdentity[T]], Set[Int], Seq[TypeParamCondition[T]])): Option[(Set[TypeValueIdentity[T]], Set[Int], Seq[TypeParamCondition[T]])] =
    node match {
      case TypeValueBranch(childs, _, _) =>
        val optTuple3 = stFoldLeftS(childs)(some(tuple)) {
          (optTuple2, child, newLeafIdx: Int) =>
            optTuple2 match {
              case None => (newLeafIdx + child.leafCount, checkLeafIndexSetsForTypeConjunction(myLeafIdxs, otherLeafIdxs, myCondIdxs, otherCondIdxs, child)(newLeafIdx, tuple))
              case _    => (newLeafIdx + child.leafCount, optTuple2)
            }
        } (leafIdx)._2
        optTuple3.map {
          tuple3 => tuple3.copy(_2 = otherCondIdxs.get(TypeValueRange(leafIdx, leafIdx + node.leafCount)).map(tuple3._2 +).getOrElse(tuple3._2))
        }
      case TypeValueLeaf(ident) =>
        if(myLeafIdxs.contains(leafIdx) && otherLeafIdxs.contains(leafIdx)) some(tuple.copy(_1 = tuple._1 + ident)) else none
    }
  
  private def fullyCheckOrDistributeSupertypeConjunctionNode[T](node: TypeValueNode[T], nodeTuple: (Map[TypeValueIdentity[T], TypeValueRangeSet[T]], Map[TypeValueRange, List[SortedSet[Int]]], SortedSet[Int]), depthRangeSets: List[TypeValueRangeSet[T]], args: Map[TypeValueIdentity[T], Seq[TypeValueTerm[T]]], isSupertype: Boolean) =
    checkOrDistributeSupertypeConjunctionNode(node, nodeTuple, depthRangeSets, isSupertype, true)(0)(-1)._2 match {
      case List((optRangeSet, newNode)) => some((optRangeSet, LogicalTypeValueTerm(newNode, args)))
      case _                            => none
    }
  
  private def fullyCheckOrDistributeSupertypeDisjunctionNode[T](ranges: Iterable[TypeValueRange], node: TypeValueNode[T], nodeTuple: (Map[TypeValueIdentity[T], TypeValueRangeSet[T]], Map[TypeValueRange, List[SortedSet[Int]]], SortedSet[Int]), depthRangeSets: List[TypeValueRangeSet[T]], args: Map[TypeValueIdentity[T], Seq[TypeValueTerm[T]]], isSupertype: Boolean) =
    checkOrDistributeSupertypeDisjunctionNode(ranges, node, nodeTuple, depthRangeSets, isSupertype, true)(0)(-1)._2 match {
      case List((optRangeSet, newNode)) => some((optRangeSet, LogicalTypeValueTerm(newNode, args)))
      case _                            => none
    }
  
  private def checkOrDistributeTypeValueNodesFromLogicalTypeValueTerms[T](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T], isSupertype: Boolean, isFirstTry: Boolean) = {
    val nodeTuple2 = (term2.info.conjRangeSets, term2.info.conjParams, term2.info.allParams)
    val nodeTuple1 = (term1.info.disjRangeSets, term1.info.disjParams, term1.info.allParams)
    (term1.conjNode, term2.conjNode) match {
      case (TypeValueBranch(childs1, tupleTypes1, _), TypeValueBranch(childs2, tupleTypes2, _)) if childs1.size > 1 && childs2.size === 1 && !isFirstTry =>
        val conjDepthRangeSets = TypeValueRangeSet.full[T] :: term2.info.conjDepthRangeSets
        val disjDepthRangeSet = term2.info.conjDepthRangeSets.headOption.getOrElse(TypeValueRangeSet.full)
        val disjDepthRangeSets = TypeValueRangeSet.full[T] :: TypeValueRangeSet.full[T] :: disjDepthRangeSet :: term1.info.conjDepthRangeSets
        for {
          pair1 <- fullyCheckOrDistributeSupertypeConjunctionNode(TypeValueBranch(Vector(TypeValueBranch(Vector(term1.conjNode), tupleTypes1, term1.conjNode.leafCount)), Nil, term1.conjNode.leafCount), nodeTuple2, conjDepthRangeSets, term1.args, isSupertype)
          pair2 <- fullyCheckOrDistributeSupertypeDisjunctionNode(List(TypeValueRange.full), term2.conjNode, nodeTuple1, disjDepthRangeSets, term2.args, !isSupertype)
        } yield (pair1, pair2)
      case (TypeValueBranch(childs1, tupleTypes1, _), TypeValueBranch(childs2, tupleTypes2, _)) if childs1.size === 1 && childs2.size > 1 && isFirstTry =>
        val conjDepthRangeSet = term2.info.conjDepthRangeSets.headOption.getOrElse(TypeValueRangeSet.full)
        val conjDepthRangeSets = TypeValueRangeSet.full[T] :: conjDepthRangeSet :: term2.info.conjDepthRangeSets
        val disjDepthRangeSets = TypeValueRangeSet.full[T] :: TypeValueRangeSet.full[T] :: term1.info.disjDepthRangeSets
        val nodeTuple2 = (term2.info.conjRangeSets, term2.info.conjParams, term2.info.allParams)
        val nodeTuple1 = (term1.info.disjRangeSets, term1.info.disjParams, term1.info.allParams)
        for {
          pair1 <- fullyCheckOrDistributeSupertypeConjunctionNode(term1.conjNode, nodeTuple2, conjDepthRangeSets, term1.args, isSupertype)
          pair2 <- fullyCheckOrDistributeSupertypeDisjunctionNode(List(TypeValueRange.full), TypeValueBranch(Vector(TypeValueBranch(Vector(term2.conjNode), tupleTypes2, term2.conjNode.leafCount)), Nil, term2.conjNode.leafCount), nodeTuple1, disjDepthRangeSets, term2.args, !isSupertype)
        } yield (pair1, pair2)
      case _ =>
        val conjDepthRangeSets = TypeValueRangeSet.full[T] :: term2.info.conjDepthRangeSets
        val disjDepthRangeSets = TypeValueRangeSet.full[T] :: TypeValueRangeSet.full[T] :: term1.info.disjDepthRangeSets
        for {
          pair1 <- fullyCheckOrDistributeSupertypeConjunctionNode(term1.conjNode, nodeTuple2, conjDepthRangeSets, term1.args, isSupertype)
          pair2 <- fullyCheckOrDistributeSupertypeDisjunctionNode(List(TypeValueRange.full), term2.conjNode, nodeTuple1, disjDepthRangeSets, term2.args, !isSupertype)
        } yield (pair1, pair2)
    }
  }
  
  private def checkTypeValueNodesFromLogicalTypeValueTerms[T](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T], isSupertype: Boolean, isFirstTry: Boolean) =
    checkOrDistributeTypeValueNodesFromLogicalTypeValueTerms(term1, term2, isSupertype, isFirstTry).map {
      case ((optRangeSet1, distributedTerm1), (optRangeSet2, distributedTerm2)) =>
        val conjDepthRangeSets = TypeValueRangeSet.full[T] :: distributedTerm2.info.conjDepthRangeSets
        val disjDepthRangeSets = TypeValueRangeSet.full[T] :: TypeValueRangeSet.full[T] :: distributedTerm1.info.disjDepthRangeSets
        val nodeTuple2 = (distributedTerm2.info.conjRangeSets, distributedTerm2.info.conjParams, distributedTerm2.info.allParams)
        val nodeTuple1 = (distributedTerm1.info.disjRangeSets, distributedTerm1.info.disjParams, distributedTerm1.info.allParams)
        val optRangeSetPair = (optRangeSet1 |@| optRangeSet2) { (_, _) }
        val conjRangeSet = optRangeSetPair.map { _._1 }.getOrElse {
          checkSupertypeConjunctionNode(distributedTerm1.conjNode, nodeTuple2, conjDepthRangeSets, isSupertype)(0)(-1)._2
        }
        val disjRangeSet = optRangeSetPair.map { _._2 }.getOrElse {
          checkSupertypeDisjunctionNode(List(TypeValueRange.full), distributedTerm2.conjNode, nodeTuple1, disjDepthRangeSets, !isSupertype)(0)(-1)._2
        }
        ((distributedTerm1, conjRangeSet), (distributedTerm2, disjRangeSet))
    }
    
  private def partiallyMatchesSupertypeValueTermWithTypeValueTerm[T](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T], isFirstTry: Boolean) =
    checkTypeValueNodesFromLogicalTypeValueTerms(term1, term2, true, isFirstTry).flatMap {
      case ((distributedTerm1, conjRangeSet), (distributedTerm2, disjRangeSet)) =>
        if(!conjRangeSet.isEmpty && !disjRangeSet.isEmpty) {
          val conjMyLeafIdxs = conjRangeSet.value.myLeafIdxs.toSet
          val disjOtherLeafIdxs = disjRangeSet.value.otherLeafIdxs.toSet
          val disjMyLeafIdxs = disjRangeSet.value.myLeafIdxs.toSet
          val conjOtherLeafIdxs = conjRangeSet.value.otherLeafIdxs.toSet
          val pairs = conjRangeSet.value.conds.toVector
          val conjMyCondIdxs = pairs.zipWithIndex.foldLeft(Map[TypeValueRange, Set[Int]]()) { 
            case (condIdxs, (((myRange, _), _), i)) => condIdxs |+| Map(myRange -> Set(i))
          }
          val conjOtherCondIdxs = pairs.zipWithIndex.foldLeft(Map[TypeValueRange, Int]()) {
            case (condIdxs, (((_, otherRanges), _), i)) => condIdxs |+| otherRanges.map { _ -> i }.toMap
          }
          for {
            conjTuple <- checkLeafIndexSetsForTypeConjunction(conjMyLeafIdxs, disjOtherLeafIdxs, conjMyCondIdxs, Map(), distributedTerm1.conjNode)(0, (Set(), Set(), Seq()))
            disjTuple <- checkLeafIndexSetsForTypeDisjunction(disjMyLeafIdxs, conjOtherLeafIdxs, Map(), conjOtherCondIdxs, distributedTerm2.conjNode)(0, (Set(), Set(), Seq()))
          } yield {
            val conds = (conjTuple._2 | disjTuple._2).toSeq.flatMap { pairs.lift(_).map { _._2 } }
            (conjTuple._1 | disjTuple._1, conds, conjTuple._3 ++ disjTuple._3)
          }
        } else
          none
    }

  private def matchesSupertypeValueTermWithTypeValueTerm[T](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T]) =
    (term1.conjNode, term2.conjNode) match {
      case (TypeValueBranch(childs1, _, _), TypeValueBranch(childs2, _, _)) if (childs1.size > 1 && childs2.size === 1) || (childs1.size === 1 && childs2.size > 1) =>
        partiallyMatchesSupertypeValueTermWithTypeValueTerm(term1, term2, true).orElse(partiallyMatchesSupertypeValueTermWithTypeValueTerm(term1, term2, false))
      case _ =>
        partiallyMatchesSupertypeValueTermWithTypeValueTerm(term1, term2, true)
    }
  
  private def matchesLocigalTypeValueTermsWithoutArgs[T, U, E](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T], typeMatching: TypeMatching.Value)(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E) =
    typeMatching match {
      case TypeMatching.Types             =>
        for {
          tuple1 <- matchesSupertypeValueTermWithTypeValueTerm(term1, term2)
          tuple2 <- matchesSupertypeValueTermWithTypeValueTerm(term2, term1)
        } yield {
          (tuple1._1 | tuple2._1, tuple1._2 ++ tuple2._2, tuple1._3 ++ tuple2._3)
        }
      case TypeMatching.SupertypeWithType =>
        matchesSupertypeValueTermWithTypeValueTerm(term1, term2)
      case TypeMatching.TypeWithSupertype =>
        matchesSupertypeValueTermWithTypeValueTerm(term2, term1)
    }
    
  def unifyLocigalTypeValueTermsS[T, U, E](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T], typeMatching: TypeMatching.Value)(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E) =
    throw new UnsupportedOperationException
}
