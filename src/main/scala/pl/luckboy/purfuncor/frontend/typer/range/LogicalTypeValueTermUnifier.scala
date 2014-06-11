/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer.range
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend.typer._
import pl.luckboy.purfuncor.util.CollectionUtils._

object LogicalTypeValueTermUnifier
{
  private def checkOrDistributeSupertypeConjunctionNode[T](node: TypeValueNode[T], rangeSets: Map[TypeValueIdentity[T], TypeValueRangeSet[T]], depthRangeSets: List[TypeValueRangeSet[T]], isSupertype: Boolean, isRoot: Boolean)(leafIdx: Int): List[(Option[TypeValueRangeSet[T]], TypeValueNode[T])] = {
    val depthRangeSet = depthRangeSets.headOption.getOrElse(TypeValueRangeSet.empty)
    (node match {
      case TypeValueBranch(childs, tupleTypes, _) =>
        val (_, pairs6) = stFoldLeftS(childs)(List[(Option[TypeValueRangeSet[T]], TypeValueNode[T])]()) {
          (pairs, child, newLeafIdx: Int) =>
            val pairs2 = checkOrDistributeSupertypeDisjunctionNode(child, rangeSets, depthRangeSets, isSupertype, isRoot)(newLeafIdx)
            if(!pairs.isEmpty) {
              val (pairs5, pairIdxs3) = pairs.foldLeft((List[(Option[TypeValueRangeSet[T]], TypeValueNode[T])](), Set[Int]())) {
                case ((pairs3, pairIdxs), pair @ (optRangeSet, newChild)) =>
                  pairs2.zipWithIndex.foldLeft((pairs3, pairIdxs)) {
                    case ((pairs4, pairIdxs2), (pair2 @ (optRangeSet2, newChild2), pairIdx)) =>
                      val optRangeSet3 = (optRangeSet |@| optRangeSet2) { _ & _ }
                      if(optRangeSet3.map { rs => !rs.isEmpty }.getOrElse(true))
                        (((optRangeSet3, newChild.withChild(newChild2))) :: pairs4, pairIdxs2 + pairIdx)
                      else
                        (pair :: pairs4, pairIdxs2)
                  }
              }
              (newLeafIdx + child.leafCount, pairs2.zipWithIndex.flatMap {
                case ((ors, n), pi) => if(!pairIdxs3.contains(pi)) List((ors, TypeValueBranch(Vector(n), tupleTypes, n.leafCount))) else Nil
              } ++ pairs5)
            } else
              (newLeafIdx + child.leafCount, pairs2.map { case (ors, n) => (ors, TypeValueBranch(Vector(n), tupleTypes, n.leafCount)) })
        } (leafIdx)
        val pairs7 = pairs6.map { case (ors, n) => (ors.map { _.withConds(TypeValueRange(leafIdx, leafIdx + node.leafCount), tupleTypes) }, n) }
        if(isRoot)
          pairs7.headOption.map {
            pair =>
              if(pairs7.size > 1)
                List(pairs7.foldLeft((some(TypeValueRangeSet.empty[T]), TypeValueBranch(Vector(), Nil, 0): TypeValueNode[T])) {
                  case ((_, n), (_, n2)) => (none[TypeValueRangeSet[T]], n &| n2)
                })
              else
                List(pair)
          }.getOrElse(List((some(TypeValueRangeSet.empty[T]), TypeValueBranch[T](Vector(), Nil, 0))))
        else
          pairs6
      case TypeValueLeaf(ident) =>
        checkOrDistributeSupertypeDisjunctionNode(node, rangeSets, depthRangeSets, isSupertype, false)(leafIdx)
    }).map { case (ors, n) => (ors.map { _.superset(depthRangeSet) }, n.normalizedTypeValueNode) }
  }

  private def checkOrDistributeSupertypeDisjunctionNode[T](node: TypeValueNode[T], rangeSets: Map[TypeValueIdentity[T], TypeValueRangeSet[T]], depthRangeSets: List[TypeValueRangeSet[T]], isSupertype: Boolean, isRoot: Boolean)(leafIdx: Int): List[(Option[TypeValueRangeSet[T]], TypeValueNode[T])] = {
    val depthRangeSets2 = depthRangeSets.headOption.map { _ => depthRangeSets.tail }.getOrElse(Nil)
    (node match {
      case TypeValueBranch(childs, tupleTypes, _) =>
        stFoldLeftS(childs)(List[(Option[TypeValueRangeSet[T]], TypeValueNode[T])]()) {
          (pairs, child, newLeafIdx: Int) =>
            val pairs2 = checkOrDistributeSupertypeDisjunctionNode(child, rangeSets, depthRangeSets, isSupertype, isRoot)(newLeafIdx)
            val pairs5 = if(!pairs2.isEmpty)
              pairs.foldLeft(List[(Option[TypeValueRangeSet[T]], TypeValueNode[T])]()) {
                case (pairs3, pair @ (optRangeSet, newChild)) =>
                  pairs2.zipWithIndex.foldLeft(pairs3) {
                    case (pairs4, (pair2 @ (optRangeSet2, newChild2), pairIdx)) =>
                      val optRangeSet3 = (optRangeSet |@| optRangeSet2) { _ | _ }.orElse(optRangeSet).orElse(optRangeSet2)
                      ((optRangeSet3, newChild.withChild(newChild2))) :: pairs4
                  }
              }
            else
              pairs2.map { case (ors, n) => (ors, TypeValueBranch(Vector(n), tupleTypes, n.leafCount)) }
            (newLeafIdx + child.leafCount, pairs5)
        } (leafIdx)._2
      case leaf: TypeValueLeaf[T] =>
        List((some(checkSupertypeDisjunctionNode(leaf, rangeSets, depthRangeSets2, isSupertype)(leafIdx)), node))
    }).map { case (ors, n) => (ors, n.normalizedTypeValueNode) }
  }
  
  private def checkSupertypeConjunctionNode[T](node: TypeValueNode[T], rangeSets: Map[TypeValueIdentity[T], TypeValueRangeSet[T]], depthRangeSets: List[TypeValueRangeSet[T]], isSupertype: Boolean)(leafIdx: Int): TypeValueRangeSet[T] = {
    val depthRangeSet = depthRangeSets.headOption.getOrElse(TypeValueRangeSet.empty)
    (node match {
      case TypeValueBranch(childs, tupleTypes, _) =>
        stFoldLeftS(childs)(TypeValueRangeSet.full[T]) {
          (rangeSet, child, newLeafIdx: Int) =>
            (newLeafIdx + child.leafCount, rangeSet & checkSupertypeDisjunctionNode(child, rangeSets, depthRangeSets, isSupertype)(newLeafIdx))
        } (leafIdx)._2.withConds(TypeValueRange(leafIdx, leafIdx + node.leafCount), tupleTypes)
      case TypeValueLeaf(_) =>
        checkSupertypeDisjunctionNode(node, rangeSets, depthRangeSets, isSupertype)(leafIdx)
    }).superset(depthRangeSet)
  }
  
  private def checkSupertypeDisjunctionNode[T](node: TypeValueNode[T], rangeSets: Map[TypeValueIdentity[T], TypeValueRangeSet[T]], depthRangeSets: List[TypeValueRangeSet[T]], isSupertype: Boolean)(leafIdx: Int): TypeValueRangeSet[T] = {
    val depthRangeSets2 = depthRangeSets.headOption.map { _ => depthRangeSets.tail }.getOrElse(Nil)
    node match {
      case TypeValueBranch(childs, _, _) =>
        stFoldLeftS(childs)(TypeValueRangeSet.full[T]) {
          (rangeSet, child, newLeafIdx: Int) =>
            (newLeafIdx + child.leafCount, rangeSet | checkSupertypeConjunctionNode(child, rangeSets, depthRangeSets2, isSupertype)(newLeafIdx))
        } (leafIdx)._2
      case leaf: TypeValueLeaf[T] =>
        checkSupertypeDisjunctionNode(leaf, rangeSets, depthRangeSets2, isSupertype)(leafIdx)
    }
  }
  
  private def checkSupertypeValueLeaf[T](leaf: TypeValueLeaf[T], rangeSets: Map[TypeValueIdentity[T], TypeValueRangeSet[T]], depthRangeSets2: List[TypeValueRangeSet[T]], isSupertype: Boolean)(leafIdx: Int) =
    leaf match {
      case TypeValueLeaf(ident) =>
        rangeSets.get(ident).map {
          rs =>
            (depthRangeSets2.headOption.map(rs.swapLeafIdxPairsWithMyLeafIdx(leafIdx).superset).getOrElse(rs.swapLeafIdxPairsWithMyLeafIdx(leafIdx)))
        }.getOrElse(TypeValueRangeSet.empty)
    }
  
  private def checkLeafIndexesForTypeConjunction[T](myLeafIdxs: Set[Int], otherLeafIdxs: Set[Int], conds: Map[TypeValueRange, Seq[TypeValueRangeCondition[T]]], node: TypeValueTerm[T])(leafIdx: Int, idents: Set[TypeValueIdentity[T]]): Option[(Set[TypeValueIdentity[T]], Seq[TypeValueRangeCondition[T]], Seq[TypeParamCondition[T]])] =
    throw new UnsupportedOperationException

  private def checkLeafIndexesForTypeDisjunction[T](myLeafIdxs: Set[Int], otherLeafIdxs: Set[Int], conds: Map[TypeValueRange, Seq[TypeValueRangeCondition[T]]], node: TypeValueTerm[T])(leafIdx: Int, idents: Set[TypeValueIdentity[T]]): Option[(Set[TypeValueIdentity[T]], Seq[TypeValueRangeCondition[T]], Seq[TypeParamCondition[T]])] =
    throw new UnsupportedOperationException
    
  private def partiallyMatchesSupertypeValueTermWithTypeValueTerm[T](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T]): Option[(Set[TypeValueIdentity[T]], Seq[TypeValueRangeCondition[T]], Seq[TypeParamCondition[T]])] =
    throw new UnsupportedOperationException

  private def matchesSupertypeValueTermWithTypeValueTerm[T](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T]): Option[(Set[TypeValueIdentity[T]], Seq[TypeValueRangeCondition[T]], Seq[TypeParamCondition[T]])] =
    throw new UnsupportedOperationException
    
  def unifyLocigalTypeValueTermsS[T, U, E](term1: LogicalTypeValueTerm[T], term2: LogicalTypeValueTerm[T], typeMatching: TypeMatching.Value)(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E) =
    throw new UnsupportedOperationException
}
