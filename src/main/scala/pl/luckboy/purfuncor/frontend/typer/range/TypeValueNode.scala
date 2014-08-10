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
import pl.luckboy.purfuncor.frontend.resolver._
import pl.luckboy.purfuncor.frontend.typer._

sealed trait TypeValueNode[T]
{
  def leafCount: Int
  
  def withChildAndTupleTypes(child: TypeValueNode[T], tupleTypes: Seq[TupleType[T]], isSupertype: Boolean, canExpandGlobalType: Boolean) =
    conjOrDisjWithTupleTypes(TypeValueBranch(Vector(child), tupleTypes, child.leafCount), tupleTypes, isSupertype, canExpandGlobalType)
  
  def conjOrDisjWithTupleTypes(node: TypeValueNode[T], tupleTypes: Seq[TupleType[T]], isSupertype: Boolean, canExpandGlobalType: Boolean): TypeValueNode[T] =
    (this, node) match {
      case (TypeValueBranch(childs1, _, leafCount1), TypeValueBranch(childs2, _, leafCount2)) => 
        TypeValueBranch(childs1 ++ childs2, tupleTypes, leafCount1 + leafCount2)
      case (TypeValueBranch(childs1, _, leafCount1), TypeValueLeaf(_, _, leafCount2))         =>
        TypeValueBranch(childs1 :+ node, tupleTypes, leafCount1 + leafCount2)
      case (TypeValueLeaf(_, _, leafCount1), TypeValueBranch(childs2, _, leafCount2))         =>
        TypeValueBranch(this +: childs2, tupleTypes, leafCount1 + leafCount2)
      case (TypeValueLeaf(_, _, leafCount1), TypeValueLeaf(_, _, leafCount2))                 =>
        TypeValueBranch(Vector(this, node), tupleTypes, leafCount1 + leafCount2)
      case (_, _)                                                                             =>
        val expandedNode1 = typeValueBranchOrTypeValueLeaf(isSupertype, canExpandGlobalType).normalizedTypeValueNode
        val expandedNode2 = node.typeValueBranchOrTypeValueLeaf(isSupertype, canExpandGlobalType).normalizedTypeValueNode
        expandedNode1.conjOrDisjWithTupleTypes(expandedNode2, tupleTypes, isSupertype, canExpandGlobalType)
    }
  
  def &| (node: TypeValueNode[T]): TypeValueBranch[T] =
    (this, node) match {
      case (TypeValueBranch(childs1, tupleTypes1, leafCount1), TypeValueBranch(childs2, tupleTypes2, leafCount2)) => 
        TypeValueBranch(childs1 ++ childs2, tupleTypes1 ++ tupleTypes2, leafCount1 + leafCount2)
      case (TypeValueBranch(childs1, tupleTypes1, leafCount1), TypeValueLeaf(_, _, leafCount2))                   =>
        TypeValueBranch(childs1 :+ node, tupleTypes1, leafCount1 + leafCount2)
      case (TypeValueLeaf(_, _, leafCount1), TypeValueBranch(childs2, tupleTypes2, leafCount2))                   =>
        TypeValueBranch(this +: childs2, tupleTypes2, leafCount1 + leafCount2)
      case (TypeValueLeaf(_, _, leafCount1), TypeValueLeaf(_, _, leafCount2))                                     =>
        TypeValueBranch(Vector(this, node), Vector(), leafCount1 + leafCount2)
      case (globalTypeApp1: GlobalTypeAppNode[T], globalTypeApp2: GlobalTypeAppNode[T])                           =>
        val node1 = TypeValueBranch(Vector(globalTypeApp1), Vector(), globalTypeApp1.leafCount)
        val node2 = TypeValueBranch(Vector(globalTypeApp2), Vector(), globalTypeApp2.leafCount)
        node1 &| node2
      case (globalTypeApp1: GlobalTypeAppNode[T], node2)                                                          =>
        TypeValueBranch(Vector(globalTypeApp1), Vector(), globalTypeApp1.leafCount) &| node2
      case (node1, globalTypeApp2: GlobalTypeAppNode[T])                                                          =>
        node1 &| TypeValueBranch(Vector(globalTypeApp2), Vector(), globalTypeApp2.leafCount)
    }
  
  def & (node: TypeValueNode[T]) =
    (this &| node) match {
      case TypeValueBranch(childs, tupleTypes, leafCount) if childs.size > 1 =>
        val childs2 = childs.filter { 
          case TypeValueLeaf(BuiltinTypeIdentity(TypeBuiltinFunction.Any, Seq()), _, _) => false
          case TypeValueLeaf(TupleTypeIdentity, _, _)                                   => false
          case _                                                                        => true
        }
        val isTupleTypeLeaf = childs.exists {
          case TypeValueLeaf(TupleTypeIdentity, _, _) => true
          case _                                      => false
        }
        val childs3 =  if(isTupleTypeLeaf) childs2 :+ TypeValueLeaf(TupleTypeIdentity, 0, 1) else childs2
        val leafCount3 = childs3.foldLeft(0) { _ + _.leafCount }
        TypeValueBranch(childs3, tupleTypes, leafCount3)
    }
  
  def | (node: TypeValueNode[T]) =
    (this &| node) match {
      case TypeValueBranch(childs, tupleTypes, leafCount) if childs.size > 1 =>
        val childs2 = childs.filter { 
          case TypeValueLeaf(BuiltinTypeIdentity(TypeBuiltinFunction.Nothing, Seq()), _, _) => false
          case TypeValueLeaf(TupleTypeIdentity, _, _)                                       => false
          case _                                                                            => true
        }
        val isTupleTypeLeaf = childs.exists {
          case TypeValueLeaf(TupleTypeIdentity, _, _) => true
          case _                                      => false
        }
        val childs3 =  if(isTupleTypeLeaf) childs2 :+ TypeValueLeaf(TupleTypeIdentity, 0, 1) else childs2
        val leafCount3 = childs3.foldLeft(0) { _ + _.leafCount }
        TypeValueBranch(childs3, tupleTypes, leafCount3)
    }
  
  def normalizedTypeValueNode: TypeValueNode[T] =
    this match {
      case TypeValueBranch(Vector(child), tupleTypes, _) =>
        child match {
          case TypeValueBranch(Vector(child2), _, _) => child2.normalizedTypeValueNode
          case TypeValueBranch(_, _, childLeafCount) => TypeValueBranch(Vector(child), tupleTypes, childLeafCount)
          case TypeValueLeaf(_, _, _)                => child
        }
      case _                                            =>
        this
    }
  
  def isTypeValueLeaf = isInstanceOf[TypeValueLeaf[T]]
    
  private def typeValueBranchOrTypeValueLeafForTypeConjunction(canExpandGlobalType: Boolean) = {
    val node = typeValueBranchOrTypeValueLeafForTypeDisjunction(canExpandGlobalType)
    TypeValueBranch(Vector(node), Vector(), node.leafCount).normalizedTypeValueNode
  }
  
  private def typeValueBranchOrTypeValueLeafForTypeDisjunction(canExpandGlobalType: Boolean): TypeValueNode[T] =
    this match {
      case GlobalTypeAppNode(loc, childs, tupleTypes, leafCount, sym) =>
        if(canExpandGlobalType)
          TypeValueBranch(Vector(TypeValueLeaf(ExpandedGlobalTypeAppIdentity(loc, sym), 0, 1), TypeValueBranch(childs, tupleTypes, leafCount - 1)), Nil, leafCount)
        else
          TypeValueLeaf(UnexpandedGlobalTypeAppIdentity(loc, sym), 0, leafCount)
      case _ =>
        this
    }

  def typeValueBranchOrTypeValueLeaf(isSupertype: Boolean, canExpandGlobalType: Boolean) =
    if(isSupertype)
      typeValueBranchOrTypeValueLeafForTypeConjunction(canExpandGlobalType)
    else
      typeValueBranchOrTypeValueLeafForTypeDisjunction(canExpandGlobalType)

  private def normalizedTypeValueChildForChecking(canExpandGlobalType: Boolean): TypeValueNode[T] =
    this match {
      case TypeValueBranch(childs, tupleTypes, leafCount) =>
        TypeValueBranch[T](childs :+ TypeValueLeaf(BuiltinTypeIdentity(TypeBuiltinFunction.Nothing, Nil), 0, 1), tupleTypes, leafCount + 1)
      case leaf @ TypeValueLeaf(BuiltinTypeIdentity(TypeBuiltinFunction.Nothing, Seq()), _, _) =>
        leaf
      case leaf: TypeValueLeaf[T] =>
        TypeValueBranch[T](Vector(leaf, TypeValueLeaf(BuiltinTypeIdentity(TypeBuiltinFunction.Nothing, Nil), 0, 1)), Vector(), leafCount + 1)
      case globalTypeAppNode: GlobalTypeAppNode[T] =>
        globalTypeAppNode.typeValueBranchOrTypeValueLeaf(false, canExpandGlobalType).normalizedTypeValueChildForChecking(canExpandGlobalType)
    }
  
  def normalizedTypeValueNodeForChecking(canExpandGlobalType: Boolean): TypeValueNode[T] =
    this match {
      case TypeValueBranch(childs, tupleTypes, leafCount) =>
        // A & (B | C) ---> (A | #Nothing) & (B | C | #Nothing) & #Any & (#Any | #Nothing)
        val childs3 = childs.map { _.normalizedTypeValueChildForChecking(canExpandGlobalType) } ++ Vector(
            // #Any
            TypeValueLeaf(BuiltinTypeIdentity[T](TypeBuiltinFunction.Any, Nil), 0, 1),
            // #Any | #Nothing
            TypeValueBranch(Vector(
                TypeValueLeaf[T](BuiltinTypeIdentity(TypeBuiltinFunction.Any, Nil), 0, 1),
                TypeValueLeaf[T](BuiltinTypeIdentity(TypeBuiltinFunction.Nothing, Nil), 0, 1)
                ), Vector(), 2))
        TypeValueBranch(childs3, tupleTypes, childs3.foldLeft(0) { _ + _.leafCount })
      case leaf: TypeValueLeaf[T] =>
        TypeValueBranch(Vector(leaf), Vector(), leaf.leafCount).normalizedTypeValueNodeForChecking(canExpandGlobalType)
      case globalTypeAppNode: GlobalTypeAppNode[T] =>
        globalTypeAppNode.typeValueBranchOrTypeValueLeaf(true, canExpandGlobalType).normalizedTypeValueNodeForChecking(canExpandGlobalType)
    }
  
  def idents: Set[TypeValueIdentity[T]] =
    this match {
      case TypeValueLeaf(ident, _, _)                =>
        Set(ident)
      case TypeValueBranch(childs, _, _)             =>
        childs.flatMap { _.idents }.toSet
      case GlobalTypeAppNode(loc, childs, _, _, sym) =>
        Set(ExpandedGlobalTypeAppIdentity(loc, sym), UnexpandedGlobalTypeAppIdentity(loc, sym)) ++ childs.flatMap { _.idents }
    }
}

object TypeValueNode
{
  def fromTypeValueTerm[T](term: TypeValueTerm[T]) =
    term match {
      case leafTerm: LeafTypeValueTerm[T] =>
        TypeValueLeaf.fromLeafTypeValueTerm(leafTerm)
      case tupleType: TupleType[T]        =>
        TypeValueBranch[T](Vector(TypeValueLeaf(BuiltinTypeIdentity(TypeBuiltinFunction.Any, Nil), 0, 1)), Vector(tupleType), 0)
      case _: LogicalTypeValueTerm[T]     =>
        term
    }
}

case class TypeValueBranch[T](childs: Seq[TypeValueNode[T]], tupleTypes: Seq[TupleType[T]], leafCount: Int) extends TypeValueNode[T]
case class TypeValueLeaf[T](ident: TypeValueIdentity[T], paramAppIdx: Int, leafCount: Int) extends TypeValueNode[T]
{
  def typeValueTerm(args: Seq[TypeValueLambda[T]]) = 
    ident match {
      case TupleTypeIdentity                         =>
        some(none)
      case FieldTypeIdentity(i)                      =>
        args match {
          case Seq(TypeValueLambda(Seq(), term)) => some(some(FieldType(i, term)))
          case _                                 => none
        }
      case BuiltinTypeIdentity(bf, _)                =>
        TypeValueTerm.typeValueTermsFromTypeValueLambdas(args).map { ts => some(BuiltinType(bf, ts)) }
      case UnittypeIdentity(loc, sym)                =>
        some(some(Unittype(loc, args, sym)))
      case GrouptypeIdentity(loc, sym)               =>
        some(some(Grouptype(loc, args, sym)))
      case ExpandedGlobalTypeAppIdentity(loc, sym)   =>
        some(some(GlobalTypeApp(loc, args, sym)))
      case UnexpandedGlobalTypeAppIdentity(loc, sym) =>
        some(some(GlobalTypeApp(loc, args, sym)))
      case TypeParamAppIdentity(param)               =>
        some(some(TypeParamApp(param, args, paramAppIdx)))
    }
}
object TypeValueLeaf
{
  def fromLeafTypeValueTerm[T](term: LeafTypeValueTerm[T]) =
    term match {
      case TypeParamApp(_, _, paramAppIdx) =>
        TypeValueLeaf(TypeValueIdentity.fromLeafTypeValueTerm(term), paramAppIdx, 1)
      case _                               =>
        TypeValueLeaf(TypeValueIdentity.fromLeafTypeValueTerm(term), 0, 1)
    }
}
case class GlobalTypeAppNode[T](loc: T, childs: Seq[TypeValueNode[T]], tupleTypes: Seq[TupleType[T]], leafCount: Int, sym: GlobalSymbol) extends TypeValueNode[T]
