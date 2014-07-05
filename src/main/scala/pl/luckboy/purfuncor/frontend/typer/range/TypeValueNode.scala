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
  
  def withChildAndTupleTypes(child: TypeValueNode[T], tupleTypes: Seq[TupleType[T]], canExpandGlobalType: Boolean) =
    this.conjOrDisjWithTupleTypes(TypeValueBranch(Vector(child), tupleTypes, child.leafCount), tupleTypes, canExpandGlobalType)
  
  def conjOrDisjWithTupleTypes(node: TypeValueNode[T], tupleTypes: Seq[TupleType[T]], canExpandGlobalType: Boolean): TypeValueNode[T] =
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
        val expandedNode1 = typeValueBranchOrTypeValueLeaf(canExpandGlobalType).normalizedTypeValueNode
        val expandedNode2 = typeValueBranchOrTypeValueLeaf(canExpandGlobalType).normalizedTypeValueNode
        expandedNode1.conjOrDisjWithTupleTypes(expandedNode2, tupleTypes, canExpandGlobalType)
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
        val node1 = TypeValueBranch(Vector(TypeValueBranch(Vector(globalTypeApp1), Vector(), globalTypeApp1.leafCount)), Vector(), globalTypeApp1.leafCount)
        val node2 = TypeValueBranch(Vector(TypeValueBranch(Vector(globalTypeApp2), Vector(), globalTypeApp2.leafCount)), Vector(), globalTypeApp2.leafCount)
        node1 &| node2
      case (globalTypeApp1: GlobalTypeAppNode[T], node2)                                                          =>
        TypeValueBranch(Vector(TypeValueBranch(Vector(globalTypeApp1), Vector(), globalTypeApp1.leafCount)), Vector(), globalTypeApp1.leafCount) &| node2
      case (node1, globalTypeApp2: GlobalTypeAppNode[T])                                                          =>
        node1 &| TypeValueBranch(Vector(TypeValueBranch(Vector(globalTypeApp2), Vector(), globalTypeApp2.leafCount)), Vector(), globalTypeApp2.leafCount)
    }
  
  def & (node: TypeValueNode[T]) =
    (this &| node) match {
      case TypeValueBranch(childs, tupleTypes, leafCount) if childs.size > 1 =>
        val childs2 = childs.filter { 
          case TypeValueLeaf(BuiltinTypeIdentity(TypeBuiltinFunction.Any, Seq()), _, _) => false
          case _                                                                        => true
        }
        val leafCount2 = childs2.foldLeft(0) { _ + _.leafCount }
        TypeValueBranch(childs2, tupleTypes, leafCount2)
    }
  
  def | (node: TypeValueNode[T]) =
    (this &| node) match {
      case TypeValueBranch(childs, tupleTypes, leafCount) if childs.size > 1 =>
        val childs2 = childs.filter { 
          case TypeValueLeaf(BuiltinTypeIdentity(TypeBuiltinFunction.Nothing, Seq()), _, _) => false
          case _                                                                            => true
        }
        val leafCount2 = childs2.foldLeft(0) { _ + _.leafCount }
        TypeValueBranch(childs2, tupleTypes, leafCount2)
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
  
  def typeValueBranchOrTypeValueLeaf(canExpandGlobalType: Boolean): TypeValueNode[T] =
    this match {
      case TypeValueBranch(Vector(globalTypeApp: GlobalTypeAppNode[T]), _, _) =>
        val child = globalTypeApp.typeValueBranchOrTypeValueLeaf(canExpandGlobalType)
        TypeValueBranch(Vector(child), Vector(), child.leafCount)
      case GlobalTypeAppNode(loc, childs, tupleTypes, leafCount, sym) =>
        if(canExpandGlobalType)
          TypeValueBranch(Vector(TypeValueLeaf(ExpandedGlobalTypeAppIdentity(loc, sym), 0, 1), TypeValueBranch(childs, tupleTypes, leafCount)), Nil, leafCount - 1)
        else
          TypeValueLeaf(UnexpandedGlobalTypeAppIdentity(loc, sym), 0, leafCount)
      case _ =>
        this
    }
  
  private def normalizedTypeValueChildForChecking(canExpandGlobalType: Boolean): TypeValueNode[T] =
    this match {
      case TypeValueBranch(childs, tupleTypes, leafCount) =>
        TypeValueBranch[T](childs :+ TypeValueLeaf(BuiltinTypeIdentity(TypeBuiltinFunction.Nothing, Nil), 0, 1), tupleTypes, leafCount + 1)
      case leaf: TypeValueLeaf[T] =>
        TypeValueBranch[T](Vector(leaf, TypeValueLeaf(BuiltinTypeIdentity(TypeBuiltinFunction.Nothing, Nil), 0, 1)), Vector(), leafCount + 1)
      case globalTypeAppNode: GlobalTypeAppNode[T] =>
        globalTypeAppNode.typeValueBranchOrTypeValueLeaf(canExpandGlobalType)normalizedTypeValueChildForChecking(canExpandGlobalType)
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
        TypeValueBranch(childs3, tupleTypes, leafCount + childs3.size + 2)
      case leaf: TypeValueLeaf[T] =>
        TypeValueBranch(Vector(leaf), Vector(), leaf.leafCount).normalizedTypeValueNodeForChecking(canExpandGlobalType)
      case globalTypeAppNode: GlobalTypeAppNode[T] =>
        globalTypeAppNode.typeValueBranchOrTypeValueLeaf(canExpandGlobalType).normalizedTypeValueNodeForChecking(canExpandGlobalType)
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
case class TypeValueBranch[T](childs: Seq[TypeValueNode[T]], tupleTypes: Seq[TupleType[T]], leafCount: Int) extends TypeValueNode[T]
case class TypeValueLeaf[T](ident: TypeValueIdentity[T], paramAppIdx: Int, leafCount: Int) extends TypeValueNode[T]
{
  def typeValueTerm(args: Seq[TypeValueLambda[T]]) = 
    ident match {
      case FieldTypeIdentity(i)                      =>
        args match {
          case Seq(TypeValueLambda(Seq(), term)) => some(FieldType(i, term))
          case _                                 => none
        }
      case BuiltinTypeIdentity(bf, _)                =>
        TypeValueTerm.typeValueTermsFromTypeValueLambdas(args).map { BuiltinType(bf, _) }
      case UnittypeIdentity(loc, sym)                =>
        some(Unittype(loc, args, sym))
      case GrouptypeIdentity(loc, sym)               =>
        some(Grouptype(loc, args, sym))
      case ExpandedGlobalTypeAppIdentity(loc, sym)   =>
        some(GlobalTypeApp(loc, args, sym))
      case UnexpandedGlobalTypeAppIdentity(loc, sym) =>
        some(GlobalTypeApp(loc, args, sym))
      case TypeParamAppIdentity(param)               =>
        some(TypeParamApp(param, args, paramAppIdx))
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
