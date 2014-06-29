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
        val expandedNode = typeValueBranchOrTypeValueLeaf(canExpandGlobalType)
        val expandedNode2 = typeValueBranchOrTypeValueLeaf(canExpandGlobalType)
        expandedNode.conjOrDisjWithTupleTypes(expandedNode2, tupleTypes, canExpandGlobalType)
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
  
  def typeValueBranchOrTypeValueLeaf[T](canExpandGlobalType: Boolean) =
    this match {
      case GlobalTypeAppNode(loc, sym, childs, tupleTypes, leafCount) =>
        if(canExpandGlobalType)
          TypeValueBranch(Seq(TypeValueLeaf(ExpandedGlobalTypeAppIdentity(loc, sym), 0, 1), TypeValueBranch(childs, tupleTypes, leafCount)), Nil, leafCount - 1)
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
}
case class TypeValueBranch[T](childs: Seq[TypeValueNode[T]], tupleTypes: Seq[TupleType[T]], leafCount: Int) extends TypeValueNode[T]
case class TypeValueLeaf[T](ident: TypeValueIdentity[T], paramAppIdx: Int, leafCount: Int) extends TypeValueNode[T]
{
  def typeValueTerm(args: Seq[TypeValueLambda[T]]) = 
    ident match {
      case FieldTypeIdentity(i, _)                   =>
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
case class GlobalTypeAppNode[T](loc: T, sym: GlobalSymbol, childs: Seq[TypeValueNode[T]], tupleTypes: Seq[TupleType[T]], leafCount: Int) extends TypeValueNode[T]
