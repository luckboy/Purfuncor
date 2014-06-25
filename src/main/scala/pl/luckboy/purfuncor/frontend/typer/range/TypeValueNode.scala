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

sealed trait TypeValueNode[T]
{
  def leafCount: Int
  
  def withChildAndTupleTypes(child: TypeValueNode[T], tupleTypes: Seq[TupleType[T]]) =
    this.conjOrDisjWithTupleTypes(TypeValueBranch(Vector(child), tupleTypes, child.leafCount), tupleTypes)
  
  def conjOrDisjWithTupleTypes(node: TypeValueNode[T], tupleTypes: Seq[TupleType[T]]) =
    (this, node) match {
      case (TypeValueBranch(childs1, _, leafCount1), TypeValueBranch(childs2, _, leafCount2)) => 
        TypeValueBranch(childs1 ++ childs2, tupleTypes, leafCount1 + leafCount2)
      case (TypeValueBranch(childs1, _, leafCount1), TypeValueLeaf(_))                        =>
        TypeValueBranch(childs1 :+ node, tupleTypes, leafCount1 + 1)
      case (TypeValueLeaf(_), TypeValueBranch(childs2, _, leafCount2))                        =>
        TypeValueBranch(this +: childs2, tupleTypes, leafCount + 1)
      case (TypeValueLeaf(_), TypeValueLeaf(_))                                               =>
        TypeValueBranch(Vector(this, node), tupleTypes, 2)
    }
  
  def normalizedTypeValueNode: TypeValueNode[T] =
    this match {
      case TypeValueBranch(Vector(child), tupleTypes, _) =>
        child match {
          case TypeValueBranch(Vector(child2), _, _) => child2.normalizedTypeValueNode
          case TypeValueBranch(_, _, childLeafCount) => TypeValueBranch(Vector(child), tupleTypes, childLeafCount)
          case TypeValueLeaf(_)                      => child
        }
      case _                                            =>
        this
    }
  
  def isTypeValueLeaf = isInstanceOf[TypeValueLeaf[T]]
}
case class TypeValueBranch[T](childs: Seq[TypeValueNode[T]], tupleTypes: Seq[TupleType[T]], leafCount: Int) extends TypeValueNode[T]
case class TypeValueLeaf[T](ident: TypeValueIdentity[T]) extends TypeValueNode[T]
{
  override def leafCount = 1
}
case class GlobalTypeAppNode[T](loc: T, childs: Seq[TypeValueNode[T]], tupleTypes: Seq[TupleType[T]], leafCount: Int) extends TypeValueNode[T]
