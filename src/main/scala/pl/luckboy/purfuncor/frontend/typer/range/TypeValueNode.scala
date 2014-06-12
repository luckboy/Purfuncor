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
  
  def withChild(child: TypeValueNode[T]): TypeValueNode[T] =
    throw new UnsupportedOperationException
  
  def conjOrDisjWithTupleTypes(node: TypeValueNode[T], tupleTypes: Seq[TupleType[T]]): TypeValueNode[T] =
    throw new UnsupportedOperationException
  
  def normalizedTypeValueNode: TypeValueNode[T] =
    throw new UnsupportedOperationException
}
case class TypeValueBranch[T](childs: Seq[TypeValueNode[T]], tupleTypes: Seq[TupleType[T]], leafCount: Int) extends TypeValueNode[T]
case class TypeValueLeaf[T](ident: TypeValueIdentity[T]) extends TypeValueNode[T]
{
  def leafCount = 1
}
