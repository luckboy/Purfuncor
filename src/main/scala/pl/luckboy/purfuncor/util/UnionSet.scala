/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.util
import scalaz._
import scalaz.Scalaz._

sealed trait UnionSet[T]
{
  def | (set: UnionSet[T]) =
    (this, set) match {
      case (_, ListUnionSet(sets)) => ListUnionSet(this :: sets)
      case (ListUnionSet(sets), _) => ListUnionSet(set :: sets)
      case (_, _)                  => ListUnionSet(List(this, set))
    }
  
  def union (set: UnionSet[T]) = this | set
}

object UnionSet
{
  def empty[T] = ListUnionSet[T](Nil)
  
  def apply[T](xs: T*) = fromIterable(xs)
  
  def fromIterable[T](xs: Iterable[T]) =
    xs.headOption.map {
      x => if(xs.size === 1) SingleUnionSet(x) else ListUnionSet(xs.map { SingleUnionSet(_) }.toList)
    }.getOrElse(empty[T])
}

case class SingleUnionSet[T](x: T) extends UnionSet[T]
case class ListUnionSet[T](sets: List[UnionSet[T]]) extends UnionSet[T]
