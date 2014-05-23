/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.instant
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.util.CollectionUtils._

case class GroupIdentity[T](funIdent: GroupNodeIdentity[T], argIdents: Seq[GroupIdentity[T]])
{
  def unify(groupIdent: GroupIdentity[T]): Option[GroupIdentity[T]] =
    (this, groupIdent) match {
      case (GroupIdentity(funIdent1, argIdents1), GroupIdentity(funIdent2, argIdents2))  =>
        if(funIdent1 === funIdent2 && argIdents1.size === argIdents2.size)
          mapToVectorOption(argIdents1.zip(argIdents2)) {
            case (argIdent1, argIdent2) => argIdent1.unify(argIdent2)
          }.map { GroupIdentity(funIdent1, _) }
        else
          (funIdent1, funIdent2) match {
            case (BuiltinTypeGroupNodeIdentity(_) | GrouptypeGroupNodeIdentity(_) | TypeParamAppGroupNodeIdentity, DefaultGroupNodeIdentity) =>
              some(this)
            case (DefaultGroupNodeIdentity, BuiltinTypeGroupNodeIdentity(_) | GrouptypeGroupNodeIdentity(_) | TypeParamAppGroupNodeIdentity) =>
              groupIdent.unify(this)
            case _ =>
              none
          }
    }
  
  def groupIdentPairs: Vector[(GroupNodeIdentity[T], Int)] =
    this match {
      case GroupIdentity(funIdent, argIdents) =>
        val pairs = argIdents.flatMap { _.groupIdentPairs }.toVector
        ((funIdent, pairs.size)) +: pairs
    }
  
  def groupNodeIdents = groupIdentPairs.map { _._1 }
}

sealed trait GroupNodeIdentity[+T]
case object DefaultGroupNodeIdentity extends GroupNodeIdentity[Nothing]
case class BuiltinTypeGroupNodeIdentity[+T](bf: GroupTypeBuiltinFunction.Value) extends GroupNodeIdentity[T]
case class GrouptypeGroupNodeIdentity[+T](loc: T) extends GroupNodeIdentity[T]
case object TypeParamAppGroupNodeIdentity extends GroupNodeIdentity[Nothing]

object GroupTypeBuiltinFunction extends Enumeration
{
  val Boolean = Value("Boolean")
  val Char = Value("Char")
  val Byte = Value("Byte")
  val Short = Value("Short")
  val Int = Value("Int")
  val Long = Value("Long")
  val Float = Value("Float")
  val Double = Value("Double")
  val Array = Value("Array")
  val Fun = Value("->")
}
