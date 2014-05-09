/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.instant

sealed trait GroupIdentity[+T]
{
  def isTypeParamAppGroupIdentity =
    this match {
      case TypeParamAppGroupIdentity => true
      case _                         => false
    }
}
case object DefaultGroupIdentity extends GroupIdentity[Nothing]
case class BuiltinTypeGroupIdentity[+T](bf: GroupTypeBuiltinFunction.Value, argIdents: Seq[GroupIdentity[T]]) extends GroupIdentity[T]
case class GrouptypeGroupIdentity[+T](loc: T, argIdents: Seq[GroupIdentity[T]]) extends GroupIdentity[T]
case object TypeParamAppGroupIdentity extends GroupIdentity[Nothing]

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
