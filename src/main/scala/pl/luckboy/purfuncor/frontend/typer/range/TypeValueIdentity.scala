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

sealed trait TypeValueIdentity[+T]
case class FieldTypeIdentity[+T](i: Int, ident: TypeValueIdentity[T]) extends TypeValueIdentity[T]
case class BuiltinTypeIdentity[+T](bf: TypeBuiltinFunction.Value, argIdents: Seq[TypeValueIdentity[T]]) extends TypeValueIdentity[T]
case class UnittypeIdentity[+T](loc: T, sym: GlobalSymbol) extends TypeValueIdentity[T]
case class GrouptypeIdentity[+T](loc: T, sym: GlobalSymbol) extends TypeValueIdentity[T]
case class ExpandedGlobalTypeAppIdentity[+T](loc: T, sym: GlobalSymbol) extends TypeValueIdentity[T]
case class UnexpandedGlobalTypeAppIdentity[+T](loc: T, sym: GlobalSymbol) extends TypeValueIdentity[T]
case class TypeParamAppIdentity[+T](param: Int) extends TypeValueIdentity[T]
