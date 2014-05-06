/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._

sealed trait TypeIdentity[+T]
case object TupleTypeIdentity extends TypeIdentity[Nothing]
case class FieldTypeIdentity[+T](i: Int) extends TypeIdentity[T]
case class BuiltinTypeIdentity[+T](bf: TypeBuiltinFunction.Value) extends TypeIdentity[T]
case class UnittypeIdentity[+T](loc: T) extends TypeIdentity[T]
case object NoTypeIdentity extends TypeIdentity[Nothing]
