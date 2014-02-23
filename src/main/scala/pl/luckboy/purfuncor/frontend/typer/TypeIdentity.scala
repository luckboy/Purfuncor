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

case class TypeIdentity[T](idents: Set[TypeValueTermIdentity[T]], paramApps: Seq[TypeParamApp[T]])

sealed trait TypeValueTermIdentity[+T]
case object TupleTypeIdentity extends TypeValueTermIdentity[Nothing]
case class FieldTypeIdentity[+T](i: Int) extends TypeValueTermIdentity[T]
case class BuiltinTypeIdentity[+T](bf: TypeBuiltinFunction.Value) extends TypeValueTermIdentity[T]
case class UnittypeIdentity[+T](loc: T) extends TypeValueTermIdentity[T]
case object TypeParamAppIdentity extends TypeValueTermIdentity[Nothing]
