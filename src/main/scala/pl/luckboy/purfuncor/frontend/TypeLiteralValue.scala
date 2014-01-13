/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend
import scalaz._
import scalaz.Scalaz._

sealed trait TypeLiteralValue
{
  override def toString =
    this match {
      case TupleTypeFunValue(n)    => "tuple " + n
      case FieldTypeFunValue(i)    => "## " + (i + 1)
      case FieldsetTypeFunValue(n) => "fieldset " + n
      case TypeBuiltinFunValue(bf) =>
        if(bf.toString.headOption.map { c => c.isLetter || c === '_' }.getOrElse(false)) "#" + bf else "##" + bf
    }
}
case class TupleTypeFunValue(n: Int) extends TypeLiteralValue
case class FieldTypeFunValue(i: Int) extends TypeLiteralValue
case class FieldsetTypeFunValue(n: Int) extends TypeLiteralValue
case class TypeBuiltinFunValue(bf: TypeBuiltinFunction.Value) extends TypeLiteralValue
