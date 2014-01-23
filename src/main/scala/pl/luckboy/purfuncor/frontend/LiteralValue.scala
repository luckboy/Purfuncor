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

sealed trait LiteralValue
{
  override def toString =
    this match {
      case BooleanValue(x)           => if(x) "true" else "false"
      case CharValue(x)              => "'" + (if(x === '\'') "\\'" else x) + "'"
      case ByteValue(x)              => x + "b"
      case ShortValue(x)             => x + "s"
      case IntValue(x)               => x.toString
      case LongValue(x)              => x + "L"
      case FloatValue(x)             => x + "f"
      case DoubleValue(x)            => x.toString
      case TupleFunValue(n)          => "tuple " + n
      case TupleFieldFunValue(n, i)  => "#" + n + " " + (i + 1)
      case MakearrayFunValue(n)      => "makearray " + n
      case MakelistFunValue(n)       => "makelist " + n
      case FieldFunValue(i)          => "##" + (i + 1)
      case FieldsetFunValue(n)       => "fieldset " + n
      case FieldSetAppFunValue(n)    => "###" + n
      case FieldswithFunValue(n, is) => "(fieldswith " + n + is.map { i => " " + (i + 1) }.mkString("") + ")"
      case BuiltinFunValue(f)        => "#" + f.toString
    }
}
case class BooleanValue(x: Boolean) extends LiteralValue
case class CharValue(x: Char) extends LiteralValue
case class ByteValue(x: Byte) extends LiteralValue
case class ShortValue(x: Short) extends LiteralValue
case class IntValue(x: Int) extends LiteralValue
case class LongValue(x: Long) extends LiteralValue
case class FloatValue(x: Float) extends LiteralValue
case class DoubleValue(x: Double) extends LiteralValue
case class TupleFunValue(n: Int) extends LiteralValue
case class TupleFieldFunValue(n: Int, i: Int) extends LiteralValue
case class MakearrayFunValue(n: Int) extends LiteralValue
case class MakelistFunValue(n: Int) extends LiteralValue
case class FieldFunValue(i: Int) extends LiteralValue
case class FieldsetFunValue(n: Int) extends LiteralValue
case class FieldSetAppFunValue(n: Int) extends LiteralValue
case class FieldswithFunValue(n: Int, is: List[Int]) extends LiteralValue
case class BuiltinFunValue(bf: BuiltinFunction.Value) extends LiteralValue
