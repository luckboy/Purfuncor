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
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

object TypeValueTerms 
{
  def anyType[T] = BuiltinType[T](TypeBuiltinFunction.Any, Nil)
  def nothingType[T] = BuiltinType[T](TypeBuiltinFunction.Nothing, Nil)
  def zeroType[T] = BuiltinType[T](TypeBuiltinFunction.Zero, Nil)
  def nonZeroType[T] = BuiltinType[T](TypeBuiltinFunction.NonZero, Nil)
  def booleanType[T] = BuiltinType[T](TypeBuiltinFunction.Boolean, Nil)
  def charType[T] = BuiltinType[T](TypeBuiltinFunction.Char, Nil)
  def byteType[T] = BuiltinType[T](TypeBuiltinFunction.Byte, Nil)
  def shortType[T] = BuiltinType[T](TypeBuiltinFunction.Short, Nil)
  def intType[T] = BuiltinType[T](TypeBuiltinFunction.Int, Nil)
  def longType[T] = BuiltinType[T](TypeBuiltinFunction.Long, Nil)
  def floatType[T] = BuiltinType[T](TypeBuiltinFunction.Float, Nil)
  def doubleType[T] = BuiltinType[T](TypeBuiltinFunction.Double, Nil)
  def emptyType[T] = BuiltinType[T](TypeBuiltinFunction.Empty, Nil)
  def nonEmptyType[T] = BuiltinType[T](TypeBuiltinFunction.NonEmpty, Nil)
  def arrayType[T](term: TypeValueTerm[T]) = BuiltinType[T](TypeBuiltinFunction.Array, Vector(term))
  def funType[T](term1: TypeValueTerm[T], term2: TypeValueTerm[T]) = BuiltinType[T](TypeBuiltinFunction.Fun, Vector(term1, term2))
  
  def typeParam[T](param: Int, paramAppIdx: Int) = TypeParamApp[T](param, Nil, paramAppIdx)
}
