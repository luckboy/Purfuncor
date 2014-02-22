/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.instant
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.kinder.InferredKind
import pl.luckboy.purfuncor.frontend.typer.InferredType
import pl.luckboy.purfuncor.frontend.typer.TypeValueTerms._

object BuiltinInstanceTree
{
  def builtinInstanceTree[T, U] = InstanceTree.fromInstanceTables[AbstractPolyFunction[T], U, GlobalInstance[T]](Map(
      // construct
      ConstructFunction -> InstanceTable.fromTuples(Vector(
          // Byte
          GlobalInstanceType(InferredType(zeroType & byteType, Seq())) -> ZeroIntegerConstructInstance(IntegerTypeFunction.Byte),
          GlobalInstanceType(InferredType(nonZeroType & byteType, Seq())) -> NonZeroIntegerConstructInstance(IntegerTypeFunction.Byte),
          // Short
          GlobalInstanceType(InferredType(zeroType & shortType, Seq())) -> ZeroIntegerConstructInstance(IntegerTypeFunction.Short),
          GlobalInstanceType(InferredType(nonZeroType & shortType, Seq())) -> NonZeroIntegerConstructInstance(IntegerTypeFunction.Short),
          // Int
          GlobalInstanceType(InferredType(zeroType & intType, Seq())) -> ZeroIntegerConstructInstance(IntegerTypeFunction.Int),
          GlobalInstanceType(InferredType(nonZeroType & intType, Seq())) -> NonZeroIntegerConstructInstance(IntegerTypeFunction.Int),
          // Long
          GlobalInstanceType(InferredType(zeroType & longType, Seq())) -> ZeroIntegerConstructInstance(IntegerTypeFunction.Long),
          GlobalInstanceType(InferredType(nonZeroType & longType, Seq())) -> NonZeroIntegerConstructInstance(IntegerTypeFunction.Long),
          // Array
          GlobalInstanceType(InferredType(emptyType & arrayType(typeParam(0, 0)), Seq(InferredKind(Star(KindType, NoPosition))))) -> EmptyArrayConstructInstance,
          GlobalInstanceType(InferredType(nonEmptyType & arrayType(typeParam(0, 0)), Seq(InferredKind(Star(KindType, NoPosition))))) -> NonEmptyArrayConstructInstance)),
      // select
      SelectFunction -> InstanceTable.fromTuples(Vector(
          // Byte
          GlobalInstanceType(InferredType((zeroType | nonZeroType) & byteType, Seq())) -> IntegerSelectInstance(IntegerTypeFunction.Byte),
          // Short
          GlobalInstanceType(InferredType((zeroType | nonZeroType) & shortType, Seq())) -> IntegerSelectInstance(IntegerTypeFunction.Short),
          // Int
          GlobalInstanceType(InferredType((zeroType | nonZeroType) & intType, Seq())) -> IntegerSelectInstance(IntegerTypeFunction.Int),
          // Long
          GlobalInstanceType(InferredType((zeroType | nonZeroType) & longType, Seq())) -> IntegerSelectInstance(IntegerTypeFunction.Long),
          // Array
          GlobalInstanceType(InferredType((emptyType | nonEmptyType) & arrayType(typeParam(0, 0)), Seq(InferredKind(Star(KindType, NoPosition))))) -> ArraySelectInstance))))
}
