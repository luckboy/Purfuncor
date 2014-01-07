/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.backend.interp
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend.SimpleTerm
import pl.luckboy.purfuncor.frontend.BuiltinFunction
import pl.luckboy.purfuncor.common.Evaluator._

object BuiltinFunctions
{
  val illegalAppNoValue = NoValue.fromString("illegal application")
  
  val builtinFunctions = Map[BuiltinFunction.Value, Function](
      // Boolean
      BuiltinFunction.ZNot -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) = argValues match {
          case Seq(BooleanValue(x)) => (env, BooleanValue(!x))
          case _                    => (env, illegalAppNoValue)
        }
      },
      BuiltinFunction.ZAnd -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(BooleanValue(x1), BooleanValue(x2)) => (env, BooleanValue(x1 & x2))
            case _                                       => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.ZOr -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(BooleanValue(x1), BooleanValue(x2)) => (env, BooleanValue(x1 | x2))
            case _                                       => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.ZXor -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(BooleanValue(x1), BooleanValue(x2)) => (env, BooleanValue(x1 ^ x2))
            case _                                       => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.ZEq -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(BooleanValue(x1), BooleanValue(x2)) => (env, BooleanValue(x1 === x2))
            case _                                       => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.ZNe -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(BooleanValue(x1), BooleanValue(x2)) => (env, BooleanValue(x1 =/= x2))
            case _                                       => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.Cond -> new Function(3) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(funValue, _, BooleanValue(true))  => appS(funValue, Vector(TupleValue(Vector())))(env)
            case Seq(_, funValue, BooleanValue(false)) => appS(funValue, Vector(TupleValue(Vector())))(env)
            case _                                     => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.CEq -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(CharValue(x1), CharValue(x2)) => (env, BooleanValue(x1 === x2))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      // Char
      BuiltinFunction.CNe -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(CharValue(x1), CharValue(x2)) => (env, BooleanValue(x1 =/= x2))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.CharFromInt -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(IntValue(x)) => (env, CharValue(x.toChar))
            case _                => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.BNeg -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ByteValue(x)) => (env, ByteValue((-x).toByte))
            case _                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.BNot -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ByteValue(x)) => (env, ByteValue((~x).toByte))
            case _                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.BAdd -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ByteValue(x1), ByteValue(x2)) => (env, ByteValue((x1 + x2).toByte))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.BSub -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ByteValue(x1), ByteValue(x2)) => (env, ByteValue((x1 - x2).toByte))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.BMul -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ByteValue(x1), ByteValue(x2)) => (env, ByteValue((x1 * x2).toByte))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.BDiv -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ByteValue(_), ByteValue(0))   => (env, NoValue.fromString("divided by zero"))
            case Seq(ByteValue(x1), ByteValue(x2)) => (env, ByteValue((x1 / x2).toByte))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.BMod -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ByteValue(_), ByteValue(0))   => (env, NoValue.fromString("divided by zero"))
            case Seq(ByteValue(x1), ByteValue(x2)) => (env, ByteValue((x1 % x2).toByte))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.BLsh -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ByteValue(x1), IntValue(x2)) => (env, ByteValue((x1 << x2).toByte))
            case _                                => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.BRsh -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ByteValue(x1), IntValue(x2)) => (env, ByteValue((x1 >> x2).toByte))
            case _                                => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.BAnd -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ByteValue(x1), ByteValue(x2)) => (env, ByteValue((x1 & x2).toByte))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.BOr -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ByteValue(x1), ByteValue(x2)) => (env, ByteValue((x1 | x2).toByte))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.BXor -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ByteValue(x1), ByteValue(x2)) => (env, ByteValue((x1 ^ x2).toByte))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.BEq -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ByteValue(x1), ByteValue(x2)) => (env, BooleanValue(x1 === x2))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.BNe -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ByteValue(x1), ByteValue(x2)) => (env, BooleanValue(x1 =/= x2))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.BLt -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ByteValue(x1), ByteValue(x2)) => (env, BooleanValue(x1 < x2))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.BLe -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ByteValue(x1), ByteValue(x2)) => (env, BooleanValue(x1 <= x2))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.BGt -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ByteValue(x1), ByteValue(x2)) => (env, BooleanValue(x1 > x2))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.BGe -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ByteValue(x1), ByteValue(x2)) => (env, BooleanValue(x1 >= x2))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.ByteFromShort -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ShortValue(x)) => (env, ByteValue(x.toByte))
            case _                  => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.ByteFromInt -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(IntValue(x)) => (env, ByteValue(x.toByte))
            case _                => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.ByteFromLong -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(LongValue(x)) => (env, ByteValue(x.toByte))
            case _                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.ByteFromFloat -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(FloatValue(x)) => (env, ByteValue(x.toByte))
            case _                  => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.ByteFromDouble -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(DoubleValue(x)) => (env, ByteValue(x.toByte))
            case _                   => (env, illegalAppNoValue)
          }
      },
      // Short
      BuiltinFunction.SNeg -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ShortValue(x)) => (env, ShortValue((-x).toShort))
            case _                  => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.SNot -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ShortValue(x)) => (env, ShortValue((~x).toShort))
            case _                  => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.SAdd -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ShortValue(x1), ShortValue(x2)) => (env, ShortValue((x1 + x2).toShort))
            case _                                   => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.SSub -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ShortValue(x1), ShortValue(x2)) => (env, ShortValue((x1 - x2).toShort))
            case _                                   => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.SMul -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ShortValue(x1), ShortValue(x2)) => (env, ShortValue((x1 * x2).toShort))
            case _                                   => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.SDiv -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ShortValue(_), ShortValue(0))   => (env, NoValue.fromString("divided by zero"))
            case Seq(ShortValue(x1), ShortValue(x2)) => (env, ShortValue((x1 / x2).toShort))
            case _                                   => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.SMod -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ShortValue(_), ShortValue(0))   => (env, NoValue.fromString("divided by zero"))
            case Seq(ShortValue(x1), ShortValue(x2)) => (env, ShortValue((x1 % x2).toShort))
            case _                                   => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.SLsh -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ShortValue(x1), IntValue(x2)) => (env, ShortValue((x1 << x2).toShort))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.SRsh -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ShortValue(x1), IntValue(x2)) => (env, ShortValue((x1 >> x2).toShort))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.SAnd -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ShortValue(x1), ShortValue(x2)) => (env, ShortValue((x1 & x2).toShort))
            case _                                   => (env, illegalAppNoValue)
          }      
      },
      BuiltinFunction.SOr -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ShortValue(x1), ShortValue(x2)) => (env, ShortValue((x1 | x2).toShort))
            case _                                   => (env, illegalAppNoValue)
          }      
      },
      BuiltinFunction.SXor -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ShortValue(x1), ShortValue(x2)) => (env, ShortValue((x1 ^ x2).toShort))
            case _                                   => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.SEq -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ShortValue(x1), ShortValue(x2)) => (env, BooleanValue(x1 === x2))
            case _                                   => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.SNe -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ShortValue(x1), ShortValue(x2)) => (env, BooleanValue(x1 =/= x2))
            case _                                   => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.SLt -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ShortValue(x1), ShortValue(x2)) => (env, BooleanValue(x1 < x2))
            case _                                   => (env, illegalAppNoValue)
          }      
      },
      BuiltinFunction.SLe -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ShortValue(x1), ShortValue(x2)) => (env, BooleanValue(x1 <= x2))
            case _                                   => (env, illegalAppNoValue)
          }      
      },
      BuiltinFunction.SGt -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ShortValue(x1), ShortValue(x2)) => (env, BooleanValue(x1 > x2))
            case _                                   => (env, illegalAppNoValue)
          }      
      },
      BuiltinFunction.SGe -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ShortValue(x1), ShortValue(x2)) => (env, BooleanValue(x1 >= x2))
            case _                                   => (env, illegalAppNoValue)
          }      
      },
      BuiltinFunction.ShortFromByte -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ByteValue(x)) => (env, ShortValue(x))
            case _                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.ShortFromInt -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(IntValue(x)) => (env, ShortValue(x.toShort))
            case _                => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.ShortFromLong -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(LongValue(x)) => (env, ShortValue(x.toShort))
            case _                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.ShortFromFloat -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(FloatValue(x)) => (env, ShortValue(x.toShort))
            case _                  => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.ShortFromDouble -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(DoubleValue(x)) => (env, ShortValue(x.toShort))
            case _                   => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.INeg -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(IntValue(x)) => (env, IntValue(-x))
            case _                => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.INot -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(IntValue(x)) => (env, IntValue(~x))
            case _                => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.IAdd -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(IntValue(x1), IntValue(x2)) => (env, IntValue(x1 + x2))
            case _                               => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.ISub -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(IntValue(x1), IntValue(x2)) => (env, IntValue(x1 - x2))
            case _                               => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.IMul -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(IntValue(x1), IntValue(x2)) => (env, IntValue(x1 * x2))
            case _                               => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.IDiv -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(IntValue(_), IntValue(0))   => (env, NoValue.fromString("divided by zero"))
            case Seq(IntValue(x1), IntValue(x2)) => (env, IntValue(x1 / x2))
            case _                               => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.IMod -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(IntValue(_), IntValue(0))   => (env, NoValue.fromString("divided by zero"))
            case Seq(IntValue(x1), IntValue(x2)) => (env, IntValue(x1 % x2))
            case _                               => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.ILsh -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(IntValue(x1), IntValue(x2)) => (env, IntValue(x1 << x2))
            case _                               => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.IRsh -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(IntValue(x1), IntValue(x2)) => (env, IntValue(x1 >> x2))
            case _                               => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.IAnd -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(IntValue(x1), IntValue(x2)) => (env, IntValue(x1 & x2))
            case _                               => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.IOr -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(IntValue(x1), IntValue(x2)) => (env, IntValue(x1 | x2))
            case _                               => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.IXor -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(IntValue(x1), IntValue(x2)) => (env, IntValue(x1 ^ x2))
            case _                               => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.IEq -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(IntValue(x1), IntValue(x2)) => (env, BooleanValue(x1 === x2))
            case _                               => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.INe -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(IntValue(x1), IntValue(x2)) => (env, BooleanValue(x1 =/= x2))
            case _                               => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.ILt -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(IntValue(x1), IntValue(x2)) => (env, BooleanValue(x1 < x2))
            case _                               => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.ILe -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(IntValue(x1), IntValue(x2)) => (env, BooleanValue(x1 <= x2))
            case _                               => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.IGt -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(IntValue(x1), IntValue(x2)) => (env, BooleanValue(x1 > x2))
            case _                               => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.IGe -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(IntValue(x1), IntValue(x2)) => (env, BooleanValue(x1 >= x2))
            case _                               => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.IntFromChar -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(CharValue(x)) => (env, IntValue(x.toInt))
            case _                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.IntFromByte -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ByteValue(x)) => (env, IntValue(x))
            case _                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.IntFromShort -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ShortValue(x)) => (env, IntValue(x))
            case _                  => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.IntFromLong -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(LongValue(x)) => (env, IntValue(x.toInt))
            case _                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.IntFromFloat -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(FloatValue(x)) => (env, IntValue(x.toInt))
            case _                  => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.IntFromDouble -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(DoubleValue(x)) => (env, IntValue(x.toInt))
            case _                   => (env, illegalAppNoValue)
          }
      },
      // Long
      BuiltinFunction.LNeg -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(LongValue(x)) => (env, LongValue(-x))
            case _                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.LNot -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(LongValue(x)) => (env, LongValue(~x))
            case _                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.LAdd -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(LongValue(x1), LongValue(x2)) => (env, LongValue(x1 + x2))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.LSub -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(LongValue(x1), LongValue(x2)) => (env, LongValue(x1 - x2))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.LMul -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(LongValue(x1), LongValue(x2)) => (env, LongValue(x1 * x2))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.LDiv -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(LongValue(_), LongValue(0))   => (env, NoValue.fromString("divided by zero"))
            case Seq(LongValue(x1), LongValue(x2)) => (env, LongValue(x1 / x2))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.LMod-> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(LongValue(_), LongValue(0))   => (env, NoValue.fromString("divided by zero"))
            case Seq(LongValue(x1), LongValue(x2)) => (env, LongValue(x1 % x2))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.LLsh -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(LongValue(x1), IntValue(x2)) => (env, LongValue(x1 << x2))
            case _                                => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.LRsh -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(LongValue(x1), IntValue(x2)) => (env, LongValue(x1 >> x2))
            case _                                => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.LAnd -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(LongValue(x1), LongValue(x2)) => (env, LongValue(x1 & x2))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.LOr -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(LongValue(x1), LongValue(x2)) => (env, LongValue(x1 | x2))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.LXor -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(LongValue(x1), LongValue(x2)) => (env, LongValue(x1 ^ x2))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.LEq -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(LongValue(x1), LongValue(x2)) => (env, BooleanValue(x1 === x2))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.LNe -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(LongValue(x1), LongValue(x2)) => (env, BooleanValue(x1 =/= x2))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.LLt -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(LongValue(x1), LongValue(x2)) => (env, BooleanValue(x1 < x2))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.LLe -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(LongValue(x1), LongValue(x2)) => (env, BooleanValue(x1 <= x2))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.LGt -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(LongValue(x1), LongValue(x2)) => (env, BooleanValue(x1 > x2))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.LGe -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(LongValue(x1), LongValue(x2)) => (env, BooleanValue(x1 >= x2))
            case _                                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.LongFromByte -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ByteValue(x)) => (env, LongValue(x))
            case _                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.LongFromShort -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ShortValue(x)) => (env, LongValue(x))
            case _                  => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.LongFromInt -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(IntValue(x)) => (env, LongValue(x))
            case _                => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.LongFromFloat -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(FloatValue(x)) => (env, LongValue(x.toLong))
            case _                  => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.LongFromDouble -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(DoubleValue(x)) => (env, LongValue(x.toLong))
            case _                   => (env, illegalAppNoValue)
          }
      },
      // Float
      BuiltinFunction.FNeg -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(FloatValue(x)) => (env, FloatValue(-x))
            case _                  => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.FAdd -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(FloatValue(x1), FloatValue(x2)) => (env, FloatValue(x1 + x2))
            case _                                   => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.FSub -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(FloatValue(x1), FloatValue(x2)) => (env, FloatValue(x1 - x2))
            case _                                   => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.FMul -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(FloatValue(x1), FloatValue(x2)) => (env, FloatValue(x1 * x2))
            case _                                   => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.FDiv -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(FloatValue(x1), FloatValue(x2)) => (env, FloatValue(x1 / x2))
            case _                                   => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.FEq -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(FloatValue(x1), FloatValue(x2)) => (env, BooleanValue(x1 === x2))
            case _                                   => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.FNe -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(FloatValue(x1), FloatValue(x2)) => (env, BooleanValue(x1 =/= x2))
            case _                                   => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.FLt -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(FloatValue(x1), FloatValue(x2)) => (env, BooleanValue(x1 < x2))
            case _                                   => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.FLe -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(FloatValue(x1), FloatValue(x2)) => (env, BooleanValue(x1 <= x2))
            case _                                   => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.FGt -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(FloatValue(x1), FloatValue(x2)) => (env, BooleanValue(x1 > x2))
            case _                                   => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.FGe -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(FloatValue(x1), FloatValue(x2)) => (env, BooleanValue(x1 >= x2))
            case _                                   => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.FloatFromByte -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ByteValue(x)) => (env, FloatValue(x))
            case _                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.FloatFromShort -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ShortValue(x)) => (env, FloatValue(x))
            case _                  => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.FloatFromInt -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(IntValue(x)) => (env, FloatValue(x))
            case _                => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.FloatFromLong -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(LongValue(x)) => (env, FloatValue(x))
            case _                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.FloatFromDouble -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(DoubleValue(x)) => (env, FloatValue(x.toFloat))
            case _                   => (env, illegalAppNoValue)
          }
      },
      // Double
      BuiltinFunction.DNeg -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(DoubleValue(x)) => (env, DoubleValue(-x))
            case _                   => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.DAdd -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(DoubleValue(x1), DoubleValue(x2)) => (env, DoubleValue(x1 + x2))
            case _                                     => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.DSub -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(DoubleValue(x1), DoubleValue(x2)) => (env, DoubleValue(x1 - x2))
            case _                                     => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.DMul -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(DoubleValue(x1), DoubleValue(x2)) => (env, DoubleValue(x1 * x2))
            case _                                     => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.DDiv -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(DoubleValue(x1), DoubleValue(x2)) => (env, DoubleValue(x1 / x2))
            case _                                     => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.DEq -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(DoubleValue(x1), DoubleValue(x2)) => (env, BooleanValue(x1 === x2))
            case _                                     => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.DNe -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(DoubleValue(x1), DoubleValue(x2)) => (env, BooleanValue(x1 =/= x2))
            case _                                     => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.DLt -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(DoubleValue(x1), DoubleValue(x2)) => (env, BooleanValue(x1 < x2))
            case _                                     => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.DLe -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(DoubleValue(x1), DoubleValue(x2)) => (env, BooleanValue(x1 <= x2))
            case _                                     => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.DGt -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(DoubleValue(x1), DoubleValue(x2)) => (env, BooleanValue(x1 > x2))
            case _                                     => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.DGe -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(DoubleValue(x1), DoubleValue(x2)) => (env, BooleanValue(x1 >= x2))
            case _                                     => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.DoubleFromByte -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ByteValue(x)) => (env, DoubleValue(x))
            case _                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.DoubleFromShort -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ShortValue(x)) => (env, DoubleValue(x))
            case _                  => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.DoubleFromInt -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(IntValue(x)) => (env, DoubleValue(x))
            case _                => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.DoubleFromLong -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(LongValue(x)) => (env, DoubleValue(x))
            case _                 => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.DoubleFromFloat -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(FloatValue(x)) => (env, DoubleValue(x))
            case _                  => (env, illegalAppNoValue)
          }
      },
      // Array
      BuiltinFunction.EmptyArray -> new Function(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, ArrayValue(Vector()))
            case _     => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.Array -> new Function(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(LongValue(x), value) => 
              if(x < 0)
                (env, ArrayValue(Vector.fill(x.toInt)(value)))
              else if(x < Integer.MAX_VALUE)
                (env, ArrayValue(Vector.fill(x.toInt)(value)))
              else
                (env, NoValue.fromString("out of memory"))
            case _                        =>
              (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.Length -> new Function(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(ArrayValue(values)) => (env, LongValue(values.size))
            case _                       => (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.MapNthOrElse -> new Function(4) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(funValue, defaultValue, LongValue(x), ArrayValue(values)) =>
              if(x >= 0 && x < values.size)
                appS(funValue, Vector(values(x.toInt)))(env)
              else
                (env, defaultValue)
            case _                                                             =>
              (env, illegalAppNoValue)
          }
      },
      BuiltinFunction.Updated -> new Function(3) {
        override def applyS[T, U, V, W, E](argValues: Seq[Value[T, U, V, W]])(env: E)(implicit eval: Evaluator[SimpleTerm[T, U, V], E, Value[T, U, V, W]]) =
          argValues match {
            case Seq(LongValue(x), value, arrValue @ ArrayValue(values)) =>
              if(x >= 0 && x < values.size)
                (env, ArrayValue(values.updated(x.toInt, value)))
              else
                (env, arrValue)
            case _                                                       =>
              (env, illegalAppNoValue)
          }
      })
}
