/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.kinder
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.common.Arrow

object TypeBuiltinFunKindTerms
{
  val typeBuiltinFunKindTerms = Map[TypeBuiltinFunction.Value, KindTerm[StarKindTerm[Int]]](
      TypeBuiltinFunction.Any -> Star(KindType, NoPosition),
      TypeBuiltinFunction.Nothing -> Star(KindType, NoPosition),
      TypeBuiltinFunction.Zero -> Star(KindType, NoPosition),
      TypeBuiltinFunction.NonZero -> Star(KindType, NoPosition),
      TypeBuiltinFunction.Boolean -> Star(KindType, NoPosition),
      TypeBuiltinFunction.Char -> Star(KindType, NoPosition),
      TypeBuiltinFunction.Byte -> Star(KindType, NoPosition),
      TypeBuiltinFunction.Short -> Star(KindType, NoPosition),
      TypeBuiltinFunction.Int -> Star(KindType, NoPosition),
      TypeBuiltinFunction.Long -> Star(KindType, NoPosition),
      TypeBuiltinFunction.Float -> Star(KindType, NoPosition),
      TypeBuiltinFunction.Double -> Star(KindType, NoPosition),
      TypeBuiltinFunction.Empty -> Star(KindType, NoPosition),
      TypeBuiltinFunction.NonEmpty -> Star(KindType, NoPosition),
      TypeBuiltinFunction.Array -> Arrow(Star(KindType, NoPosition), Star(KindType, NoPosition), NoPosition),
      TypeBuiltinFunction.Fun -> Arrow(Star(KindType, NoPosition), Arrow(Star(KindType, NoPosition), Star(KindType, NoPosition), NoPosition), NoPosition),
      TypeBuiltinFunction.Conj -> Arrow(Star(KindType, NoPosition), Arrow(Star(KindType, NoPosition), Star(KindType, NoPosition), NoPosition), NoPosition),
      TypeBuiltinFunction.Disj -> Arrow(Star(KindType, NoPosition), Arrow(Star(KindType, NoPosition), Star(KindType, NoPosition), NoPosition), NoPosition))
}
