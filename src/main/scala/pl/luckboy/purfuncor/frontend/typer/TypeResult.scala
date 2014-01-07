/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.kinder.Kind
import pl.luckboy.purfuncor.frontend.kinder.NoKind

object TypeResult
{
  def typeResultFromKind[T](kind: Kind) =
    kind match {
      case noKind: NoKind => NoType.fromNoKind[T](noKind).failure
      case _              => kind.success
    }
  
  def typeResultFromKindResult[T, U](res: Validation[NoKind, T]) = res.swap.map(NoType.fromNoKind[U]).swap

  def typeResultFromTypeValue[T, U, V, W](value: TypeValue[T, U, V, W]) =
    value match {
      case noValue: NoTypeValue[T, U, V, W] => NoType.fromNoTypeValue(noValue).failure
      case _                                => value.success
    }
  
  def typeResultFromTypeValueResult[T, U, V, W, X](res: Validation[NoTypeValue[T, U, V, W], X]) = res.swap.map(NoType.fromNoTypeValue[T, U, V, W]).swap
  
  def resultFromTypeResult[T, U](res: Validation[NoType[T], U]) = res.swap.map { _.errs.toNel.getOrElse(NonEmptyList(FatalError("no error", none, NoPosition))) }.swap
}
