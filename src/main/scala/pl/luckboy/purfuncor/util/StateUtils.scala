/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.util
import scalaz._
import scalaz.Scalaz._

object StateUtils
{
  def steS[E, S, T](f: S => (S, Validation[E, T])) = StateT[EitherP[(S, E)]#A, S, T]({
    s =>
      val (s2, res) = f(s)
      res.map { Right(s2, _) }.valueOr { Left(s2, _) }
  })
  
  implicit def ste[E, S, T](f: State[S, Validation[E, T]]) = steS[E, S, T](f.run)
  
  def ste1S[E, S, T](f: S => (S, T)) = StateT[EitherP[(S, E)]#A, S, T]({ x => Right(f(x)) })
  
  implicit def ste1[E, S, T](f: State[S, T]) = ste1S(f.run)
  
  implicit def st[E, S, T](f: StateT[EitherP[(S, E)]#A, S, T]) = State[S, Validation[E, T]]({
    s => f.run(s).fold({ case (s2, err) => (s2, err.failure) }, { case (s2, y) => (s2, y.success) })
  })
}
