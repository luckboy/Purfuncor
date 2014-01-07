/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.kinder
import scalaz._
import scalaz.Scalaz._

trait KindInferenceEnvironmentState[E, L]
{
  def instantiateLocalKindTablesS(env: E): (E, Validation[NoKind, Unit])
  
  def instantiateKindS(kind: Kind)(env: E): (E, Kind)
  
  def withTypeCombinatorLocationS[T](loc: Option[L])(f: E => (E, T))(env: E): (E, T)
  
  def withClearS[T](f: E => (E, T))(env: E): (E, T)
}
