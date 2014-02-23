/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.instant
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.typer.TypeIdentity

trait TypeEnvironmentState[E, L]
{
  def withPartialEvaluationS[T](isPartial: Boolean)(f: E => (E, T))(env: E): (E, T)
  
  def getTypeIdentityFromEnvironmentS(loc: L)(env: E): (E, Option[TypeIdentity[L]])
  
  def addTypeIdentityS(loc: L, ident: TypeIdentity[L])(env: E): (E, Unit)
}
