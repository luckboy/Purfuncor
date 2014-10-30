/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer

trait TypeEnvironmentState[E, L, V]
{
  def typeParamCountFromEnvironmentS(env: E): (E, Int)
  
  def withTypeParamsS[T](paramCount: Int)(f: (Int, Int, E) => (E, T))(env: E): (E, T)
  
  def currentTypeParamAppIdxFromEnvironmentS(env: E): (E, Int)
  
  def globalTypeVarValueFromEnvironmentS(loc: L)(env: E): (E, V)
  
  def withPartialEvaluationS[T](isPartial: Boolean)(f: E => (E, T))(env: E): (E, T)
  
  // This method for leaks of type closures.
  def withClearS[T](f: E => (E, T))(env: E): (E, T)
  
  def hasRecursiveTypeCombinator(loc: L)(env: E): (E, Boolean)

  def hasApplyingTypeCombinator(loc: L)(env: E): (E, Boolean)
}
