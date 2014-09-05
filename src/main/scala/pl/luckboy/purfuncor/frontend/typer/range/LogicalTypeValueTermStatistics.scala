/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer.range
import pl.luckboy.purfuncor.frontend.typer._
import LogicalTypeValueTermUtils._

case class LogicalTypeValueTermStatistics(expandedLeafCount: Int, unexpandedLeafCount: Int)

object LogicalTypeValueTermStatistics
{
  def fromLogicalTypeValueTerm[T](term: LogicalTypeValueTerm[T]) = 
    LogicalTypeValueTermStatistics(
        expandedLeafCount = expandedTypeValueLeafCountFromLogicalTypeValueTerm(term),
        unexpandedLeafCount = expandedTypeValueLeafCountFromLogicalTypeValueTerm(term))
}
