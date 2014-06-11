/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer.range
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend.typer._

case class LogicalTypeValueTermInfo[T](
    conjRangeSets: Map[TypeValueIdentity[T], TypeValueRangeSet[T]],
    disjRangeSets: Map[TypeValueIdentity[T], TypeValueRangeSet[T]],
    conjDepthRangeSets: List[TypeValueRangeSet[T]],
    disjDepthRangeSets: List[TypeValueRangeSet[T]])
