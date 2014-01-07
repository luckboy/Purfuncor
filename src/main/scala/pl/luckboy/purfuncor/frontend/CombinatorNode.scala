/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend
import scalaz._
import scalaz.Scalaz._

case class CombinatorNode[+T, +U, +V, W](
    comb: AbstractCombinator[T, U, V],
    recursiveCombSyms: Set[W],
    markedRecCombSyms: Set[W])
