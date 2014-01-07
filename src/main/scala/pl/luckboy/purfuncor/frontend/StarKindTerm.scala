/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend

sealed trait StarKindTerm[+T]
case class KindParam[+T](param: T) extends StarKindTerm[T]
case object KindType extends StarKindTerm[Nothing]
