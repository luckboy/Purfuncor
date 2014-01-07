/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.lmbdindexer

case class LambdaInfo[+T](lambdaInfo: T, idx: Int)
{
  override def toString = (if(!lambdaInfo.toString.isEmpty) "idx=" + lambdaInfo + ";" else "") + idx
}
