/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.kinder._

case class TypeMatchingCondition[T](
    firstArgIdxs: Map[Int, Int],
    secondArgIdxs: Map[Int, Int],
    matches: Seq[TypeValueTermMatch[T]],
    otherParams: Set[Int],
    lambdaParams: Set[Int])
    
case class TypeValueTermMatch[T](
    params: Set[Int],
    term: Option[TypeValueTerm[T]],
    kind: InferredKind)
