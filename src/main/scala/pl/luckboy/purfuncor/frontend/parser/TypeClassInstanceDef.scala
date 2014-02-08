/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.parser
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

case class TypeClassInstanceDef(
    typeArgs: List[TypeArg],
    typeClassSym: Symbol,
    typeClassArgs: List[Term[TypeSimpleTerm[Symbol, TypeLambdaInfo]]],
    memberValues: NonEmptyList[MemberValue])

case class MemberValue(
    name: String,
    args: List[Arg[TypeSimpleTerm[Symbol, TypeLambdaInfo]]],
    body: Term[SimpleTerm[Symbol, LambdaInfo, TypeSimpleTerm[Symbol, TypeLambdaInfo]]], 
    pos: Position)
