/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.parser
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

case class DatatypeDef(
    sym: Symbol,
    kind: Option[KindTerm[StarKindTerm[String]]],
    args: List[TypeArg],
    extendType: Option[Term[TypeSimpleTerm[Symbol, TypeLambdaInfo]]],
    constrs: NonEmptyList[Constructor])

sealed trait Constructor
{
  def sym: Symbol
  
  def extendType: Option[Term[TypeSimpleTerm[Symbol, TypeLambdaInfo]]]
}

case class UnnamedFieldConstructor(
    sym: Symbol,
    fieldTypes: List[Term[TypeSimpleTerm[Symbol, TypeLambdaInfo]]],
    extendType: Option[Term[TypeSimpleTerm[Symbol, TypeLambdaInfo]]]) extends Constructor
case class NamedFieldConstructor(
    sym: Symbol,
    fields: List[NamedField],
    extendType: Option[Term[TypeSimpleTerm[Symbol, TypeLambdaInfo]]]) extends Constructor

case class NamedField(
    name: String,
    typ: Term[TypeSimpleTerm[Symbol, TypeLambdaInfo]], 
    default: Option[Term[SimpleTerm[Symbol, LambdaInfo, TypeSimpleTerm[Symbol, TypeLambdaInfo]]]])
