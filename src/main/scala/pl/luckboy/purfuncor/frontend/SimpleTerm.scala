/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend
import scala.util.parsing.input.Position
import scalaz._
import pl.luckboy.purfuncor.common._

sealed trait SimpleTerm[+T, +U, +V]
// A lambdaInfo field for types.
case class Let[+T, +U, +V](binds: NonEmptyList[Bind[T, U, V]], body: Term[SimpleTerm[T, U, V]], lambdaInfo: U) extends SimpleTerm[T, U, V]
case class Lambda[+T, +U, +V](args: NonEmptyList[Arg[V]], body: Term[SimpleTerm[T, U, V]], lambdaInfo: U) extends SimpleTerm[T, U, V]
case class Var[+T, +U, +V](loc: T, lambdaInfo: U) extends SimpleTerm[T, U, V]
case class Literal[+T, +U, +V](value: LiteralValue) extends SimpleTerm[T, U, V]
case class TypedTerm[+T, +U, +V](term: Term[SimpleTerm[T, U, V]], typ: Term[V]) extends SimpleTerm[T, U, V]
case class Construct[+T, +U, +V](n: Int, lambdaInfo: U) extends SimpleTerm[T, U, V]
case class Select[+T, +U, +V](term: Term[SimpleTerm[T, U, V]], cases: NonEmptyList[Case[T, U, V]], lambdaInfo: U) extends SimpleTerm[T, U, V]
case class Extract[+T, +U, +V](term: Term[SimpleTerm[T, U, V]], args: NonEmptyList[Arg[V]], body: Term[SimpleTerm[T, U, V]], lambdaInfo: U) extends SimpleTerm[T, U, V]

case class Bind[+T, +U, +V](name: String, body: Term[SimpleTerm[T, U, V]], pos: Position)

case class Arg[+V](name: Option[String], typ: Option[Term[V]], pos: Position)

case class Case[+T, +U, +V](name: Option[String], typ: Option[CaseType[V]], body: Term[SimpleTerm[T, U, V]], lambdaInfo: U)

sealed trait CaseType[+V]
{
  def term: Term[V]
  
  def isDefinedCaseType = isInstanceOf[DefinedCaseType[V]]
}

case class DefinedCaseType[+V](term: Term[V]) extends CaseType[V]
case class MatchingCaseType[+V](term: Term[V]) extends CaseType[V]
