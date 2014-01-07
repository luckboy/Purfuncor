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
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._

sealed trait TypeSimpleTerm[+T, +U]
{
  override def toString =
    this match {
      case TypeLambda(args, body, lambdaInfo) =>
        "\\" + args.map { a => a.kind.map { _ => "(" + a + ")" }.getOrElse(a.toString) + " " }.list.mkString("") +
        (if(lambdaInfo.toString =/= "")  "/*" + lambdaInfo.toString + "*/ " else "") +
        "=> " + typeTermShowing.stringFrom(body)
      case TypeVar(loc)                       =>
        loc.toString
      case TypeLiteral(value)                 =>
        value.toString
      case KindedTypeTerm(term, kind)         =>
        typeTermShowing.stringFrom(term) + ": " + stringKindTermShowing.stringFrom(kind)
    }
}

case class TypeLambda[+T, +U](args: NonEmptyList[TypeArg], body: Term[TypeSimpleTerm[T, U]], lambdaInfo: U) extends TypeSimpleTerm[T, U]
case class TypeVar[+T, +U](loc: T) extends TypeSimpleTerm[T, U]
case class TypeLiteral[+T, +U](value: TypeLiteralValue) extends TypeSimpleTerm[T, U]
case class KindedTypeTerm[+T, +U](term: Term[TypeSimpleTerm[T, U]], kind: KindTerm[StarKindTerm[String]]) extends TypeSimpleTerm[T, U]

case class TypeArg(name: Option[String], kind: Option[KindTerm[StarKindTerm[String]]], pos: Position)
{
  override def toString = name.map { _.toString }.getOrElse("_") + kind.map { k => ": " + stringKindTermShowing.stringFrom(k) }.getOrElse("")
}
