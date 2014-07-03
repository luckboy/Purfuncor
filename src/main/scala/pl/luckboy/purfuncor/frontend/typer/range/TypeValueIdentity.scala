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
import pl.luckboy.purfuncor.frontend.resolver._
import pl.luckboy.purfuncor.frontend.typer._

sealed trait TypeValueIdentity[+T]

object TypeValueIdentity
{
  def fromLeafTypeValueTerm[T](term: LeafTypeValueTerm[T]): TypeValueIdentity[T] =
    term match {
      case FieldType(i, _) =>
        FieldTypeIdentity(i)
      case BuiltinType(bf @ (TypeBuiltinFunction.FieldSet1 | TypeBuiltinFunction.FieldSet2), args) =>
        val argIdents = args.map {
          case FieldType(i, _)           => FieldSetTypeFieldIdentity(i)
          case TypeParamApp(param, _, _) => FieldSetTypeParamIdentity(param)
          case _                         => NoFieldSetTypeArgIdentity
        }
        BuiltinTypeIdentity(bf, argIdents)
      case BuiltinType(bf, _) =>
        BuiltinTypeIdentity(bf, Nil)
      case Unittype(loc, _, sym) =>
        UnittypeIdentity(loc, sym)
      case Grouptype(loc, _, sym) =>
        GrouptypeIdentity(loc, sym)
      case TypeParamApp(param, _, _) =>
        TypeParamAppIdentity(param)
    }
}

case class FieldTypeIdentity[+T](i: Int) extends TypeValueIdentity[T]
case class BuiltinTypeIdentity[+T](bf: TypeBuiltinFunction.Value, argIdents: Seq[FieldSetTypeArgIdentity]) extends TypeValueIdentity[T]
case class UnittypeIdentity[+T](loc: T, sym: GlobalSymbol) extends TypeValueIdentity[T]
case class GrouptypeIdentity[+T](loc: T, sym: GlobalSymbol) extends TypeValueIdentity[T]
case class ExpandedGlobalTypeAppIdentity[+T](loc: T, sym: GlobalSymbol) extends TypeValueIdentity[T]
case class UnexpandedGlobalTypeAppIdentity[+T](loc: T, sym: GlobalSymbol) extends TypeValueIdentity[T]
case class TypeParamAppIdentity[+T](param: Int) extends TypeValueIdentity[T]

sealed trait FieldSetTypeArgIdentity
case class FieldSetTypeFieldIdentity(i: Int) extends FieldSetTypeArgIdentity
case class FieldSetTypeParamIdentity(param: Int ) extends FieldSetTypeArgIdentity
case object NoFieldSetTypeArgIdentity extends FieldSetTypeArgIdentity
