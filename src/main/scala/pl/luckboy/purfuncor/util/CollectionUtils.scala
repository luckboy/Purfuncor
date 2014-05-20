/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.util
import scala.collection.TraversableLike
import scalaz._
import scalaz.Scalaz._

object CollectionUtils
{
  //
  // map
  //
  
  def mapToReversedListValidation[E, T, U](xs: Iterable[T])(f: T => Validation[E, U]) =
    xs.foldLeft(List[U]().success[E]) {
      case (Success(ys), x) => f(x).map { _ :: ys }
      case (res,  _)        => res
    }
  
  def mapToListValidation[E, T, U](xs: Iterable[T])(f: T => Validation[E, U]) =
    mapToReversedListValidation(xs)(f).map { _.reverse }
  
  def mapToVectorValidation[E, T, U](xs: Iterable[T])(f: T => Validation[E, U]) =
    xs.foldLeft(Vector[U]().success[E]) {
      case (Success(ys), x) => f(x).map { ys :+ _ }
      case (res,  _)        => res
    }

  def mapToSetValidation[E, T, U](xs: Iterable[T])(f: T => Validation[E, U]) =
    xs.foldLeft(Set[U]().success[E]) {
      case (Success(ys), x) => f(x).map { ys + _ }
      case (res,  _)        => res
    }
  
  def mapToMapValidation[E, T, U, V](xs: Iterable[T])(f: T => Validation[E, (U, V)]) =
    xs.foldLeft(Map[U, V]().success[E]) {
      case (Success(ys), x) => f(x).map { ys + _ }
      case (res,  _)        => res
    }  
  
  def mapToReversedListS[T, U, S](xs: Iterable[T])(f: (T, S) => (S, U))(s: S) =
    xs.foldLeft((s, List[U]())) {
      case ((s2, ys), x) => 
        val (s3, y) = f(x, s2)
        (s3, y :: ys)
    }
  
  def mapToListS[E, T, U, S](xs: Iterable[T])(f: (T, S) => (S, U))(s: S) =
    mapToReversedListS(xs)(f)(s).mapElements(identity, _.reverse)

  def mapToVectorS[T, U, S](xs: Iterable[T])(f: (T, S) => (S, U))(s: S) =
    xs.foldLeft((s, Vector[U]())) {
      case ((s2, ys), x) => 
        val (s3, y) = f(x, s2)
        (s3, ys :+ y)
    }

  def mapToSetS[T, U, S](xs: Iterable[T])(f: (T, S) => (S, U))(s: S) =
    xs.foldLeft((s, Set[U]())) {
      case ((s2, ys), x) => 
        val (s3, y) = f(x, s2)
        (s3, ys + y)
    }
  
  def mapToMapS[T, U, V, S](xs: Iterable[T])(f: (T, S) => (S, (U, V)))(s: S) =
    xs.foldLeft((s, Map[U, V]())) {
      case ((s2, ys), x) => 
        val (s3, y) = f(x, s2)
        (s3, ys + y)
    }
  
  def mapToReversedListValidationS[E, T, U, S](xs: Iterable[T])(f: (T, S) => (S, Validation[E, U]))(s: S) =
    xs.foldLeft((s, List[U]().success[E])) {
      case ((s2, Success(ys)), x) =>
        val (s3, res) = f(x, s2)
        (s3, res.map { _ :: ys })
      case (t, _)                 =>
        t
    }
  
  def mapToListValidationS[E, T, U, S](xs: Iterable[T])(f: (T, S) => (S, Validation[E, U]))(s: S) =
    mapToReversedListValidationS(xs)(f)(s).mapElements(identity, _.map { _.reverse })
  
  def mapToVectorValidationS[E, T, U, S](xs: Iterable[T])(f: (T, S) => (S, Validation[E, U]))(s: S) =
    xs.foldLeft((s, Vector[U]().success[E])) {
      case ((s2, Success(ys)), x) =>
        val (s3, res) = f(x, s2)
        (s3, res.map { ys :+ _ })
      case (t, _)                 =>
        t
    }
  
  def mapToSetValidationS[E, T, U, S](xs: Iterable[T])(f: (T, S) => (S, Validation[E, U]))(s: S) =
    xs.foldLeft((s, Set[U]().success[E])) {
      case ((s2, Success(ys)), x) =>
        val (s3, res) = f(x, s2)
        (s3, res.map { ys + _ })
      case (t, _)                 =>
        t
    }
  
  def mapToMapValidationS[E, T, U, V, S](xs: Iterable[T])(f: (T, S) => (S, Validation[E, (U, V)]))(s: S) =
    xs.foldLeft((s, Map[U, V]().success[E])) {
      case ((s2, Success(ys)), x) =>
        val (s3, res) = f(x, s2)
        (s3, res.map { ys + _ })
      case (t, _)                 =>
        t
    }
  
  def mapToReversedListM[T, U, S](xs: Iterable[T])(f: T => State[S, U]) =
    State(mapToReversedListS(xs)({ f(_).run(_: S) }))
    
  def mapToListM[T, U, S](xs: Iterable[T])(f: T => State[S, U]) =
    State(mapToListS(xs)({ f(_).run(_: S) }))

  def mapToVectorM[T, U, S](xs: Iterable[T])(f: T => State[S, U]) =
    State(mapToVectorS(xs)({ f(_).run(_: S) }))

  def mapToSetM[T, U, S](xs: Iterable[T])(f: T => State[S, U]) =
    State(mapToSetS(xs)({ f(_).run(_: S) }))

  def mapToMapM[T, U, V, S](xs: Iterable[T])(f: T => State[S, (U, V)]) =
    State(mapToMapS(xs)({ f(_).run(_: S) }))

  def mapToReversedListValudationM[E, T, U, S](xs: Iterable[T])(f: T => State[S, Validation[E, U]]) =
    State(mapToReversedListValidationS(xs)({ f(_).run(_: S) }))
    
  def mapToListValidationM[E, T, U, S](xs: Iterable[T])(f: T => State[S, Validation[E, U]]) =
    State(mapToListValidationS(xs)({ f(_).run(_: S) }))

  def mapToVectorValidationM[E, T, U, S](xs: Iterable[T])(f: T => State[S, Validation[E, U]]) =
    State(mapToVectorValidationS(xs)({ f(_).run(_: S) }))

  def mapToSetValidationM[E, T, U, S](xs: Iterable[T])(f: T => State[S, Validation[E, U]]) =
    State(mapToSetValidationS(xs)({ f(_).run(_: S) }))

  def mapToMapValidationM[E, T, U, V, S](xs: Iterable[T])(f: T => State[S, Validation[E,(U, V)]]) =
    State(mapToMapValidationS(xs)({ f(_).run(_: S) }))
    
  // flatMap
        
  def flatMapToListValidation[E, T, U](xs: Iterable[T])(f: T => Validation[E, Iterable[U]]) =
    xs.foldLeft(List[U]().success[E]) {
      case (Success(ys), x) => f(x).map { ys ++ _ }
      case (res,  _)        => res
    }
  
  def flatMapToVectorValidation[E, T, U](xs: Iterable[T])(f: T => Validation[E, Iterable[U]]) =
    xs.foldLeft(Vector[U]().success[E]) {
      case (Success(ys), x) => f(x).map { ys ++ _ }
      case (res,  _)        => res
    }

  def flatMapToSetValidation[E, T, U](xs: Iterable[T])(f: T => Validation[E, Iterable[U]]) =
    xs.foldLeft(Set[U]().success[E]) {
      case (Success(ys), x) => f(x).map { ys ++ _ }
      case (res,  _)        => res
    }
  
  def flatMapToMapValidation[E, T, U, V](xs: Iterable[T])(f: T => Validation[E, Iterable[(U, V)]]) =
    xs.foldLeft(Map[U, V]().success[E]) {
      case (Success(ys), x) => f(x).map { ys ++ _ }
      case (res,  _)        => res
    }
  
  def flatMapToListS[T, U, S](xs: Iterable[T])(f: (T, S) => (S, Iterable[U]))(s: S) =
    xs.foldLeft((s, List[U]())) {
      case ((s2, ys), x) => 
        val (s3, zs) = f(x, s2)
        (s3, ys ++ zs)
    }
  
  def flatMapToVectorS[T, U, S](xs: Iterable[T])(f: (T, S) => (S, Iterable[U]))(s: S) =
    xs.foldLeft((s, Vector[U]())) {
      case ((s2, ys), x) => 
        val (s3, zs) = f(x, s2)
        (s3, ys ++ zs)
    }

  def flatMapToSetS[T, U, S](xs: Iterable[T])(f: (T, S) => (S, Iterable[U]))(s: S) =
    xs.foldLeft((s, Set[U]())) {
      case ((s2, ys), x) => 
        val (s3, zs) = f(x, s2)
        (s3, ys ++ zs)
    }
  
  def flatMapToMapS[T, U, V, S](xs: Iterable[T])(f: (T, S) => (S, Iterable[(U, V)]))(s: S) =
    xs.foldLeft((s, Map[U, V]())) {
      case ((s2, ys), x) => 
        val (s3, zs) = f(x, s2)
        (s3, ys ++ zs)
    }
  
  def flatMapToListValidationS[E, T, U, S](xs: Iterable[T])(f: (T, S) => (S, Validation[E, Iterable[U]]))(s: S) =
    xs.foldLeft((s, List[U]().success[E])) {
      case ((s2, Success(ys)), x) =>
        val (s3, res) = f(x, s2)
        (s3, res.map { ys ++ _ })
      case (t, _)                 =>
        t
    }
  
  
  def flatMapToVectorValidationS[E, T, U, S](xs: Iterable[T])(f: (T, S) => (S, Validation[E, Iterable[U]]))(s: S) =
    xs.foldLeft((s, Vector[U]().success[E])) {
      case ((s2, Success(ys)), x) =>
        val (s3, res) = f(x, s2)
        (s3, res.map { ys ++ _ })
      case (t, _)                 =>
        t
    }
  
  def flatMapToSetValidationS[E, T, U, S](xs: Iterable[T])(f: (T, S) => (S, Validation[E, Iterable[U]]))(s: S) =
    xs.foldLeft((s, Set[U]().success[E])) {
      case ((s2, Success(ys)), x) =>
        val (s3, res) = f(x, s2)
        (s3, res.map { ys ++ _ })
      case (t, _)                 =>
        t
    }
  
  def flatMapToMapValidationS[E, T, U, V, S](xs: Iterable[T])(f: (T, S) => (S, Validation[E, Iterable[(U, V)]]))(s: S) =
    xs.foldLeft((s, Map[U, V]().success[E])) {
      case ((s2, Success(ys)), x) =>
        val (s3, res) = f(x, s2)
        (s3, res.map { ys ++ _ })
      case (t, _)                 =>
        t
    }
  
  def flatMapToListM[T, U, S](xs: Iterable[T])(f: T => State[S, Iterable[U]]) =
    State(flatMapToListS(xs)({ f(_).run(_: S) }))

  def flatMapToVectorM[T, U, S](xs: Iterable[T])(f: T => State[S, Iterable[U]]) =
    State(flatMapToVectorS(xs)({ f(_).run(_: S) }))

  def flatMapToSetM[T, U, S](xs: Iterable[T])(f: T => State[S, Iterable[U]]) =
    State(mapToSetS(xs)({ f(_).run(_: S) }))

  def flatMapToMapM[T, U, V, S](xs: Iterable[T])(f: T => State[S, Iterable[(U, V)]]) =
    State(flatMapToMapS(xs)({ f(_).run(_: S) }))

  def flatMapToListValidationM[E, T, U, S](xs: Iterable[T])(f: T => State[S, Validation[E, Iterable[U]]]) =
    State(flatMapToListValidationS(xs)({ f(_).run(_: S) }))

  def flatMapToVectorValidationM[E, T, U, S](xs: Iterable[T])(f: T => State[S, Validation[E, Iterable[U]]]) =
    State(flatMapToVectorValidationS(xs)({ f(_).run(_: S) }))

  def flatMapToSetValidationM[E, T, U, S](xs: Iterable[T])(f: T => State[S, Validation[E, Iterable[U]]]) =
    State(flatMapToSetValidationS(xs)({ f(_).run(_: S) }))

  def flatMapToMapValidationM[E, T, U, V, S](xs: Iterable[T])(f: T => State[S, Validation[E, Iterable[(U, V)]]]) =
    State(flatMapToMapValidationS(xs)({ f(_).run(_: S) }))
    
  //
  // foldLeft
  //
    
  def foldLeftValidation[E, T, U](xs: Iterable[T])(zRes: Validation[E, U])(f: (U, T) => Validation[E, U]) =
    xs.foldLeft(zRes) {
      case (Success(y), x) => f(y, x)
      case (yRes, _)       => yRes
    }
  
  def foldLeftS[T, U, S](xs: Iterable[T])(z: U)(f: (U, T, S) => (S, U))(s: S) =
    xs.foldLeft((s, z)) { case ((s2, y), x) => f(y, x, s2) }
  
  def foldLeftValidationS[E, T, U, S](xs: Iterable[T])(zRes: Validation[E, U])(f: (U, T, S) => (S, Validation[E, U]))(s: S) =
    xs.foldLeft((s, zRes)) {
      case ((s2, Success(y)), x) => f(y, x, s2)
      case (yRes, _)             => yRes
    }
  
  def foldLeftM[T, U, S](xs: Iterable[T])(z: U)(f: (U, T) => State[S, U]) =
    State(foldLeftS(xs)(z) { f(_, _).run(_: S) })
    
  def foldLeftValidationM[E, T, U, S](xs: Iterable[T])(zRes: Validation[E, U])(f: (U, T) => State[S, Validation[E, U]]) =
    State(foldLeftValidationS(xs)(zRes) { f(_, _).run(_: S) })
  
  //
  // foldRight
  //
    
  def foldRightValidation[E, T, U](xs: Iterable[T])(zRes: Validation[E, U])(f: (T, U) => Validation[E, U]) =
    xs.foldRight(zRes) {
      case (x, Success(y)) => f(x, y)
      case (_, yRes)       => yRes
    }
  
  def foldRightS[T, U, S](xs: Iterable[T])(z: U)(f: (T, U, S) => (S, U))(s: S) =
    xs.foldRight((s, z)) { case (x, (s2, y)) => f(x, y, s2) }
  
  def foldRightValidationS[E, T, U, S](xs: Iterable[T])(zRes: Validation[E, U])(f: (T, U, S) => (S, Validation[E, U]))(s: S) =
    xs.foldRight((s, zRes)) {
      case (x, (s2, Success(y))) => f(x, y, s2)
      case (_, yRes)             => yRes
    }
  
  def foldRightM[T, U, S](xs: Iterable[T])(z: U)(f: (T, U) => State[S, U]) =
    State(foldRightS(xs)(z) { f(_, _).run(_: S) })
    
  def foldRightValidationM[E, T, U, S](xs: Iterable[T])(zRes: Validation[E, U])(f: (T, U) => State[S, Validation[E, U]]) =
    State(foldRightValidationS(xs)(zRes) { f(_, _).run(_: S) })
}
