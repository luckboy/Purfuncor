/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.util
import scala.collection.immutable.IntMap
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
  
  def mapToIntMapValidation[E, T, U](xs: Iterable[T])(f: T => Validation[E, (Int, U)]) =
    xs.foldLeft(IntMap[U]().success[E]) {
      case (Success(ys), x) => f(x).map { ys + _ }
      case (res,  _)        => res
    }

  def stMapToReversedListS[T, U, S](xs: Iterable[T])(f: (T, S) => (S, U))(s: S) =
    xs.foldLeft((s, List[U]())) {
      case ((s2, ys), x) => 
        val (s3, y) = f(x, s2)
        (s3, y :: ys)
    }
  
  def stMapToListS[E, T, U, S](xs: Iterable[T])(f: (T, S) => (S, U))(s: S) =
    stMapToReversedListS(xs)(f)(s).mapElements(identity, _.reverse)

  def stMapToVectorS[T, U, S](xs: Iterable[T])(f: (T, S) => (S, U))(s: S) =
    xs.foldLeft((s, Vector[U]())) {
      case ((s2, ys), x) => 
        val (s3, y) = f(x, s2)
        (s3, ys :+ y)
    }

  def stMapToSetS[T, U, S](xs: Iterable[T])(f: (T, S) => (S, U))(s: S) =
    xs.foldLeft((s, Set[U]())) {
      case ((s2, ys), x) => 
        val (s3, y) = f(x, s2)
        (s3, ys + y)
    }
  
  def stMapToMapS[T, U, V, S](xs: Iterable[T])(f: (T, S) => (S, (U, V)))(s: S) =
    xs.foldLeft((s, Map[U, V]())) {
      case ((s2, ys), x) => 
        val (s3, y) = f(x, s2)
        (s3, ys + y)
    }
  
  def stMapToIntMapS[T, U, S](xs: Iterable[T])(f: (T, S) => (S, (Int, U)))(s: S) =
    xs.foldLeft((s, IntMap[U]())) {
      case ((s2, ys), x) => 
        val (s3, y) = f(x, s2)
        (s3, ys + y)
    }
  
  def stMapToReversedListValidationS[E, T, U, S](xs: Iterable[T])(f: (T, S) => (S, Validation[E, U]))(s: S) =
    xs.foldLeft((s, List[U]().success[E])) {
      case ((s2, Success(ys)), x) =>
        val (s3, res) = f(x, s2)
        (s3, res.map { _ :: ys })
      case (t, _)                 =>
        t
    }
  
  def stMapToListValidationS[E, T, U, S](xs: Iterable[T])(f: (T, S) => (S, Validation[E, U]))(s: S) =
    stMapToReversedListValidationS(xs)(f)(s).mapElements(identity, _.map { _.reverse })
  
  def stMapToVectorValidationS[E, T, U, S](xs: Iterable[T])(f: (T, S) => (S, Validation[E, U]))(s: S) =
    xs.foldLeft((s, Vector[U]().success[E])) {
      case ((s2, Success(ys)), x) =>
        val (s3, res) = f(x, s2)
        (s3, res.map { ys :+ _ })
      case (t, _)                 =>
        t
    }
  
  def stMapToSetValidationS[E, T, U, S](xs: Iterable[T])(f: (T, S) => (S, Validation[E, U]))(s: S) =
    xs.foldLeft((s, Set[U]().success[E])) {
      case ((s2, Success(ys)), x) =>
        val (s3, res) = f(x, s2)
        (s3, res.map { ys + _ })
      case (t, _)                 =>
        t
    }
  
  def stMapToMapValidationS[E, T, U, V, S](xs: Iterable[T])(f: (T, S) => (S, Validation[E, (U, V)]))(s: S) =
    xs.foldLeft((s, Map[U, V]().success[E])) {
      case ((s2, Success(ys)), x) =>
        val (s3, res) = f(x, s2)
        (s3, res.map { ys + _ })
      case (t, _)                 =>
        t
    }
  
  def stMapToIntMapValidationS[E, T, U, S](xs: Iterable[T])(f: (T, S) => (S, Validation[E, (Int, U)]))(s: S) =
    xs.foldLeft((s, IntMap[U]().success[E])) {
      case ((s2, Success(ys)), x) =>
        val (s3, res) = f(x, s2)
        (s3, res.map { ys + _ })
      case (t, _)                 =>
        t
    }
  
  def stMapToReversedList[T, U, S](xs: Iterable[T])(f: T => State[S, U]) =
    State(stMapToReversedListS(xs)({ f(_).run(_: S) }))
    
  def stMapToList[T, U, S](xs: Iterable[T])(f: T => State[S, U]) =
    State(stMapToListS(xs)({ f(_).run(_: S) }))

  def stMapToVector[T, U, S](xs: Iterable[T])(f: T => State[S, U]) =
    State(stMapToVectorS(xs)({ f(_).run(_: S) }))

  def stMapToSet[T, U, S](xs: Iterable[T])(f: T => State[S, U]) =
    State(stMapToSetS(xs)({ f(_).run(_: S) }))

  def stMapToMap[T, U, V, S](xs: Iterable[T])(f: T => State[S, (U, V)]) =
    State(stMapToMapS(xs)({ f(_).run(_: S) }))
    
  def stMapToIntMap[T, U, S](xs: Iterable[T])(f: T => State[S, (Int, U)]) =
    State(stMapToIntMapS(xs)({ f(_).run(_: S) }))

  def stMapToReversedListValudation[E, T, U, S](xs: Iterable[T])(f: T => State[S, Validation[E, U]]) =
    State(stMapToReversedListValidationS(xs)({ f(_).run(_: S) }))
    
  def stMapToListValidation[E, T, U, S](xs: Iterable[T])(f: T => State[S, Validation[E, U]]) =
    State(stMapToListValidationS(xs)({ f(_).run(_: S) }))

  def stMapToVectorValidation[E, T, U, S](xs: Iterable[T])(f: T => State[S, Validation[E, U]]) =
    State(stMapToVectorValidationS(xs)({ f(_).run(_: S) }))

  def stMapToSetValidation[E, T, U, S](xs: Iterable[T])(f: T => State[S, Validation[E, U]]) =
    State(stMapToSetValidationS(xs)({ f(_).run(_: S) }))

  def stMapToMapValidation[E, T, U, V, S](xs: Iterable[T])(f: T => State[S, Validation[E, (U, V)]]) =
    State(stMapToMapValidationS(xs)({ f(_).run(_: S) }))
    
  def stMapToIntMapValidation[E, T, U, S](xs: Iterable[T])(f: T => State[S, Validation[E, (Int, U)]]) =
    State(stMapToIntMapValidationS(xs)({ f(_).run(_: S) }))
    
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
  
  def flatMapToIntMapValidation[E, T, U](xs: Iterable[T])(f: T => Validation[E, Iterable[(Int, U)]]) =
    xs.foldLeft(IntMap[U]().success[E]) {
      case (Success(ys), x) => f(x).map { ys ++ _ }
      case (res,  _)        => res
    }
  
  def stFlatMapToListS[T, U, S](xs: Iterable[T])(f: (T, S) => (S, Iterable[U]))(s: S) =
    xs.foldLeft((s, List[U]())) {
      case ((s2, ys), x) => 
        val (s3, zs) = f(x, s2)
        (s3, ys ++ zs)
    }
  
  def stFlatMapToVectorS[T, U, S](xs: Iterable[T])(f: (T, S) => (S, Iterable[U]))(s: S) =
    xs.foldLeft((s, Vector[U]())) {
      case ((s2, ys), x) => 
        val (s3, zs) = f(x, s2)
        (s3, ys ++ zs)
    }

  def stFlatMapToSetS[T, U, S](xs: Iterable[T])(f: (T, S) => (S, Iterable[U]))(s: S) =
    xs.foldLeft((s, Set[U]())) {
      case ((s2, ys), x) => 
        val (s3, zs) = f(x, s2)
        (s3, ys ++ zs)
    }
  
  def stFlatMapToMapS[T, U, V, S](xs: Iterable[T])(f: (T, S) => (S, Iterable[(U, V)]))(s: S) =
    xs.foldLeft((s, Map[U, V]())) {
      case ((s2, ys), x) => 
        val (s3, zs) = f(x, s2)
        (s3, ys ++ zs)
    }
  
  def stFlatMapToIntMapS[T, U, S](xs: Iterable[T])(f: (T, S) => (S, Iterable[(Int, U)]))(s: S) =
    xs.foldLeft((s, IntMap[U]())) {
      case ((s2, ys), x) => 
        val (s3, zs) = f(x, s2)
        (s3, ys ++ zs)
    }
  
  def stFlatMapToListValidationS[E, T, U, S](xs: Iterable[T])(f: (T, S) => (S, Validation[E, Iterable[U]]))(s: S) =
    xs.foldLeft((s, List[U]().success[E])) {
      case ((s2, Success(ys)), x) =>
        val (s3, res) = f(x, s2)
        (s3, res.map { ys ++ _ })
      case (t, _)                 =>
        t
    }
  
  def stFlatMapToVectorValidationS[E, T, U, S](xs: Iterable[T])(f: (T, S) => (S, Validation[E, Iterable[U]]))(s: S) =
    xs.foldLeft((s, Vector[U]().success[E])) {
      case ((s2, Success(ys)), x) =>
        val (s3, res) = f(x, s2)
        (s3, res.map { ys ++ _ })
      case (t, _)                 =>
        t
    }
  
  def stFlatMapToSetValidationS[E, T, U, S](xs: Iterable[T])(f: (T, S) => (S, Validation[E, Iterable[U]]))(s: S) =
    xs.foldLeft((s, Set[U]().success[E])) {
      case ((s2, Success(ys)), x) =>
        val (s3, res) = f(x, s2)
        (s3, res.map { ys ++ _ })
      case (t, _)                 =>
        t
    }
  
  def stFlatMapToMapValidationS[E, T, U, V, S](xs: Iterable[T])(f: (T, S) => (S, Validation[E, Iterable[(U, V)]]))(s: S) =
    xs.foldLeft((s, Map[U, V]().success[E])) {
      case ((s2, Success(ys)), x) =>
        val (s3, res) = f(x, s2)
        (s3, res.map { ys ++ _ })
      case (t, _)                 =>
        t
    }
  
  def stFlatMapToIntMapValidationS[E, T, U, S](xs: Iterable[T])(f: (T, S) => (S, Validation[E, Iterable[(Int, U)]]))(s: S) =
    xs.foldLeft((s, IntMap[U]().success[E])) {
      case ((s2, Success(ys)), x) =>
        val (s3, res) = f(x, s2)
        (s3, res.map { ys ++ _ })
      case (t, _)                 =>
        t
    }
  
  def stFlatMapToList[T, U, S](xs: Iterable[T])(f: T => State[S, Iterable[U]]) =
    State(stFlatMapToListS(xs)({ f(_).run(_: S) }))

  def stFlatMapToVector[T, U, S](xs: Iterable[T])(f: T => State[S, Iterable[U]]) =
    State(stFlatMapToVectorS(xs)({ f(_).run(_: S) }))

  def stFlatMapToSet[T, U, S](xs: Iterable[T])(f: T => State[S, Iterable[U]]) =
    State(stFlatMapToSetS(xs)({ f(_).run(_: S) }))

  def stFlatMapToMap[T, U, V, S](xs: Iterable[T])(f: T => State[S, Iterable[(U, V)]]) =
    State(stFlatMapToMapS(xs)({ f(_).run(_: S) }))
    
  def stFlatMapToIntMap[T, U, S](xs: Iterable[T])(f: T => State[S, Iterable[(Int, U)]]) =
    State(stFlatMapToIntMapS(xs)({ f(_).run(_: S) }))

  def stFlatMapToListValidation[E, T, U, S](xs: Iterable[T])(f: T => State[S, Validation[E, Iterable[U]]]) =
    State(stFlatMapToListValidationS(xs)({ f(_).run(_: S) }))

  def stFlatMapToVectorValidation[E, T, U, S](xs: Iterable[T])(f: T => State[S, Validation[E, Iterable[U]]]) =
    State(stFlatMapToVectorValidationS(xs)({ f(_).run(_: S) }))

  def stFlatMapToSetValidation[E, T, U, S](xs: Iterable[T])(f: T => State[S, Validation[E, Iterable[U]]]) =
    State(stFlatMapToSetValidationS(xs)({ f(_).run(_: S) }))

  def stFlatMapToMapValidation[E, T, U, V, S](xs: Iterable[T])(f: T => State[S, Validation[E, Iterable[(U, V)]]]) =
    State(stFlatMapToMapValidationS(xs)({ f(_).run(_: S) }))
    
  def stFlatMapToIntMapValidation[E, T, U, S](xs: Iterable[T])(f: T => State[S, Validation[E, Iterable[(Int, U)]]]) =
    State(stFlatMapToIntMapValidationS(xs)({ f(_).run(_: S) }))
    
  //
  // foldLeft
  //
    
  def foldLeftValidation[E, T, U](xs: Iterable[T])(zRes: Validation[E, U])(f: (U, T) => Validation[E, U]) =
    xs.foldLeft(zRes) {
      case (Success(y), x) => f(y, x)
      case (yRes, _)       => yRes
    }
  
  def stFoldLeftS[T, U, S](xs: Iterable[T])(z: U)(f: (U, T, S) => (S, U))(s: S) =
    xs.foldLeft((s, z)) { case ((s2, y), x) => f(y, x, s2) }
  
  def stFoldLeftValidationS[E, T, U, S](xs: Iterable[T])(zRes: Validation[E, U])(f: (U, T, S) => (S, Validation[E, U]))(s: S) =
    xs.foldLeft((s, zRes)) {
      case ((s2, Success(y)), x) => f(y, x, s2)
      case (yRes, _)             => yRes
    }
  
  def stFoldLeft[T, U, S](xs: Iterable[T])(z: U)(f: (U, T) => State[S, U]) =
    State(stFoldLeftS(xs)(z) { f(_, _).run(_: S) })
    
  def stFoldLeftValidation[E, T, U, S](xs: Iterable[T])(zRes: Validation[E, U])(f: (U, T) => State[S, Validation[E, U]]) =
    State(stFoldLeftValidationS(xs)(zRes) { f(_, _).run(_: S) })
  
  //
  // foldRight
  //
    
  def foldRightValidation[E, T, U](xs: Iterable[T])(zRes: Validation[E, U])(f: (T, U) => Validation[E, U]) =
    xs.foldRight(zRes) {
      case (x, Success(y)) => f(x, y)
      case (_, yRes)       => yRes
    }
  
  def stFoldRightS[T, U, S](xs: Iterable[T])(z: U)(f: (T, U, S) => (S, U))(s: S) =
    xs.foldRight((s, z)) { case (x, (s2, y)) => f(x, y, s2) }
  
  def stFoldRightValidationS[E, T, U, S](xs: Iterable[T])(zRes: Validation[E, U])(f: (T, U, S) => (S, Validation[E, U]))(s: S) =
    xs.foldRight((s, zRes)) {
      case (x, (s2, Success(y))) => f(x, y, s2)
      case (_, yRes)             => yRes
    }
  
  def stFoldRight[T, U, S](xs: Iterable[T])(z: U)(f: (T, U) => State[S, U]) =
    State(stFoldRightS(xs)(z) { f(_, _).run(_: S) })
    
  def stFoldRightValidation[E, T, U, S](xs: Iterable[T])(zRes: Validation[E, U])(f: (T, U) => State[S, Validation[E, U]]) =
    State(stFoldRightValidationS(xs)(zRes) { f(_, _).run(_: S) })
}
