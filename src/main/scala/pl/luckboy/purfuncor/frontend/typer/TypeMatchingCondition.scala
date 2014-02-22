/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer
import scala.collection.immutable.IntMap
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.kinder._
import TypeValueTermUtils._

case class TypeMatchingCondition[T](
    firstArgIdxs: Map[Int, Int],
    secondArgIdxs: Map[Int, Int],
    matchings: Seq[TypeValueTermMatching[T]],
    otherParams: Set[Int],
    lambdaParams: Set[Int])
{
  def withFirstArgIdxs(argIdxs: Map[Int, Int]) = copy(firstArgIdxs = argIdxs)
  
  def withSecondArgIdxs(argIdxs: Map[Int, Int]) = copy(secondArgIdxs = argIdxs)
}

object TypeMatchingCondition
{
  def fromTypeParamUnionsWithTypeValueTermsS[T, U, E](paramUnionsWithTerms: (Map[Int, (Set[Int], Option[TypeValueTerm[T]])]))(env: E)(implicit envSt: TypeInferenceEnvironmentState[E, U, T]) = {
    val terms = paramUnionsWithTerms.map { _._2._2 }
    val otherParams = terms.flatMap { _.map(typeParamsFromTypeValueTerm).getOrElse(Set()) }.toSet
    val lambdaParams = terms.flatMap { _.map(typeArgParamsFromTypeValueTerm).getOrElse(Set()) }.toSet
    val (env2, res) = paramUnionsWithTerms.foldLeft((env, IntMap[TypeValueTermMatching[T]]().success[NoType[T]])) {
      case ((newEnv, Success(matchings)), (rootParam, (params, term))) =>
        val (newEnv2, newRes) = envSt.inferTypeValueTermKindS(TypeParamApp(rootParam, Nil, 0))(newEnv)
        newRes.map {
          kind =>
            val (newEnv3, newRes2) = envSt.inferredKindFromKindS(kind)(newEnv2)
            newRes2.map {
              inferredKind =>
                (newEnv3, (matchings + (rootParam -> TypeValueTermMatching(params, term, inferredKind))).success)
            }.valueOr { nt => (newEnv3, nt.failure) }
        }.valueOr { nt => (newEnv2, nt.failure) }
    }
    (env2, res.map { ms => TypeMatchingCondition(Map(), Map(), ms.values.toVector, otherParams, lambdaParams) })
  }
}
    
case class TypeValueTermMatching[T](
    params: Set[Int],
    term: Option[TypeValueTerm[T]],
    kind: InferredKind)
