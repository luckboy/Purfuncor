/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.kinder.Kind
import pl.luckboy.purfuncor.frontend.kinder.NoKind
import pl.luckboy.purfuncor.frontend.kinder.InferredKind
import pl.luckboy.purfuncor.frontend.kinder.InferringKind
import pl.luckboy.purfuncor.common.Inferrer._

object TypeValueTermKindInferrer
{  
  def inferTypeValueTermKindS[T, U, E](term: TypeValueTerm[T])(env: E)(implicit inferrer: Inferrer[U, E, Kind], envSt: KindInferrenceEnvironmentState[E, T]): (E, Kind) =
    term match {
      case TupleType(args) =>
        val (env2, res) = inferTypeValueTermKindsS(args)(env)
        res.map { appStarKindS(_)(env2) }.valueOr { (env2, _) }
      case BuiltinType(bf, args) =>
        val (env2, res) = inferTypeValueTermKindsS(args)(env)
        res.map { appStarKindS(_)(env2) }.valueOr { (env2, _) }
      case Unittype(loc, args, _) =>
        val (env2, res) = inferTypeValueTermKindsS(args)(env)
        res.map { appStarKindS(_)(env2) }.valueOr { (env2, _) }
      case GlobalTypeApp(loc, args, _) =>
        val (env2, funKind) = envSt.globalTypeVarKindFromEnvironmentS(loc)(env)
        val (env3, res) = inferTypeValueLambdaKindsS(args)(env2)
        res.map { appInfoS(funKind, _)(env3) }.valueOr { (env3, _) }
      case TypeParamApp(param, args, _) =>
        val (env2, funKind) = envSt.typeParamKindFromEnvironmentS(param)(env)
        val (env3, res) = inferTypeValueLambdaKindsS(args)(env2)
        res.map { appInfoS(funKind, _)(env3) }.valueOr { (env3, _) }
      case TypeConjunction(terms) =>
        val (env2, res) = inferTypeValueTermKindsS(terms.toSeq)(env)
        res.map { appStarKindS(_)(env2) }.valueOr { (env2, _) }
      case TypeDisjunction(terms) =>
        val (env2, res) = inferTypeValueTermKindsS(terms.toSeq)(env)
        res.map { appStarKindS(_)(env2) }.valueOr { (env2, _) }
    }

  def inferTypeValueTermKindsS[T, U, E](terms: Seq[TypeValueTerm[T]])(env: E)(implicit inferrer: Inferrer[U, E, Kind], envSt: KindInferrenceEnvironmentState[E, T]) =
    terms.foldLeft((env, Seq[Kind]().success[NoKind])) {
      case ((newEnv, Success(kinds)), term) => 
        inferTypeValueTermKindS(term)(newEnv) match {
          case (newEnv2, noKind: NoKind) => (newEnv2, noKind.failure)
          case (newEnv2, kind)           => (newEnv2, (kinds :+ kind).success)
        }
      case ((newEnv, Failure(noKind)), _)     =>
        (newEnv, noKind.failure)
    }
  
  def inferTypeValueLambdaKindS[T, U, E](lambda: TypeValueLambda[T])(env: E)(implicit inferrer: Inferrer[U, E, Kind], envSt: KindInferrenceEnvironmentState[E, T]) = {
    val (env2, kind) = inferTypeValueTermKindS(lambda.body)(env)
    kind match {
      case noKind: NoKind =>
        (env2, noKind)
      case _              =>
       val (env3, argParamKinds) = lambda.argParams.foldLeft((env2, Seq[Kind]())) {
         case ((newEnv, ks), p) => envSt.typeParamKindFromEnvironmentS(p)(newEnv).mapElements(identity, ks :+ _)
       }
       lambdaKindS(argParamKinds, kind)(env3)
    }
  }

  def inferTypeValueLambdaKindsS[T, U, E](lambdas: Seq[TypeValueLambda[T]])(env: E)(implicit inferrer: Inferrer[U, E, Kind], envSt: KindInferrenceEnvironmentState[E, T]) =
    lambdas.foldLeft((env, Seq[Kind]().success[NoKind])) {
      case ((newEnv, Success(kinds)), lambda) =>
        inferTypeValueLambdaKindS(lambda)(newEnv) match {
          case (newEnv2, noKind: NoKind) => (newEnv2, noKind.failure)
          case (newEnv2, kind)           => (newEnv2, (kinds :+ kind).success)
        }
      case ((newEnv, Failure(noKind)), _)        =>
        (newEnv, noKind.failure)
    }
 
  def appStarKindS[T, E](argKinds: Seq[Kind])(env: E)(implicit envSt: KindInferrenceEnvironmentState[E, T]) = {
    val (env2, res) = argKinds.foldLeft((env, ().success[NoKind])) {
      case ((newEnv, Success(_)), kind)   =>
        envSt.unifyStarKindWithKindS(kind)(newEnv) match {
          case (newEnv2, noKind: NoKind) => (newEnv2, noKind.failure)
          case (newEnv2, _)              => (newEnv2, ().success)
        }
      case ((newEnv, Failure(noKind)), _) =>
        (newEnv, noKind.failure)
    }
    (env2, res.map { _ => InferredKind(Star(KindType, NoPosition)) }.valueOr(identity))
  }
  
  def lambdaKindS[T, U, E](argKinds: Seq[Kind], retKind: Kind)(env: E)(implicit inferrer: Inferrer[U, E, Kind], envSt: KindInferrenceEnvironmentState[E, T]) = {
    val funKind = inferrer.functionInfo(argKinds.size)
    val (env2, unifiedFunKind) = inferrer.unifyInfosS(funKind, funKind)(env)
    val (env3, res) = inferrer.argInfosFromInfoS(unifiedFunKind, argKinds.size)(env2)
    res.map {
      funArgKinds =>
      val (env4, res2) = unifyArgInfoListsS(funArgKinds.toList, argKinds.toList)(env3)
      res2.map {
        unifiedFunArgKinds =>
          inferrer.returnInfoFromInfoS(unifiedFunKind, argKinds.size)(env4) match {
            case (env5, noKind: NoKind) =>
              (env5, noKind)
            case (env5, funRetKind)     =>
              inferrer.unifyInfosS(funRetKind, retKind)(env5) match {
                case (env6, noKind: NoKind) => (env6, noKind)
                case (env6, _)              => (env6, unifiedFunKind)
              }
           }
      }.valueOr { (env4, _) }
    }.valueOr { (env3, _) }
  }
  
  def unifyStarKindWithKindS[T, E](kind: Kind)(env: E)(implicit unifier: Unifier[NoKind, KindTerm[StarKindTerm[Int]], E, Int]) =
    kind match {
      case InferredKind(Star(KindType, _)) | InferringKind(Star(KindType, _)) =>
        (env, kind)
      case InferringKind(Star(KindParam(param), _)) =>
        val (env2, rootParamRes) = unifier.findRootParamS(param)(env)
        rootParamRes.map {
          rootParam =>
            unifier.getParamTermS(rootParam)(env2) match {
              case (env3, Some(Star(KindType, _))) =>
                (env3, kind)
              case (env3, None)                    =>
                val (env4, res) = unifier.replaceParamS(rootParam, Star(KindType, NoPosition))(env3)
                (env4, res.map { _ => kind }.valueOr(identity))
              case (env3, _)                       =>
                unifier.mismatchedTermErrorS(env3)
            }
        }.valueOr { (env2, _) }
      case _ =>
        unifier.mismatchedTermErrorS(env)
    }
}
