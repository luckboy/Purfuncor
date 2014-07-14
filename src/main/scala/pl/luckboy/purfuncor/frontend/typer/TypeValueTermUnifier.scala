/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer
import scala.annotation.tailrec
import scala.collection.immutable.IntMap
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.typer.range._
import pl.luckboy.purfuncor.frontend.kinder.Kind
import pl.luckboy.purfuncor.frontend.kinder.NoKind
import pl.luckboy.purfuncor.frontend.kinder.InferredKind
import pl.luckboy.purfuncor.frontend.kinder.InferringKind
import pl.luckboy.purfuncor.common.Unifier._
import pl.luckboy.purfuncor.frontend.KindTermUtils._
import pl.luckboy.purfuncor.util.CollectionUtils._
import pl.luckboy.purfuncor.util.StateUtils._
import TypeValueTermUtils._
import LogicalTypeValueTermUnifier._

object TypeValueTermUnifier
{
  def matchesTypeValueTermListsWithReturnKindS[T, U, V, E](terms1: Seq[TypeValueTerm[T]], terms2: Seq[TypeValueTerm[T]])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]) = {
    val (env2, savedTypeMatching) = envSt.currentTypeMatchingFromEnvironmentS(env)
    val (env3, _) = envSt.setCurrentTypeMatchingS(TypeMatching.Types)(env2)
    val (env6, res2) = if(terms1.size === terms2.size) {
      val (env4, res) = stFoldLeftValidationS(terms1.zip(terms2))((z, Vector[Kind]()).success[NoType[T]]) {
        (pair1, pair2, newEnv: E) =>
          val (x, kinds) = pair1
          val (term1, term2) = pair2
          val (newEnv2, newRes) = matchesTypeValueTermsS(term1, term2)(x)(f)(newEnv: E)
          newRes.map {
            x2 => envSt.returnKindFromEnvironmentS(newEnv2).mapElements(identity, k => (x2, kinds :+ k).success)
          }.valueOr { nt => (newEnv2: E, nt.failure) }
      } (env3)
      res.flatMap { 
        case (x, argKinds) =>
          val (env5, res2) = envSt.appStarKindS(argKinds)(env4)
          res2.map { envSt.setReturnKindS(_)(env5).mapElements(identity, _ => x.success) }
      }.valueOr { nt => (env4, nt.failure) }
    } else
      (env3, NoType.fromError[T](FatalError("unequal list lengths", none, NoPosition)).failure)
    envSt.setCurrentTypeMatchingS(savedTypeMatching)(env6).mapElements(identity, _ => res2)
  }
  
  def matchesTypeValueLambdaListsWithReturnKindS[T, U, V, E](lambdas1: Seq[TypeValueLambda[T]], lambdas2: Seq[TypeValueLambda[T]], funKind: Kind)(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]) = {
    val (env2, savedTypeMatching) = envSt.currentTypeMatchingFromEnvironmentS(env)
    val (env3, _) = envSt.setCurrentTypeMatchingS(TypeMatching.Types)(env2)
    val (env6, res2) = if(lambdas1.size === lambdas2.size) {
      val (env4, res) = stFoldLeftValidationS(lambdas1.zip(lambdas2))((z, Vector[Kind]()).success[NoType[T]]) {
        (pair1, pair2, newEnv: E) =>
          val (x, kinds) = pair1
          val (lambda1, lambda2) = pair2
          val (newEnv2, newRes) = matchesTypeValueLambdasS(lambda1, lambda2)(x)(f)(newEnv)
          newRes.map {
            x2 => envSt.returnKindFromEnvironmentS(newEnv2).mapElements(identity, k => (x2, kinds :+ k).success)
          }.valueOr { nt => (newEnv2, nt.failure) }
      } (env3)
      res.flatMap {
        case (x, argKinds) => 
          val (env5, res2) = envSt.appKindS(funKind, argKinds)(env4)
          res2.map { envSt.setReturnKindS(_)(env5).mapElements(identity, _ => x.success) }
      }.valueOr { nt => (env4, nt.failure) }
    } else
      (env3, NoType.fromError[T](FatalError("unequal list lengths", none, NoPosition)).failure)
    envSt.setCurrentTypeMatchingS(savedTypeMatching)(env6).mapElements(identity, _ => res2)
  }
  
  private def typeValueLambdasFromParamsS[T, U, E](params: Seq[Int])(env: E)(implicit envSt: TypeInferenceEnvironmentState[E, U, T]): (E, Validation[NoType[T], Seq[TypeValueLambda[T]]]) =
    stMapToVectorValidationS(params) {
      (param, newEnv: E) =>
        val (newEnv2, res) = envSt.allocateTypeParamAppIdxS(newEnv)
        (newEnv2, res.map {
          pai => TypeValueLambda[T](Nil, TypeParamApp(param, Nil, pai)).success
        }.valueOr { _.failure })
    } (env)
  
  private def withTypeLambdaArgsWithReturnKindS[T, U, V, E](argParams: Seq[Set[Int]])(f: E => (E, Validation[NoType[T], U]))(env: E)(implicit envSt: TypeInferenceEnvironmentState[E, V, T]): (E, Validation[NoType[T], U]) =
    envSt.withTypeLambdaArgsS(argParams) {
      env2 =>
        val (env3, res) = stMapToVectorValidationS(argParams) {
          (argParamSet, newEnv: E) =>
            val argParamSeq = argParamSet.toSeq
            argParamSeq.headOption.map {
              argParam =>
                val (newEnv2, kindRes) = envSt.inferTypeValueTermKindS(TypeParamApp(argParam, Nil, 0))(newEnv)
                stFoldLeftValidationS(argParamSeq)(kindRes.valueOr { _.toNoKind }.success[NoType[T]]) {
                  (kind1, param2, newEnv3: E) =>
                    val (newEnv4, kindRes2) = envSt.inferTypeValueTermKindS(TypeParamApp(param2, Nil, 0))(newEnv3)
                    envSt.unifyKindsS(kind1, kindRes2.valueOr { _.toNoKind })(newEnv4)
                } (newEnv2)
            }.getOrElse((newEnv, NoType.fromError[T](FatalError("no type arguments", none, NoPosition)).failure))
        } (env2)
        res match {
          case Success(argKinds)      =>
            val (env4, res2) = f(env3)
            res2.map {
              x =>
                (for {
                  retKind <- State(envSt.returnKindFromEnvironmentS)
                  lambdaKindRes <- State(envSt.lambdaKindS(argKinds, retKind))
                  _ <- State(envSt.setReturnKindS(lambdaKindRes.valueOr { _.toNoKind }))
                } yield (x.success)).run(env4)
            }.valueOr { nt => (env4, nt.failure) }
          case Failure(noType) =>
            (env3, noType.failure)
        }
    } (env)
  
  @tailrec
  def matchesTypeValueLambdasS[T, U, V, E](lambda1: TypeValueLambda[T], lambda2: TypeValueLambda[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]): (E, Validation[NoType[T], U]) =
    (lambda1, lambda2) match {
      case (TypeValueLambda(argParams1, body1), TypeValueLambda(argParams2, body2)) if argParams1.size === argParams2.size =>
        val argParams = argParams1.zip(argParams2).map { case (p1, p2) => Set(p1, p2) }
        withTypeLambdaArgsWithReturnKindS(argParams) { matchesTypeValueTermsS(body1, body2)(z)(f)(_: E) } (env)
      case (TypeValueLambda(argParams1, typeApp1: TypeApp[T]), TypeValueLambda(argParams2, body2)) if argParams1.size < argParams2.size =>
        val otherArgParams = argParams2.drop(argParams1.size)
        val argParams = argParams1.zip(argParams2).map { case (p1, p2) => Set(p1, p2) } ++ otherArgParams.map(Set(_))
        withTypeLambdaArgsWithReturnKindS(argParams) {
          (env2: E) =>
            typeValueLambdasFromParamsS(otherArgParams)(env2) match {
              case (env3, Success(otherArgs)) =>
                val term1 = typeApp1.withArgs(typeApp1.args ++ otherArgs)
                envSt.inferTypeValueTermKindS(term1)(env3) match {
                  case (env4, Success(_))      => matchesTypeValueTermsS(term1, body2)(z)(f)(env4)
                  case (env4, Failure(noType)) => (env4, noType.failure)
                }
              case (env3, Failure(noType))    => (env3, noType.failure)
            }
        } (env)
      case (_, TypeValueLambda(_, _: TypeApp[T])) =>
        val (env2, _) = reverseTypeMatchingS(env)
        matchesTypeValueLambdasS(lambda2, lambda1)(z)(f)(env2)
      case (_, _) =>
        unifier.mismatchedTermErrorS(env).mapElements(identity, _.failure)
    }
  
  def partiallyInstantiateTypeValueTermForMarkedParamsS[T, E](term: TypeValueTerm[T])(markedParams: Set[Int])(err: E => (E, NoType[T]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int]): (E, Validation[NoType[T], (TypeValueTerm[T], Option[Int])]) =
    term match {
      case TypeParamApp(param, args, paramAppIdx) =>
        val (env2, rootParamRes) = unifier.findRootParamS(param)(env)
        rootParamRes match {
          case Success(rootParam) =>
          	val (env3, optParamTerm) = unifier.getParamTermS(rootParam)(env2)
          	if(!markedParams.contains(rootParam))
              optParamTerm match {
          	    case Some(paramTerm) =>
          	      val (env4, res) = partiallyInstantiateTypeValueTermForMarkedParamsS(paramTerm)(markedParams + rootParam)(err)(env3)
          	      res.map {
          	        case (GlobalTypeApp(loc2, args2, sym2), _) => 
          	          (env4, (GlobalTypeApp(loc2, args2 ++ args, sym2), some(rootParam)).success)
          	        case (TypeParamApp(param2, args2, _), _)   =>
          	          (env4, (TypeParamApp(param2, args2 ++ args, paramAppIdx), some(rootParam)).success)
          	        case (term2, _)                            =>
          	          if(args.isEmpty) 
          	            (env4, (term2, some(rootParam)).success)
          	          else
          	            (env4, NoType.fromError[T](FatalError("type value term isn't type application", none, NoPosition)).failure)
          	      }.valueOr { nt => (env4, nt.failure) }
          	    case None            =>
          	      (env3, (term, none).success)
          	  }
          	else
          	  err(env3).mapElements(identity, _.failure)
          case Failure(noType)    =>
            (env2, noType.failure)
        }
      case _                                      =>
        (env, (term, none).success)
    }

  def inifityTypeValueTermNoType[T] = NoType.fromError[T](FatalError("infinity type value term", none, NoPosition))

  def partiallyInstantiateTypeValueTermS[T, E](term: TypeValueTerm[T])(err: E => (E, NoType[T]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int]) =
    partiallyInstantiateTypeValueTermForMarkedParamsS(term)(Set())(err)(env)
    
  private def setReturnKindFromTypeValueTermsS[T, U, E](term1: TypeValueTerm[T], term2: TypeValueTerm[T])(env: E)(implicit envSt: TypeInferenceEnvironmentState[E, U, T]) =
    (for {
      funKindRes1 <- State(envSt.inferTypeValueTermKindS(term1))
      funKindRes2 <- State(envSt.inferTypeValueTermKindS(term2))
      unifiedFunKindRes <- State(envSt.unifyKindsS(funKindRes1.valueOr { _.toNoKind }, funKindRes2.valueOr { _.toNoKind }))
      _ <- State(envSt.setReturnKindS(unifiedFunKindRes.valueOr { _.toNoKind }))
    } yield ()).run(env)
    
  private def setReturnKindFromTypeValueTerms[T, U, E](term1: TypeValueTerm[T], term2: TypeValueTerm[T])(implicit envSt: TypeInferenceEnvironmentState[E, U, T]) =
    State(setReturnKindFromTypeValueTermsS[T, U, E](term1, term2))
    
  private def mismatchedTypeValueTermNoTypeWithReturnKindS[T, U, E](term1: TypeValueTerm[T], term2: TypeValueTerm[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    (for {
      res <- setReturnKindFromTypeValueTerms(term1, term2)
      noType <- State(unifier.mismatchedTermErrorS)
    } yield noType).run(env)

  private def addDelayedErrorsFromResultS[T, U, V, E](res: Validation[NoType[T], U], paramAppIdxs: Set[Int])(z: U)(env: E)(implicit envSt: TypeInferenceEnvironmentState[E, V, T]) =
    res.map { x => (env, x.success) }.valueOr {
      nt => 
        envSt.returnKindFromEnvironmentS(env) match {
          case (env2, noKind: NoKind) =>
            (env2, NoType.fromNoKind[T](noKind).failure)
          case (env2, _)              => 
            val (env3, res2) = envSt.inferringKindFromKindS(InferredKind(Star(KindParam(0), NoPosition)))(env2)
            res2.map {
              inferringKind =>
                val (env4, _) = envSt.setReturnKindS(inferringKind)(env3)
                envSt.addDelayedErrorsS(paramAppIdxs.map { (_, nt) }.toMap)(env4).mapElements(identity, _ => z.success)
            }.valueOr { nt => (env3, nt.failure) }
        }
    }
    
  private def reverseTypeMatchingS[T, U, E](env: E)(implicit envSt: TypeInferenceEnvironmentState[E, T, U]) = {
    val (env2, oldTypeMatching) = envSt.currentTypeMatchingFromEnvironmentS(env)
    val newTypeMatching = oldTypeMatching match {
      case TypeMatching.Types             => TypeMatching.Types
      case TypeMatching.SupertypeWithType => TypeMatching.TypeWithSupertype
      case TypeMatching.TypeWithSupertype => TypeMatching.SupertypeWithType
    }
    envSt.setCurrentTypeMatchingS(newTypeMatching)(env2)
  }
  
  private def appForGlobalTypeWithAllocatedTypeParamsWithoutInstantiatonS[T, U, E](funLoc: T, argLambdas: Seq[TypeValueLambda[T]])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    st(for {
      unallocatedParamAppIdx <- rsteS(envSt.nextTypeParamAppIdxFromEnvironmentS)
      paramCount <- rsteS(envSt.nextTypeParamFromEnvironmentS)
      retTerm <- steS(envSt.appForGlobalTypeS(funLoc, argLambdas, paramCount, unallocatedParamAppIdx))
      allocatedParams <- rsteS(envSt.allocatedTypeParamsFromEnvironmentS)
      retTerm3 <- ste(allocateTypeValueTermParams(retTerm)(allocatedParams.map { p => p -> p }.toMap, unallocatedParamAppIdx)).flatMap {
        case (_, allocatedArgParams, _, retTerm2) =>
          if(!allocatedArgParams.isEmpty)
            steS(envSt.inferTypeValueTermKindS(retTerm)(_: E).mapElements(identity, _.map { _ => retTerm2 }))
          else
            rsteS((_: E, retTerm2))
      }
    } yield retTerm3).run(env)
    
  private def appForGlobalTypeWithAllocatedTypeParamsWithoutInstantiaton[T, U, E](funLoc: T, argLambdas: Seq[TypeValueLambda[T]])(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    State(appForGlobalTypeWithAllocatedTypeParamsWithoutInstantiatonS[T, U, E](funLoc, argLambdas))
  
  def appForGlobalTypeWithAllocatedTypeParamsS[T, U, E](funLoc: T, argLambdas: Seq[TypeValueLambda[T]])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    st(for {
      argLambdas2 <- steS(stMapToVectorValidationS(argLambdas) {
        (argLambda, newEnv: E) =>
          val (newEnv2, newRes) = instantiateS(argLambda.body)(newEnv)
          (newEnv2, newRes.map { b => TypeValueLambda[T](argLambda.argParams, b) })
      })
      retTerm <- appForGlobalTypeWithAllocatedTypeParamsWithoutInstantiaton(funLoc, argLambdas2)
    } yield retTerm).run(env)
    
  private def appForGlobalTypeWithOnlyAllocatedTypeParamsS[T, U, E](funLoc: T, argCount: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) = {
    val (env2, res) = stMapToVectorValidationS(0 until argCount) {
      (_, newEnv: E) =>
        val (newEnv2, newRes) = unifier.allocateParamS(newEnv)
        newRes.map {
          param =>
            val (newEnv3, newRes2) = envSt.allocateTypeParamAppIdxS(newEnv2)
            (newEnv3, newRes2.map { (param, _) })
        }.valueOr { nt => (newEnv2, nt.failure) }
    } (env)
    res.map {
      pairs =>
        val (env3, res2) = appForGlobalTypeWithAllocatedTypeParamsWithoutInstantiatonS(funLoc, pairs.map { case (p, i) => TypeValueLambda[T](Seq(), TypeParamApp(p, Seq(), i)) })(env2)
        (env3, res2.map { (_, pairs.map { _._1 }) })
    }.valueOr { nt => (env2, nt.failure) }
  }
  
  private def checkTypeMatchingConditionS[T, U, V, E](cond: TypeMatchingCondition[T], globalTypeApp1: GlobalTypeApp[T], globalTypeApp2: GlobalTypeApp[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]) =
    (cond, globalTypeApp1, globalTypeApp2) match {
      case (TypeMatchingCondition(firstArgIdxs, secondArgIdxs, matchings, otherParams, lambdaParams), GlobalTypeApp(loc1, lambdas1, sym1), GlobalTypeApp(loc2, lambdas2, sym2)) =>
        if(firstArgIdxs.size === lambdas1.size && secondArgIdxs.size === lambdas2.size) {
          (for {
            res <- State(stMapToIntMapValidationS(otherParams) {
              (param, newEnv: E) => unifier.allocateParamS(newEnv).mapElements(identity, _.map { param -> _ })
            })
            res2 <- State(stMapToIntMapValidationS(lambdaParams) {
              (param, newEnv: E) => unifier.allocateParamS(newEnv).mapElements(identity, _.map { param -> _ })
            })
            res4 <- (res |@| res2) {
              (otherParams2, lambdaParams2) =>
                for {
                  nextParam <- State(envSt.nextTypeParamFromEnvironmentS(_: E))
                  res3 <- {
                    val termParams2 = otherParams2 ++ (firstArgIdxs.keys ++ secondArgIdxs.keys).zipWithIndex.map { case (p1, p2) => (p1, p2 + nextParam) }
                    State(stFoldLeftValidationS(matchings)((z, IntMap[Kind]()).success[NoType[T]]) {
                      (pair, matching, newEnv: E) =>
                        val (x, kinds) = pair
                        val TypeValueTermMatching(params, term, kind) = matching
                        val (newEnv5, newRes) = stFoldLeftValidationS(params)((x, kinds, none[(TypeValueLambda[T], Int)], none[Kind]).success[NoType[T]]) {
                          (tuple, param, newEnv2: E) =>
                            val (x2, kinds2, optPair1, optRetKind) = tuple
                            val optPair2 = firstArgIdxs.get(param).flatMap { 
                              i => lambdas1.lift(i).map { (_, i) }
                            }.orElse(secondArgIdxs.get(param).flatMap { 
                              i => lambdas2.lift(i).map { (_, i + lambdas1.size) } 
                            })
                            (optPair1, optPair2) match {
                              case (Some((lambda1, argIdx1)), Some((lambda2, argIdx2))) =>
                                matchesTypeValueLambdasS(lambda1, lambda2)(x2)(f)(newEnv2) match {
                                  case (newEnv3, Success(y))      =>
                                    val (newEnv4, retKind) = envSt.returnKindFromEnvironmentS(newEnv3)
                                    (newEnv4, (y, kinds2 + (argIdx1 -> retKind) + (argIdx2 -> retKind), some((lambda2, argIdx2)), some(retKind)).success)
                                  case (newEnv3, Failure(noType)) =>
                                    (newEnv3, noType.failure)
                                }
                              case (Some(pair1), None)                                  =>
                                (newEnv2, (x2, kinds2, some(pair1), optRetKind).success)
                              case (None, Some(pair2))                                  =>
                                (newEnv2, (x2, kinds2, some(pair2), optRetKind).success)
                              case (None, None)                                         =>
                                (newEnv2, (x2, kinds2, none, optRetKind).success)                    
                            }
                        } (newEnv)
                        newRes.map {
                          case (x3, kinds3, optPair, optRetKind) =>
                            optRetKind.map { k => (newEnv5, (k, kinds3)) }.orElse {
                              optPair.map {
                                case (lambda, argIdx) =>
                                  val (newEnv6, newRes2) = envSt.inferTypeValueLambdaKindS(lambda)(newEnv5)
                                  (newEnv6, (newRes2.valueOr { _.toNoKind }, kinds3 + (argIdx -> newRes2.valueOr { _.toNoKind })))
                              }
                            }.map {
                              case (newEnv7, (retKind, kinds4)) =>
                                envSt.unifyKindsS(kind, retKind)(newEnv7) match {
                                  case (newEnv8, Success(_))      =>
                                    (term |@| optPair) {
                                      case (term2, (lambda, _)) =>
                                        val term3 = normalizeTypeParamsForTermParamsAndLambdaParams(term2, nextParam + lambdas1.size + lambdas2.size)(termParams2, lambdaParams2)
                                        val lambdas = (lambdas1 ++ lambdas2).zipWithIndex.map { case (l, p) => (p + nextParam, l) }.toMap
                                        val (newEnv9, lambdaMapRes) = prepareTypeValueLambdasForSubstitutionS(lambdas, term3, nextParam)(newEnv8)
                                        lambdaMapRes.map {
                                          lambdas2 =>
                                            substituteTypeValueLambdas(term3, lambdas, nextParam + lambdas1.size + lambdas2.size).map {
                                              term4 =>
                                                val (newEnv10, newRes2) = envSt.inferTypeValueTermKindS(term4)(newEnv9)
                                                newRes2.map {
                                                  _ =>
                                                    matchesTypeValueLambdasS(lambda, TypeValueLambda(Seq(), term4))(x3)(f)(newEnv10) match {
                                                      case (newEnv11, Success(y))      => (newEnv11, (y, kinds4).success)
                                                      case (newEnv11, Failure(noType)) => (newEnv11, noType.failure)
                                                    }
                                                }.valueOr { nt => (newEnv10, nt.failure) }
                                            }.getOrElse(unifier.mismatchedTermErrorS(newEnv8).mapElements(identity, _.failure))
                                        }.valueOr { nt => (newEnv9, nt.failure) }
                                    }.getOrElse((newEnv8, (x3, kinds4).success))
                                  case (newEnv8, Failure(noType)) =>
                                    (newEnv8, noType.failure)
                                }
                            }.getOrElse((newEnv5, (x3, kinds3).success))
                        }.valueOr { nt => (newEnv5, nt.failure) }
                    } (_: E)).flatMap {
                      _.map {
                        case (y, kinds) =>
                          val optKinds2 = mapToVectorOption(0 until (lambdas1.size + lambdas2.size))(kinds.lift)
                          optKinds2.map {
                            kinds2 =>
                              val (kinds21, kinds22) = kinds2.splitAt(lambdas1.size)
                              for {
                                funKindRes1 <- State(envSt.inferTypeValueTermKindS(GlobalTypeApp(loc1, Seq(), sym1))(_: E))
                                funKindRes2 <- State(envSt.inferTypeValueTermKindS(GlobalTypeApp(loc2, Seq(), sym2))(_: E))
                                retKindRes1 <- State(envSt.appKindS(funKindRes1.valueOr { _.toNoKind }, kinds21)(_: E))
                                retKindRes2 <- State(envSt.appKindS(funKindRes2.valueOr { _.toNoKind }, kinds22)(_: E))
                                unifiedKindRes <- State(envSt.unifyKindsS(retKindRes1.valueOr { _.toNoKind }, retKindRes2.valueOr { _.toNoKind })(_: E))
                                y2 <- unifiedKindRes.map {
                                  unifiedKind => State({ (env2: E) => envSt.setReturnKindS(unifiedKind)(env2).mapElements(identity, _ => y.success) })
                                }.valueOr { nt => State((_: E, nt.failure)) }
                              } yield y2
                          }.getOrElse(State((_: E, NoType.fromError[T](FatalError("index out of bounds", none, NoPosition)).failure)))
                      }.valueOr { nt => State((_: E, nt.failure)) }
                    }
                  }
                } yield res3
            }.valueOr { nt => State((_: E, nt.failure)) }
          } yield some(res4)).run(env)
        } else
          (env, none)
    }
  
  private def unifyGlobalTypeAppsWithTypeParamsS[T, U, V, E](globalTypeApp1: GlobalTypeApp[T], globalTypeApp2: GlobalTypeApp[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T]) =
    (globalTypeApp1, globalTypeApp2) match {
      case (GlobalTypeApp(loc1, args1, sym1), GlobalTypeApp(loc2, args2, sym2)) =>
        (for {
          funKindRes1 <- State(envSt.inferTypeValueTermKindS(GlobalTypeApp(loc1, Nil, sym1))(_: E))
          funKindRes2 <- State(envSt.inferTypeValueTermKindS(GlobalTypeApp(loc2, Nil, sym2))(_: E))
          funKindArgCountRes1 <- State(envSt.argCountFromKindS(funKindRes1.valueOr { _.toNoKind })(_: E))
          funKindArgCountRes2 <- State(envSt.argCountFromKindS(funKindRes2.valueOr { _.toNoKind })(_: E))
          optRes2 <- State({
            (env2: E) =>
              (funKindArgCountRes1 |@| funKindArgCountRes2) {
                case (funKindArgCount1, funKindArgCount2) =>
                  if(funKindArgCount1 === args1.size && funKindArgCount2 === args2.size) {
                    envSt.withEmptyTypeParamForestS {
                      env3 =>
                        envSt.withRecursionCheckingS(Set(loc1, loc2)) { 
                          env4 =>
                            appForGlobalTypeWithOnlyAllocatedTypeParamsS(loc1, funKindArgCount1)(env4) match {
                              case (env5, Success((evaluatedTerm1, argParams1))) =>
                                appForGlobalTypeWithOnlyAllocatedTypeParamsS(loc2, funKindArgCount2)(env5) match {
                                  case (env6, Success((evaluatedTerm2, argParams2))) =>
                                    val (env7, res) = unifyS(evaluatedTerm1, evaluatedTerm2)(env6)
                                    res.map {
                                      _ => envSt.findTypeMatchingCondiationS(argParams1, argParams2)(env7)
                                    }.valueOr { nt => (env7, nt.failure) }
                                  case (env6, Failure(noType)) =>
                                    (env6, noType.failure)
                                }
                              case (env5, Failure(noType)) =>
                                (env5, noType.failure)
                            }
                        } (env3).mapElements(identity, some)
                     } (env2)
                  } else
                    (env2, none)
              }.valueOr { nt => (env2, some(nt.failure)) }
          })
        } yield optRes2).run(env)
    }
  
  private def matchesGlobalTypeAppsForTypeMatchingConditionS[T, U, V, E](globalTypeApp1: GlobalTypeApp[T], globalTypeApp2: GlobalTypeApp[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]): (E, Option[Validation[NoType[T], U]]) =
    (globalTypeApp1, globalTypeApp2) match {
      case (GlobalTypeApp(loc1, args1, _), GlobalTypeApp(loc2, args2, _)) =>
        val (env2, typeMatching) = envSt.currentTypeMatchingFromEnvironmentS(env)
        typeMatching match {
          case TypeMatching.TypeWithSupertype =>
            val (env3, _) = reverseTypeMatchingS(env2)
            matchesGlobalTypeAppsForTypeMatchingConditionS(globalTypeApp2, globalTypeApp1)(z)(f)(env3)
          case _ =>
            val globalTypeMatching = typeMatching match {
              case TypeMatching.Types => GlobalTypeMatching.Types
              case _                  => GlobalTypeMatching.SupertypeWithType
            }
            val (env3, optCond) = envSt.getTypeMatchingConditionFromEnvironmentS(globalTypeMatching, loc1, loc2)(env2)
            val (env4, optCondRes) = optCond.map { c => (env3, some(c.success)) }.getOrElse {
              unifyGlobalTypeAppsWithTypeParamsS(globalTypeApp1, globalTypeApp2)(env3)
            }
            optCondRes.map {
              case Success(cond)   =>
                val (env5, _) = envSt.addTypeMatchingConditionS(globalTypeMatching, loc1, loc2, cond)(env4)
                checkTypeMatchingConditionS(cond, globalTypeApp1, globalTypeApp2)(z)(f)(env5)
              case Failure(noType) =>
                (env4, some(noType.failure))
            }.getOrElse((env4, none))
        }
    }
  
  private def matchesGlobalTypesS[T, U, V, E](globalType1: GlobalType[T], globalType2: GlobalType[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]): (E, Validation[NoType[T], U]) =
    if(globalType1.loc === globalType2.loc && globalType1.args.size === globalType2.args.size) {
      val (env2, funKindRes) = envSt.inferTypeValueTermKindS(GlobalTypeApp(globalType1.loc, Nil, globalType1.sym))(env)
      matchesTypeValueLambdaListsWithReturnKindS(globalType1.args, globalType2.args, funKindRes.valueOr { _.toNoKind })(z)(f)(env2)
    } else 
      unifier.mismatchedTermErrorS(env).mapElements(identity, _.failure)
    
  private def matchesGlobalTypeAppWithTypeValueTermS[T, U, V, E](globalTypeApp1: GlobalTypeApp[T], term2: TypeValueTerm[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]): (E, Validation[NoType[T], U]) =
    (globalTypeApp1, term2) match {
      case (GlobalTypeApp(loc1, args1, sym1), globalTypeApp2 @ GlobalTypeApp(loc2, args2, _)) =>
        val (env3, optRes) = if(loc1 === loc2) {
          val (env2, funKindRes) = envSt.inferTypeValueTermKindS(GlobalTypeApp(loc1, Nil, sym1))(env)
          unifier.withSaveS {
            matchesTypeValueLambdaListsWithReturnKindS(args1, args2, funKindRes.valueOr { _.toNoKind })(z)(f)(_)
          } (env2).mapElements(identity, _.map { x => some(x.success) }.getOrElse(none))
        } else
          matchesGlobalTypeAppsForTypeMatchingConditionS(globalTypeApp1, globalTypeApp2)(z)(f)(env)
        optRes match {
          case Some(res) =>
            (env3, res)
          case None      =>
            envSt.withRecursionCheckingS(Set(loc1, loc2)) { 
              env4 =>
                appForGlobalTypeWithAllocatedTypeParamsS(loc1, args1)(env4) match {
                  case (env5, Success(evaluatedTerm1)) =>
                    appForGlobalTypeWithAllocatedTypeParamsS(loc2, args2)(env5) match {
                      case (env6, Success(evaluatedTerm2)) =>
                        matchesTypeValueTermsS(evaluatedTerm1, evaluatedTerm2)(z)(f)(env6)
                      case (env6, Failure(noType))         =>
                        (env6, noType.failure)
                    }
                  case (env5, Failure(noType))         =>
                    (env5, noType.failure)
                }
            } (env3)
        }
      case (GlobalTypeApp(loc1, args1, _), _) =>
        envSt.withRecursionCheckingS(Set(loc1)) {
          env2 =>
            appForGlobalTypeWithAllocatedTypeParamsS(loc1, args1)(env2) match {
              case (env3, Success(evaluatedTerm1)) =>
                matchesTypeValueTermsS(evaluatedTerm1, term2)(z)(f)(env3)
              case (env3, Failure(noType))         =>
                (env3, noType.failure)
            }
        } (env)
      case (_, _) =>
        unifier.mismatchedTermErrorS(env).mapElements(identity, _.failure)
    }
  
  private def matchesTypeParamAppWithTypeValueTermS[T, U, V, E](typeParamApp1: TypeParamApp[T], term2: TypeValueTerm[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]): (E, Validation[NoType[T], U]) =
    (typeParamApp1, term2) match {
      case (TypeParamApp(param1, Seq(), paramAppIdx1), TypeParamApp(param2, Seq(), paramAppIdx2)) =>
        val (env2, res) = f(param1, Left(param2), z, env)
        addDelayedErrorsFromResultS(res, Set(paramAppIdx1, paramAppIdx2))(z)(env2)
      case (TypeParamApp(param1, args1, paramAppIdx1), TypeParamApp(param2, args2, paramAppIdx2)) =>
        val (param, paramOrTerm, tmpArgs1, tmpArgs2, isReversedTypeMatching) = if(args1.size === args2.size)
          (param1, Left(param2), args1, args2, false)
        else if(args1.size < args2.size)
          (param1, Right(TypeParamApp(param2, args2.take(args2.size - args1.size), paramAppIdx2)), args1, args2.drop(args2.size - args1.size), false)
        else
          (param2, Right(TypeParamApp(param1, args1.take(args1.size - args2.size), paramAppIdx1)),  args2, args1.drop(args1.size - args2.size), true)
        val (env6, res) = unifier.withSaveS {
          env2 =>
            f(param, paramOrTerm, z, env2) match {
              case (env3, Success(x))      =>
                val (env4, funKind) = envSt.returnKindFromEnvironmentS(env3)
                val (env5, _) = if(isReversedTypeMatching) reverseTypeMatchingS(env4) else (env4, ())
                matchesTypeValueLambdaListsWithReturnKindS(tmpArgs1, tmpArgs2, funKind)(x)(f)(env5)
              case (env3, Failure(noType)) =>
                (setReturnKindFromTypeValueTermsS(typeParamApp1, term2)(env3)._1, noType.failure)
            }
        } (env)
        addDelayedErrorsFromResultS(res, Set(paramAppIdx1, paramAppIdx2))(z)(env6)
      case (TypeParamApp(param1, Seq(), paramAppIdx1), _: LogicalTypeValueTerm[T]) =>
        term2.normalizedTypeValueTerm match {
          case Some(normalizedTerm2: LogicalTypeValueTerm[T]) =>
            val (env2, res2) = unifier.withSaveS { f(param1, Right(normalizedTerm2), z, _) } (env)
            val (env3, res3) = res2 match {
              case Success(x) =>
                (env2, x.success)
              case Failure(_) =>
                logicalTypeValueTermFromTypeValueTermS(typeParamApp1)(env2) match {
                  case (env4, Success(logicalTerm1)) => matchesLocigalTypeValueTermsS(logicalTerm1, normalizedTerm2)(z)(f)(env4)
                  case (env4, Failure(noType))       => (env4, noType.failure)
                }
            }
            addDelayedErrorsFromResultS(res3, Set(paramAppIdx1))(z)(env3)
          case Some(normalizedTerm2) =>
            val (env2, res) = f(param1, Right(normalizedTerm2), z, env)
            addDelayedErrorsFromResultS(res, Set(paramAppIdx1))(z)(env2)
          case None =>
            (env, NoType.fromError(FatalError("can't normalize type value term", none, NoPosition)).failure)
        }
      case (TypeParamApp(param1, Seq(), paramAppIdx1), _) =>
        val (env2, res) = f(param1, Right(term2), z, env)
        addDelayedErrorsFromResultS(res, Set(paramAppIdx1))(z)(env2)
      case (TypeParamApp(param1, args1, paramAppIdx1), GlobalTypeApp(loc2, args2, sym2)) if args1.size <= args2.size =>
        val (env5, res) = unifier.withSaveS {
          env2 =>
            f(param1, Right(GlobalTypeApp(loc2, args2.take(args2.size - args1.size), sym2)), z, env2) match {
              case (env3, Success(x))      =>
                val (env4, funKind) = envSt.returnKindFromEnvironmentS(env3)
                matchesTypeValueLambdaListsWithReturnKindS(args1, args2.drop(args2.size - args1.size), funKind)(x)(f)(env4)
              case (env3, Failure(noType)) =>
                (setReturnKindFromTypeValueTermsS(typeParamApp1, term2)(env3)._1, noType.failure)
            }
        } (env)
        addDelayedErrorsFromResultS(res, Set(paramAppIdx1))(z)(env5)
      case (_, globalTypeApp2: GlobalTypeApp[T]) =>
        val (env2, _) = reverseTypeMatchingS(env)
        matchesGlobalTypeAppWithTypeValueTermS(globalTypeApp2, typeParamApp1)(z)(f)(env2)
      case (TypeParamApp(_, args1, _), logicalTerm2: LogicalTypeValueTerm[T]) if !args1.isEmpty =>
        logicalTypeValueTermFromTypeValueTermS(typeParamApp1)(env) match {
          case (env2, Success(logicalTerm1)) => 
            matchesLocigalTypeValueTermsS(logicalTerm1, logicalTerm2)(z)(f)(env2)
          case (env2, Failure(noType))       => 
            (env2, noType.failure)
        }
      case (TypeParamApp(param1, args1, paramAppIdx1), _) =>
        val (env2, noType) = mismatchedTypeValueTermNoTypeWithReturnKindS(typeParamApp1, term2)(env)
        addDelayedErrorsFromResultS(noType.failure, Set(paramAppIdx1))(z)(env2)
      case (_, _) =>
        unifier.mismatchedTermErrorS(env).mapElements(identity, _.failure)
    }  
  
  private def matchesTypeValueTermsForLogicalTypeValueTermsS[T, U, V, E](term1: TypeValueTerm[T], term2: TypeValueTerm[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]): (E, Validation[NoType[T], U]) = {
    val (env2, typeMatching) = envSt.currentTypeMatchingFromEnvironmentS(env)
    val (env7, res) = partiallyInstantiateTypeValueTermS(term1)(unifier.mismatchedTermErrorS)(env) match {
      case (env3, Success((instantiatedTerm1, optInstantiatedParam1))) =>
        partiallyInstantiateTypeValueTermS(term2)(unifier.mismatchedTermErrorS)(env3) match {
          case (env4, Success((instantiatedTerm2, optInstantiatedParam2))) =>
            envSt.withInfinityCheckingS(optInstantiatedParam1.toSet ++ optInstantiatedParam2) {
              env5 =>
                (instantiatedTerm1, instantiatedTerm2) match {
                  case (globalTypeApp1: GlobalTypeApp[T], _: LogicalTypeValueTerm[T]) =>
                    matchesGlobalTypeAppWithTypeValueTermS(globalTypeApp1, instantiatedTerm2)(z)(f)(env5)
                  case (_: LogicalTypeValueTerm[T], globalTypeApp2: GlobalTypeApp[T]) =>
                    val (env6, _) = reverseTypeMatchingS(env5)
                    matchesGlobalTypeAppWithTypeValueTermS(globalTypeApp2, instantiatedTerm1)(z)(f)(env6)
                  case _ =>
                    matchesTypeValueTermsWithoutPartiallyInstantiationS(instantiatedTerm1, instantiatedTerm2, typeMatching)(z)(f)(env5)
                }
            } (env4)
          case (env4, Failure(noType)) =>
            (env4, noType.failure)
        }
      case (env3, Failure(noType)) =>
        (env3, noType.failure)
    }
    envSt.setCurrentTypeMatchingS(typeMatching)(env7).mapElements(identity, _ => res)
  }
    
  private def matchesBuiltinTypeWithTypeValueTermS[T, U, V, E](term: TypeValueTerm[T])(z: U)(env: E)(implicit envSt: TypeInferenceEnvironmentState[E, V, T]) =
    (for {
      retKindRes <- State(envSt.inferTypeValueTermKindS(term))
      unifiedRetKindRes <- State(envSt.unifyKindsS(InferredKind(Star(KindType, NoPosition)), retKindRes.valueOr { _.toNoKind }))
      _ <- State(envSt.setReturnKindS(unifiedRetKindRes.valueOr { _.toNoKind }))
    } yield (unifiedRetKindRes.map { _ => z })).run(env)
  
  @tailrec
  private def matchesFunctionTypesWithoutTypeMatchingS[T, U, V, E](term1: TypeValueTerm[T], term2: TypeValueTerm[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]): (E, Validation[NoType[T], U]) =
    (term1, term2) match {
      case (BuiltinType(TypeBuiltinFunction.Fun, Seq(argTerm1, retTerm1)), BuiltinType(TypeBuiltinFunction.Fun, Seq(argTerm2, retTerm2))) =>
        matchesTypeValueTermsS(argTerm1, argTerm2)(z)(f)(env) match {
          case (env2, Success(x))      =>
            val (env3, argKind) = envSt.returnKindFromEnvironmentS(env2)
            envSt.appStarKindS(Seq(argKind))(env3) match {
              case (env4, Success(_))      => matchesFunctionTypesWithoutTypeMatchingS(retTerm1, retTerm2)(x)(f)(env4)
              case (env4, Failure(noType)) => (env4, noType.failure)
            }
          case (env2, Failure(noType)) =>
            (env2, noType.failure)
        }
      case (_, _) =>
        val (env2, res) = matchesTypeValueTermsS(term1, term2)(z)(f)(env)
        res.flatMap {
          x =>
            val (env3, kind) = envSt.returnKindFromEnvironmentS(env2)
            val (env4, res2) = envSt.appStarKindS(Seq(kind))(env3)
            res2.map { envSt.setReturnKindS(_)(env4).mapElements(identity, _ => x.success) }
        }.valueOr { nt => (env2, nt.failure) }
    }

  private def matchesFunctionTypesS[T, U, V, E](term1: TypeValueTerm[T], term2: TypeValueTerm[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]): (E, Validation[NoType[T], U]) = {
    val (env2, savedTypeMatching) = envSt.currentTypeMatchingFromEnvironmentS(env)
    val (env3, _) = envSt.setCurrentTypeMatchingS(TypeMatching.Types)(env2)
    val (env4, res) = matchesFunctionTypesWithoutTypeMatchingS(term1, term2)(z)(f)(env3)
    envSt.setCurrentTypeMatchingS(savedTypeMatching)(env4).mapElements(identity, _ => res)
  }
  
  private def matchesTypeValueTermsWithoutPartiallyInstantiationS[T, U, V, E](term1: TypeValueTerm[T], term2: TypeValueTerm[T], typeMatching: TypeMatching.Value)(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]) =
    (term1, term2) match {
      case (TupleType(args1), TupleType(args2)) if args1.size === args2.size =>
        matchesTypeValueTermListsWithReturnKindS(args1, args2)(z)(f)(env)
      case (FieldType(i1, term3), FieldType(i2, term4)) if i1 === i2 =>
        matchesTypeValueTermListsWithReturnKindS(Seq(term3), Seq(term4))(z)(f)(env)
      case (BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _))) =>
        matchesFunctionTypesS(term1, term2)(z)(f)(env)
      case (BuiltinType(bf1, args1), BuiltinType(bf2, args2)) if bf1 === bf2 && args1.size === args2.size =>
        matchesTypeValueTermListsWithReturnKindS(args1, args2)(z)(f)(env)
      case (globalType1: GlobalType[T], globalType2: GlobalType[T]) =>
        matchesGlobalTypesS(globalType1, globalType2)(z)(f)(env)
      case (typeParamApp1: TypeParamApp[T], _) =>
        matchesTypeParamAppWithTypeValueTermS(typeParamApp1, term2)(z)(f)(env)
      case (_, typeParamApp2: TypeParamApp[T]) =>
        val (env2, _) = reverseTypeMatchingS(env)
        matchesTypeParamAppWithTypeValueTermS(typeParamApp2, term1)(z)(f)(env2)
      case (BuiltinType(TypeBuiltinFunction.Any, Seq()), _) if typeMatching === TypeMatching.SupertypeWithType =>
        matchesBuiltinTypeWithTypeValueTermS(term2)(z)(env)
      case (_, BuiltinType(TypeBuiltinFunction.Nothing, Seq())) if typeMatching === TypeMatching.SupertypeWithType =>
        matchesBuiltinTypeWithTypeValueTermS(term1)(z)(env)
      case (_, BuiltinType(TypeBuiltinFunction.Any, Seq())) if typeMatching === TypeMatching.TypeWithSupertype =>
        matchesBuiltinTypeWithTypeValueTermS(term1)(z)(env)
      case (BuiltinType(TypeBuiltinFunction.Nothing, Seq()), _) if typeMatching === TypeMatching.TypeWithSupertype =>
        matchesBuiltinTypeWithTypeValueTermS(term2)(z)(env)
      case (globalTypeApp1: GlobalTypeApp[T], logicalTerm2: LogicalTypeValueTerm[T]) =>
        logicalTypeValueTermFromTypeValueTermS(globalTypeApp1)(env) match {
          case (env2, Success(logicalTerm1)) => matchesLocigalTypeValueTermsS(logicalTerm1, logicalTerm2)(z)(f)(env2)
          case (env2, Failure(noType))       => (env2, noType.failure)
        }
      case (logicalTerm1: LogicalTypeValueTerm[T], globalTypeApp2: GlobalTypeApp[T]) =>
        logicalTypeValueTermFromTypeValueTermS(globalTypeApp2)(env) match {
          case (env2, Success(logicalTerm2)) => matchesLocigalTypeValueTermsS(logicalTerm1, logicalTerm2)(z)(f)(env2)
          case (env2, Failure(noType))       => (env2, noType.failure)
        }
      case (globalTypeApp1: GlobalTypeApp[T], _) =>
        matchesGlobalTypeAppWithTypeValueTermS(globalTypeApp1, term2)(z)(f)(env)
      case (_, globalTypeApp2: GlobalTypeApp[T]) =>
        val (env2, _) = reverseTypeMatchingS(env)
        matchesGlobalTypeAppWithTypeValueTermS(globalTypeApp2, term1)(z)(f)(env2)
      case (logicalTerm1: LogicalTypeValueTerm[T], logicalTerm2: LogicalTypeValueTerm[T]) =>
        matchesLocigalTypeValueTermsS(logicalTerm1, logicalTerm2)(z)(f)(env)
      case (logicalTerm1: LogicalTypeValueTerm[T], _) =>
        logicalTypeValueTermFromTypeValueTermS(term2)(env) match {
          case (env2, Success(logicalTerm2)) => matchesLocigalTypeValueTermsS(logicalTerm1, logicalTerm2)(z)(f)(env2)
          case (env2, Failure(noType))       => (env2, noType.failure)
        }
      case (_, logicalTerm2: LogicalTypeValueTerm[T]) =>
        logicalTypeValueTermFromTypeValueTermS(term1)(env) match {
          case (env2, Success(logicalTerm1)) => matchesLocigalTypeValueTermsS(logicalTerm1, logicalTerm2)(z)(f)(env2)
          case (env2, Failure(noType))       => (env2, noType.failure)
        }
      case (_, _) =>
        unifier.mismatchedTermErrorS(env).mapElements(identity, _.failure)
    }
  
  def matchesTypeValueTermsS[T, U, V, E](term1: TypeValueTerm[T], term2: TypeValueTerm[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]): (E, Validation[NoType[T], U]) = {
    val (env2, typeMatching) = envSt.currentTypeMatchingFromEnvironmentS(env)
    val (env6, res) = partiallyInstantiateTypeValueTermS(term1)(unifier.mismatchedTermErrorS)(env2) match {
      case (env3, Success((instantiatedTerm1, optInstantiatedParam1))) =>
        partiallyInstantiateTypeValueTermS(term2)(unifier.mismatchedTermErrorS)(env3) match {
          case (env4, Success((instantiatedTerm2, optInstantiatedParam2))) =>
            envSt.withInfinityCheckingS(optInstantiatedParam1.toSet ++ optInstantiatedParam2) {
              env5 =>
                matchesTypeValueTermsWithoutPartiallyInstantiationS(instantiatedTerm1, instantiatedTerm2, typeMatching)(z)(f)(env5)
            } (env4)
          case (env4, Failure(noType)) =>
            (env4, noType.failure)
        }
      case (env3, Failure(noType)) =>
        (env3, noType.failure)
    }
    envSt.setCurrentTypeMatchingS(typeMatching)(env6).mapElements(identity, _ => res)
  }
  
  def replaceTypeParamsFromTypeValueTermsS[T, U, E](terms: Seq[TypeValueTerm[T]])(f: (Int, E) => (E, Validation[NoType[T], Either[Int, TypeValueTerm[T]]]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    stMapToVectorValidationS(terms) { replaceTypeValueTermParamsS(_)(f)(_: E) } (env)
  
  def replaceTypeParamsFromTypeValueLambdasS[T, U, E](lambdas: Seq[TypeValueLambda[T]])(f: (Int, E) => (E, Validation[NoType[T], Either[Int, TypeValueTerm[T]]]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    stMapToVectorValidationS(lambdas) { replaceTypeValueLambdaParamsS(_)(f)(_: E) } (env)
    
  def replaceTypeParamsFromTupleTypesS[T, U, E](tupleTypes: Seq[TupleType[T]])(f: (Int, E) => (E, Validation[NoType[T], Either[Int, TypeValueTerm[T]]]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    stMapToVectorValidationS(tupleTypes) { replaceTupleTypeParamsS(_)(f)(_: E) } (env)
  
  def replaceTypeValueLambdaParamsS[T, U, E](lambda: TypeValueLambda[T])(f: (Int, E) => (E, Validation[NoType[T], Either[Int, TypeValueTerm[T]]]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    lambda match {
      case TypeValueLambda(argParams, body) =>
        val (env2, res) = stMapToVectorValidationS(argParams) { unifier.findRootParamS(_)(_: E) } (env)
        res.map {
          argParams2 => replaceTypeValueTermParamsS(body)(f)(env2).mapElements(identity, _.map { TypeValueLambda(argParams2, _) })
        }.valueOr { nt => (env2, nt.failure) }
    }
  
  def replaceTupleTypeParamsS[T, U, E](tupleType: TupleType[T])(f: (Int, E) => (E, Validation[NoType[T], Either[Int, TypeValueTerm[T]]]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    tupleType match {
      case TupleType(args) =>
        replaceTypeParamsFromTypeValueTermsS(args)(f)(env).mapElements(identity, _.map { TupleType(_) })
    }
    
  def replaceTypeValueTermParamsS[T, U, E](term: TypeValueTerm[T])(f: (Int, E) => (E, Validation[NoType[T], Either[Int, TypeValueTerm[T]]]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]): (E, Validation[NoType[T], TypeValueTerm[T]]) =
    term match {
      case tupleType: TupleType[T] =>
        replaceTupleTypeParamsS(tupleType)(f)(env)
      case FieldType(i, term2) =>
        replaceTypeValueTermParamsS(term2)(f)(env).mapElements(identity, _.map { FieldType(i, _) })
      case BuiltinType(bf, args) =>
        replaceTypeParamsFromTypeValueTermsS(args)(f)(env).mapElements(identity, _.map { BuiltinType(bf, _) })
      case Unittype(loc, args, sym) =>
        replaceTypeParamsFromTypeValueLambdasS(args)(f)(env).mapElements(identity, _.map { Unittype(loc, _, sym) })
      case Grouptype(loc, args, sym) =>
        replaceTypeParamsFromTypeValueLambdasS(args)(f)(env).mapElements(identity, _.map { Grouptype(loc, _, sym) })
      case GlobalTypeApp(loc, args, sym) =>
        replaceTypeParamsFromTypeValueLambdasS(args)(f)(env).mapElements(identity, _.map { GlobalTypeApp(loc, _, sym) })
      case TypeParamApp(param, args, paramAppIdx) =>
        val (env2, res) = f(param, env)
        res match {
          case Success(Left(param2))     =>
            val (env3, res2) = replaceTypeParamsFromTypeValueLambdasS(args)(f)(env2)
            (env3, res2.map { TypeParamApp(param2, _, paramAppIdx) })
          case Success(Right(paramTerm)) =>
            paramTerm.normalizedTypeValueTerm.map {
              _ match {
                case TypeParamApp(param2, args2, _)   =>
                  val (env3, res2) = replaceTypeParamsFromTypeValueLambdasS(args2 ++ args)(f)(env2)
                  (env3, res2.map { TypeParamApp(param2, _, paramAppIdx) })
                case GlobalTypeApp(loc2, args2, sym2) =>
                  val (env3, res2) = replaceTypeParamsFromTypeValueLambdasS(args2 ++ args)(f)(env2)
                  (env3, res2.map { GlobalTypeApp(loc2, _, sym2) })
                case _                                =>
                  if(args.isEmpty)
                    (env2, paramTerm.success)
                  else
                    (env2, NoType.fromError[T](FatalError("type value term isn't type application", none, NoPosition)).failure)
              }
            }.getOrElse((env2, NoType.fromError[T](FatalError("can't normalized type value term", none, NoPosition)).failure))
          case Failure(noType)           =>
            (env2, noType.failure)
        }
      case logicalTerm: LogicalTypeValueTerm[T] =>
        replaceLogicalTypeValueTermParamsS(logicalTerm)(f)(env)
    }
  
  def unsafeAllocateTypeParamsFromTypeValueTermsS[T, U, E](terms: Iterable[TypeValueTerm[T]])(allocatedParams: Map[Int, Int], unallocatedParamAppIdx: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    stFoldLeftValidationS(terms)((allocatedParams, Set[Int](), Set[Int](), Vector[TypeValueTerm[T]]()).success[NoType[T]]) {
      (tuple, term, newEnv: E) =>
        val (newAllocatedParams, newAllocatedArgParams, allocatedParamAppIdxs, newTerms) = tuple
        val (newEnv2, newRes) = unsafeAllocateTypeValueTermParamsS(term)(newAllocatedParams, unallocatedParamAppIdx)(newEnv)
        (newEnv2, newRes.map { 
          _.mapElements(identity, newAllocatedArgParams | _, allocatedParamAppIdxs | _, newTerms :+ _)
        })
    } (env)
    
  def unsafeAllocateTypeParamsFromTypeValueLambdasS[T, U, E](lambdas: Iterable[TypeValueLambda[T]])(allocatedParams: Map[Int, Int], unallocatedParamAppIdx: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    stFoldLeftValidationS(lambdas)((allocatedParams, Set[Int](), Set[Int](), Vector[TypeValueLambda[T]]()).success[NoType[T]]) {
      (tuple, lambda, newEnv: E) =>
        val (newAllocatedParams, newAllocatedArgParams, allocatedParamAppIdxs, newTerms) = tuple
        val (newEnv2, newRes) = unsafeAllocateTypeValueLambdaParamsS(lambda)(newAllocatedParams, unallocatedParamAppIdx)(newEnv)
        (newEnv2, newRes.map {
          _.mapElements(identity, newAllocatedArgParams | _, allocatedParamAppIdxs | _, newTerms :+ _)
        })
    } (env)

  def unsafeAllocateTypeParamsFromTupleTypesS[T, U, E](tupleTypes: Iterable[TupleType[T]])(allocatedParams: Map[Int, Int], unallocatedParamAppIdx: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    stFoldLeftValidationS(tupleTypes)((allocatedParams, Set[Int](), Set[Int](), Vector[TupleType[T]]()).success[NoType[T]]) {
      (tuple, tupleType, newEnv: E) =>
        val (newAllocatedParams, newAllocatedArgParams, allocatedParamAppIdxs, newTerms) = tuple
        val (newEnv2, newRes) = unsafeAllocateTupleTypeParamsS(tupleType)(newAllocatedParams, unallocatedParamAppIdx)(newEnv)
        (newEnv2, newRes.map { 
          _.mapElements(identity, newAllocatedArgParams | _, allocatedParamAppIdxs | _, newTerms :+ _)
        })
    } (env)
    
  def unsafeAllocateTypeValueLambdaParamsS[T, U, E](lambda: TypeValueLambda[T])(allocatedParams: Map[Int, Int], unallocatedParamAppIdx: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    lambda match {
      case TypeValueLambda(argParams, body) =>
        val (env2, res) = stFoldLeftValidationS(argParams)((IntMap[Int](), Vector[Int]()).success[NoType[T]]) {
          (pair, argParam, newEnv: E) =>
            val (newAllocatedArgParams, newArgParams) = pair
            allocatedParams.get(argParam).map { argParam2 => (newEnv, (newAllocatedArgParams, newArgParams :+ argParam2).success) }.getOrElse {
              val (newEnv2, res) = unifier.allocateParamS(newEnv)
              res.map { argParam2 => (newEnv2, (newAllocatedArgParams + (argParam -> argParam2), newArgParams :+ argParam2).success) }.valueOr {
                nt => (newEnv2, nt.failure)
              }
            }
        } (env)
        res match {
          case Success((allocatedArgParams, argParams2)) =>
            val (env3, res2) = unsafeAllocateTypeValueTermParamsS(body)(allocatedParams ++ allocatedArgParams, unallocatedParamAppIdx)(env2)
            val argParamsSet2 = argParams2.toSet
            (env3, res2.map { _.mapElements(_.flatMap { case (p, p2) => if(argParamsSet2.contains(p2)) Map() else some(p -> p2) }, _ => argParamsSet2, identity, TypeValueLambda(argParams2, _)) })
          case Failure(noType)                           =>
            (env2, noType.failure)
        }
    }

  def unsafeAllocateTupleTypeParamsS[T, U, E](tupleType: TupleType[T])(allocatedParams: Map[Int, Int], unallocatedParamAppIdx: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    tupleType match {
      case TupleType(args) =>
        val (env2, res) = unsafeAllocateTypeParamsFromTypeValueTermsS(args)(allocatedParams, unallocatedParamAppIdx)(env)
        (env2, res.map { _.mapElements(identity, identity, identity, TupleType(_)) })
    }
    
  def unsafeAllocateTypeValueTermParamsS[T, U, E](term: TypeValueTerm[T])(allocatedParams: Map[Int, Int], unallocatedParamAppIdx: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]): (E, Validation[NoType[T], (Map[Int, Int], Set[Int], Set[Int], TypeValueTerm[T])]) =
    term match {
      case tupleType: TupleType[T] =>
        unsafeAllocateTupleTypeParamsS(tupleType)(allocatedParams, unallocatedParamAppIdx)(env)
      case FieldType(i, term2) =>
        val (env2, res) = unsafeAllocateTypeValueTermParamsS(term2)(allocatedParams, unallocatedParamAppIdx)(env)
        (env2, res.map { _.mapElements(identity, identity, identity, FieldType(i, _)) })
      case BuiltinType(bf, args) =>
        val (env2, res) = unsafeAllocateTypeParamsFromTypeValueTermsS(args)(allocatedParams, unallocatedParamAppIdx)(env)
        (env2, res.map { _.mapElements(identity, identity, identity, BuiltinType(bf, _)) })
      case Unittype(loc, args, sym) =>
        val (env2, res) = unsafeAllocateTypeParamsFromTypeValueLambdasS(args)(allocatedParams, unallocatedParamAppIdx)(env)
        (env2, res.map { _.mapElements(identity, identity, identity, Unittype(loc, _, sym)) })
      case Grouptype(loc, args, sym) =>
        val (env2, res) = unsafeAllocateTypeParamsFromTypeValueLambdasS(args)(allocatedParams, unallocatedParamAppIdx)(env)
        (env2, res.map { _.mapElements(identity, identity, identity, Grouptype(loc, _, sym)) })
      case GlobalTypeApp(loc, args, sym) =>
        val (env2, res) = unsafeAllocateTypeParamsFromTypeValueLambdasS(args)(allocatedParams, unallocatedParamAppIdx)(env)
        (env2, res.map { _.mapElements(identity, identity, identity, GlobalTypeApp(loc, _, sym)) })
      case TypeParamApp(param, args, paramAppIdx) =>
        val (env3, res2) = allocatedParams.get(param).map { param2 => (env, (allocatedParams, param2).success) }.getOrElse {
          val (env2, res) = unifier.allocateParamS(env)
          res.map { param2 => (env2, (allocatedParams + (param -> param2), param2).success) }.valueOr {
            nt => (env2, nt.failure)
          }
        }
        res2 match {
          case Success((allocatedParams2, param2)) =>
            val (env4, res3) = if(paramAppIdx === unallocatedParamAppIdx)
              envSt.allocateTypeParamAppIdxS(env3)
            else
              (env3, paramAppIdx.success)
            res3 match {
              case Success(paramAppIdx2) =>
                val (env5, res4) = unsafeAllocateTypeParamsFromTypeValueLambdasS(args)(allocatedParams2, unallocatedParamAppIdx)(env4)
                (env5, res4.map { _.mapElements(identity, identity, _ + paramAppIdx2, TypeParamApp(param2, _, paramAppIdx2)) })
              case Failure(noType)       =>
                (env4, noType.failure)
            }
          case Failure(noType) =>
            (env3, noType.failure)
        }
      case logicalTerm: LogicalTypeValueTerm[T] =>
        unsafeAllocateTypeParamsFromLogicalTypeValueTermS(logicalTerm)(allocatedParams, unallocatedParamAppIdx)(env)
    }

  def allocateTypeValueTermParamsS[T, U, E](term: TypeValueTerm[T])(allocatedParams: Map[Int, Int], unallocatedParamAppIdx: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    unifier.withSaveS { unsafeAllocateTypeValueTermParamsS(term)(allocatedParams, unallocatedParamAppIdx)(_: E) } (env)
    
  def allocateTypeValueTermParams[T, U, E](term: TypeValueTerm[T])(allocatedParams: Map[Int, Int], unallocatedParamAppIdx: Int)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    State(allocateTypeValueTermParamsS[T, U, E](term)(allocatedParams, unallocatedParamAppIdx))
  
  def allocateTypeValueTermParamsWithKindsS[T, U, E](term: TypeValueTerm[T], kinds: Map[Int, Kind])(allocatedParams: Map[Int, Int], unallocatedParamAppIdx: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    unifier.withSaveS {
      env2 =>
        val mustInferKinds = (kinds.foldLeft(0) {
          case (s, (_, InferredKind(kt))) => s + (if(kindParamsFromKindTerm(kt).isEmpty) 0 else 1)
          case (s, _)                     => s
        } > 1)
        val (env3, res) = unsafeAllocateTypeValueTermParamsS(term)(allocatedParams, unallocatedParamAppIdx)(env2)
        res.map {
          case (allocatedParams, allocatedArgParams, allocatedParamAppIdxs, term2) =>
            st(for {
              inferringKinds <- steS(stFlatMapToIntMapValidationS(kinds) {
                (pair, newEnv: E) =>
                  val (param, kind) = pair
                  val (newEnv2, inferringKindRes) = envSt.inferringKindFromKindS(kind)(newEnv)
                    (newEnv2, inferringKindRes.map { 
                      inferringKind => allocatedParams.get(param).map { _ -> inferringKind }.toList
                  })
              })
              _ <- rsteS(envSt.setTypeParamKindsS(inferringKinds)).flatMap {
                _ =>
                  if(mustInferKinds || !allocatedArgParams.isEmpty)
                    steS(envSt.inferTypeValueTermKindS(term2)).map { _ => () }
                  else
                    rsteS[NoType[T], E, Unit]((_: E, ()))
              }
            } yield (allocatedParams, allocatedArgParams, allocatedParamAppIdxs, term2)).run(env3)
        }.valueOr { nt => (env3, nt.failure) }
    } (env)

  def allocateTypeValueTermParamsWithKinds[T, U, E](term: TypeValueTerm[T], kinds: Map[Int, Kind])(allocatedParams: Map[Int, Int], unallocatedParamAppIdx: Int)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    State(allocateTypeValueTermParamsWithKindsS[T, U, E](term, kinds)(allocatedParams, unallocatedParamAppIdx))
    
  def checkDefinedTypeS[T, U, E](definedType: DefinedType[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) = {
    val params = definedType.args.flatMap { _.param }.toSet
    val (env2, res) = stMapToSetValidationS(params) { unifier.findRootParamS(_)(_: E) } (env)
    val (env3, isInstTypeMatching) = envSt.isInstanceTypeMatchingS(env2)
    val prefix = if(!isInstTypeMatching) "defined" else "instance"
    (env2, res.map {
      rootParams => if(rootParams.size === params.size) ().success else NoType.fromError[T](Error("parameters are distinct at " + prefix + " type " + definedType, none, definedType.pos)).failure
    }.valueOr { _.failure })
  }
  
  def checkDefinedTypesS[T, U, E](definedTypes: Seq[DefinedType[T]])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]): (E, Validation[NoType[T], Unit]) =
    definedTypes.foldLeft((env, ().success[NoType[T]])) {
      case ((newEnv, newRes), definedType) =>
        checkDefinedTypeS(definedType)(newEnv).mapElements(identity, r => (newRes |@| r) { (u, _) => u })
    }
  
  private def checkDefinedTypes2[T, U, E](definedTypes: Seq[DefinedType[T]])(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    State(checkDefinedTypesS[T, U, E](definedTypes))
  
  def checkDefinedTypes[T, U, E](definedTypes: Seq[DefinedType[T]])(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    checkDefinedTypes2[T, U, E](definedTypes)

  def checkTypeParamsFromTypeValueTermsS[T, U, E](terms: Seq[TypeValueTerm[T]])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    stFoldLeftValidationS(terms)(().success[NoType[T]]) {
      (_, t, newEnv: E) => checkTypeParamsFromTypeValueTermS(t)(newEnv)
    } (env)
      
  def checkTypeParamsFromTypeValueLambdasS[T, U, E](lambdas: Seq[TypeValueLambda[T]])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    stFoldLeftValidationS(lambdas)(().success[NoType[T]]) {
      (_, l, newEnv: E) => checkTypeParamsFromTypeValueTermS(l.body)(newEnv)
    } (env)
  
  def checkTypeParamsFromTupleTypesS[T, U, E](terms: Seq[TupleType[T]])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    stFoldLeftValidationS(terms)(().success[NoType[T]]) {
      (_, tupleType, newEnv: E) =>
        partiallyInstantiateTypeValueTermS(tupleType)(unifier.mismatchedTermErrorS)(newEnv) match {
          case (newEnv2, Success((instantiatedTerm, optInstantiatedParam))) =>
            envSt.withInfinityCheckingS(optInstantiatedParam.toSet) {
              newEnv3 =>
                instantiatedTerm match {
                  case TupleType(args) =>
                    checkTypeParamsFromTypeValueTermsS(args)(newEnv3)
                  case _               =>
                    (newEnv3, NoType.fromError[T](FatalError("no tuple type", none, NoPosition)).failure)
                }
            } (newEnv2)
          case (newEnv2, Failure(noType)) =>
            (newEnv2, noType.failure)
        }
    } (env)
    
  def checkTypeParamsFromTypeValueTermS[T, U, E](term: TypeValueTerm[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]): (E, Validation[NoType[T], Unit]) =
    partiallyInstantiateTypeValueTermS(term)(unifier.mismatchedTermErrorS)(env) match {
      case (env2, Success((instantiatedTerm, optInstantiatedParam))) =>
        envSt.withInfinityCheckingS(optInstantiatedParam.toSet) {
          env3 =>
            instantiatedTerm match {
              case TupleType(args) =>
                checkTypeParamsFromTypeValueTermsS(args)(env3)
              case FieldType(_, term2) =>
                checkTypeParamsFromTypeValueTermsS(Seq(term2))(env3)
              case BuiltinType(_, args) =>
                checkTypeParamsFromTypeValueTermsS(args)(env3)
              case globalType: GlobalType[T] =>
                checkTypeParamsFromTypeValueLambdasS(globalType.args)(env3)
              case GlobalTypeApp(_, args, _) =>
                checkTypeParamsFromTypeValueLambdasS(args)(env3)
              case TypeParamApp(param, args, _) =>
                val (env4, isLambdaArgParam) = envSt.isTypeLambdaArgParamS(param)(env3)
                if(!isLambdaArgParam)
                  checkTypeParamsFromTypeValueLambdasS(args)(env4)
                else
                  unifier.mismatchedTermErrorS(env4).mapElements(identity, _.failure)
              case logicalTerm: LogicalTypeValueTerm[T] =>
                checkTypeParamsFromLogicalTypeValueTermS(logicalTerm)(env3)
            }
        } (env2)
      case (env2, Failure(noType)) =>
        (env2, noType.failure)
    }
    
  def normalizeTypeAppS[T, U, E](typeApp: TypeApp[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    st(for {
      kind <- steS(envSt.inferTypeValueTermKindS(typeApp))
      typeApp3 <- steS(envSt.argCountFromKindS(kind)).flatMap {
        argCount =>
          if(argCount > 0) {
            steS(stMapToVectorValidationS(0 until argCount) {
              (_, newEnv: E) =>
                val (newEnv2, newRes) = unifier.allocateParamS(newEnv)
                newRes.map {
                  param =>
                    val (newEnv3, newRes2) = envSt.allocateTypeParamAppIdxS(newEnv2)
                    (newEnv3, newRes2.map { (param, _).success }.valueOr { _.failure })
                 }.valueOr { nt => (newEnv2, nt.failure) }
            }).flatMap {
              ps =>
                val args2 = ps.map { p => TypeValueLambda[T](Nil, TypeParamApp(p._1, Nil, p._2)) }
                val typeApp2 = typeApp.withArgs(typeApp.args ++ args2)
                steS(envSt.inferTypeValueTermKindS(typeApp2)).map { _ => typeApp2 }
            }
          } else
            rsteS((_: E, typeApp))
      }
    } yield typeApp3).run(env)
  
  def normalizeTypeValueTermS[T, U, E](term: TypeValueTerm[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    partiallyInstantiateTypeValueTermS(term) { (_: E, inifityTypeValueTermNoType) } (env) match {
      case (env2, Success((typeApp: TypeApp[T], _))) => normalizeTypeAppS(typeApp)(env2)
      case (env2, Success(_))                   => (env2, term.success)
      case (env2, Failure(noType))              => (env2, noType.failure)
    }
    
  def logicalTypeValueTermFromTypeValueTermS[T, U, E](term: TypeValueTerm[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]): (E, Validation[NoType[T], LogicalTypeValueTerm[T]]) =
    term match {
      case GlobalTypeApp(loc, args, sym) =>
        val (env2, res) = appForGlobalTypeWithAllocatedTypeParamsWithoutInstantiatonS(loc, args)(env)
        (env2, res.map { _.unevaluatedLogicalTypeValueTerm.globalTypeAppForLogicalTypeValueTerm(loc, args, sym) })
      case _                             =>
        (env, term.unevaluatedLogicalTypeValueTerm.success)
    }
  
  def prepareTypeValueLambdasForSubstitutionS[T, U, E](lambdas: Map[Int, TypeValueLambda[T]], term: TypeValueTerm[T], nextParam: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) = {
    val logicalTerms = logicalTypeValueTermsFromTypeValueTerm(term)
    st(for {
      lambdas2 <- steS({
        stMapToIntMapValidationS(lambdas) {
          (pair, newEnv: E) =>
            val (param, lambda) = pair
            if(param < nextParam)
              unifier.findRootParamS(param)(newEnv).mapElements(identity, _.map { _ -> (param -> lambda) })
            else
              (newEnv, (param -> (param -> lambda)).success)
        }
      })
      lambdas3 <- steS({
        stFoldLeftValidationS(logicalTerms)(IntMap[(Int, TypeValueLambda[T])]().success[NoType[T]]) {
          (newLambdas, logicalTerm, newEnv: E) =>
            stFoldLeftValidationS(logicalTerm.args.keys)(newLambdas.success[NoType[T]]) {
              (newLambdas2, ident, newEnv2: E) =>
                ident match {
                  case TypeParamAppIdentity(param) =>
                    val (newEnv3, newRes) = if(param < nextParam)
                      unifier.findRootParamS(param)(newEnv2)
                    else
                      (newEnv2, param.success)
                    newRes.map {
                      rootParam =>
                        lambdas2.get(rootParam).map {
                          case (lambdaParam, TypeValueLambda(args, body)) =>
                            logicalTypeValueTermFromTypeValueTermS(body)(newEnv2).mapElements(identity, _.map { t => ((lambdaParam, TypeValueLambda(args, t)), true) })
                        }.orElse {
                          newLambdas.get(rootParam).map { case p => (newEnv2, (p, false).success) }
                        }.map {
                          case (newEnv4, Success(((lambdaParam, lambda @ TypeValueLambda(args, body: LogicalTypeValueTerm[T])), isNewLambda))) =>
                            val leafIdents = logicalTerm.args.keySet & body.args.keySet
                            if(leafIdents.forall { i => (logicalTerm.args.get(i) |@| body.args.get(i)) { (as1, as2) => as1.toVector === as2.toVector }.getOrElse(false) })
                              (newEnv4, (if(isNewLambda) newLambdas2 + (rootParam -> (lambdaParam, lambda)) else newLambdas2).success)
                            else
                              (newEnv4, NoType.fromError[T](Error("same type functions haven't same arguments at logical type expression", none, NoPosition)).failure)
                          case (newEnv4, Success(_)) =>
                            (newEnv4, NoType.fromError[T](FatalError("incorrect body of type value lambda", none, NoPosition)).failure)
                          case (newEnv4, Failure(noValue)) =>
                            (newEnv4, noValue.failure)
                        }.getOrElse((newEnv2, newLambdas2.success))
                    }.valueOr { nt => (newEnv3, nt.failure) }
                  case _                           =>
                   (newEnv2, newLambdas2.success)
                }
            } (newEnv)
        }  
      })
    } yield (lambdas ++ lambdas3.values)).run(env)
  }
}
