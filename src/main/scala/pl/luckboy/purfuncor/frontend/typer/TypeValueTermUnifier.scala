package pl.luckboy.purfuncor.frontend.typer
import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.kinder.Kind
import pl.luckboy.purfuncor.frontend.kinder.NoKind
import pl.luckboy.purfuncor.frontend.kinder.InferredKind
import pl.luckboy.purfuncor.frontend.kinder.InferringKind
import pl.luckboy.purfuncor.common.Unifier._
import pl.luckboy.purfuncor.frontend.KindTermUtils._

object TypeValueTermUnifier
{
  def matchesTypeValueTermListsWithReturnKindS[T, U, V, E](terms1: Seq[TypeValueTerm[T]], terms2: Seq[TypeValueTerm[T]])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]) = {
    val (env2, savedTypeMatching) = envSt.currentTypeMatchingFromEnvironmentS(env)
    val (env3, _) = envSt.setCurrentTypeMatchingS(TypeMatching.Types)(env2)
    val (env6, res2) = if(terms1.size === terms2.size) {
      val (env4, res) = terms1.zip(terms2).foldLeft((env3, (z, Seq[Kind]()).success[NoType[T]])) {
        case ((newEnv, Success((x, kinds))), (term1, term2)) => 
          val (newEnv2, newRes) = matchesTypeValueTermsS(term1, term2)(x)(f)(newEnv)
          newRes.map {
            x => envSt.returnKindFromEnvironmentS(newEnv2).mapElements(identity, k => (x, kinds :+ k).success)
          }.valueOr { nt => (newEnv2, nt.failure) }
        case ((newEnv, Failure(nt)), _)                      =>
          (newEnv, nt.failure)
      }
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
      val (env4, res) = lambdas1.zip(lambdas2).foldLeft((env3, (z, Seq[Kind]()).success[NoType[T]])) {
        case ((newEnv, Success((x, kinds))), (lambda1, lambda2)) => 
          val (newEnv2, newRes) = matchesTypeValueLambdasS(lambda1, lambda2)(x)(f)(newEnv)
          newRes.map {
            x => envSt.returnKindFromEnvironmentS(newEnv2).mapElements(identity, k => (x, kinds :+ k).success)
          }.valueOr { nt => (newEnv2, nt.failure) }
        case ((newEnv, Failure(nt)), _)                          =>
          (newEnv, nt.failure)
      }
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
    params.foldLeft((env, Seq[TypeValueLambda[T]]().success[NoType[T]])) {
      case ((newEnv, Success(lambdas)), param) =>
        val (newEnv2, res) = envSt.allocateTypeParamAppIdxS(newEnv)
        (newEnv2, res.map {
          pai => (lambdas :+ TypeValueLambda(Nil, TypeParamApp(param, Nil, pai))).success
        }.valueOr { _.failure })
      case ((newEnv, Failure(noType)), _)      =>
        (newEnv, noType.failure)
    }
  
  private def withTypeLambdaArgsWithReturnKindS[T, U, V, E](argParams: Seq[Set[Int]])(f: E => (E, Validation[NoType[T], U]))(env: E)(implicit envSt: TypeInferenceEnvironmentState[E, V, T]): (E, Validation[NoType[T], U]) =
    envSt.withTypeLambdaArgsS(argParams) {
      env2 =>
        val (env3, res) = argParams.foldLeft((env, Seq[Kind]().success[NoType[T]])) {
          case ((newEnv, Success(argParamKinds)), argParamSet) =>
            val argParamSeq = argParamSet.toSeq
            val (newEnv5, unifiedKindRes) = argParamSeq.headOption.map {
              argParam =>
                val (newEnv2, kindRes) = envSt.inferTypeValueTermKindS(TypeParamApp(argParam, Nil, 0))(newEnv)
                argParamSeq.foldLeft((newEnv, kindRes.valueOr { _.toNoKind }.success[NoType[T]])) {
                  case ((newEnv3, Success(kind1)), param2) =>
                    val (newEnv4, kindRes2) = envSt.inferTypeValueTermKindS(TypeParamApp(param2, Nil, 0))(newEnv3)
                    envSt.unifyKindsS(kind1, kindRes2.valueOr { _.toNoKind })(newEnv4)
                  case ((newEnv2, Failure(noType)), _)           =>
                    (newEnv2, noType.failure)
                }
            }.getOrElse((newEnv, NoType.fromError[T](FatalError("no type arguments", none, NoPosition)).failure))
            (newEnv5, unifiedKindRes.map { argParamKinds :+ _ })
          case ((newEnv, Failure(noType)), _)      =>
            (newEnv, noType.failure)
        }
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

  private def partiallyInstantiateTypeValueTermForMarkedParamsS[T, E](term: TypeValueTerm[T])(markedParams: Set[Int])(err: E => (E, NoType[T]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int]): (E, Validation[NoType[T], (TypeValueTerm[T], Option[Int])]) =
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
          case (env2, noKind: NoKind) => (env2, NoType.fromNoKind[T](noKind).failure)
          case (env2, _)              => envSt.addDelayedErrorsS(paramAppIdxs.map { (_, nt) }.toMap)(env2).mapElements(identity, _ => z.success)
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
  
  def appForGlobalTypeWithAllocatedTypeParamsS[T, U, E](funLoc: T, argLambdas: Seq[TypeValueLambda[T]])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    (for {
      res <- State({
        (env2: E) =>
          argLambdas.foldLeft((env2, Seq[TypeValueLambda[T]]().success[NoType[T]])) {
            case ((newEnv, Success(newArgLambdas)), argLambda) =>
              val (newEnv2, newRes) = instantiateS(argLambda.body)(newEnv)
              (newEnv2, newRes.map { b => newArgLambdas :+ TypeValueLambda(argLambda.argParams, b) })
            case ((newEnv, Failure(noType)), _)                =>
              (newEnv, noType.failure)
          }
      })
      res7 <- res.map {
        instantiatedArgLambdas =>
          (for {
            unallocatedParamAppIdx <- State(envSt.nextTypeParamAppIdxFromEnvironmentS)
            paramCount <- State(envSt.nextTypeParamFromEnvironmentS)
            res2 <- State(envSt.appForGlobalTypeS(funLoc, instantiatedArgLambdas, paramCount, unallocatedParamAppIdx))
            res6 <- res2.map {
              retTerm =>
                for {
                  allocatedParams <- State(envSt.allocatedTypeParamsFromEnvironmentS)
                  res3 <- allocateTypeValueTermParams(retTerm)(allocatedParams.map { p => p -> p }.toMap, unallocatedParamAppIdx)
                  res5 <- res3.map {
                    case (_, allocatedArgParams, _, retTerm2) =>
                      for {
                        res4 <- if(!allocatedArgParams.isEmpty)
                          State(envSt.inferTypeValueTermKindS(retTerm)(_: E).mapElements(identity, _.map { _ => () }))
                        else
                          State((_: E, ().success))
                      } yield (res4.map { _ => retTerm2 })
                  }.valueOr { nt => State((_: E, nt.failure)) }
                } yield res5
            }.valueOr { nt => State((_: E, nt.failure)) }
          } yield res6)
      }.valueOr { nt => State((_: E, nt.failure)) }
    } yield res7).run(env)
  
  private def matchesGlobalTypeAppWithTypeValueTermS[T, U, V, E](globalTypeApp1: GlobalTypeApp[T], term2: TypeValueTerm[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]): (E, Validation[NoType[T], U]) =
    (globalTypeApp1, term2) match {
      case (GlobalTypeApp(loc1, args1, sym1), GlobalTypeApp(loc2, args2, _)) =>
        val (env3, res) = if(loc1 === loc2) {
          val (env2, funKindRes) = envSt.inferTypeValueTermKindS(GlobalTypeApp(loc1, Nil, sym1))(env)
          unifier.withSaveS {
            matchesTypeValueLambdaListsWithReturnKindS(args1, args2, funKindRes.valueOr { _.toNoKind })(z)(f)(_)
          } (env2)
        } else
          unifier.mismatchedTermErrorS(env).mapElements(identity, _.failure)
        res match {
          case Success(x) =>
            (env3, x.success)
          case Failure(_) =>
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
      case (TypeParamApp(_, args1, _), TypeConjunction(_) | TypeDisjunction(_)) if !args1.isEmpty =>
        matchesLogicalTypeValueTermsS(typeParamApp1, term2)(z)(f)(env)
      case (TypeParamApp(param1, args1, paramAppIdx1), _) =>
        val (env2, noType) = mismatchedTypeValueTermNoTypeWithReturnKindS(typeParamApp1, term2)(env)
        addDelayedErrorsFromResultS(noType.failure, Set(paramAppIdx1))(z)(env2)
      case (_, _) =>
        unifier.mismatchedTermErrorS(env).mapElements(identity, _.failure)
    }
  
  private def instantiateAndSortPairsS[T, U, E](pair1: (TypeValueTerm[T], Option[Int]), pairs2: Seq[(TypeValueTerm[T], Option[Int])])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T], locEqual: Equal[T]) = {
    val (env2, res) = partiallyInstantiateTypeValueTermS(pair1._1)(unifier.mismatchedTermErrorS)(env)
    res.map {
      case (instantiatedTerm1, tmpOptInstantiatedParam1) =>
        val instantiatedPair1 = (instantiatedTerm1, pair1._2.orElse(tmpOptInstantiatedParam1))
        val (env3, res2) = pairs2.foldLeft((env2, Seq[(TypeValueTerm[T], Option[Int])]().success[NoType[T]])) {
          case ((newEnv, Success(newPairs2)), (term2, optParam1)) =>
            val (newEnv2, newRes) = partiallyInstantiateTypeValueTermS(term2)(unifier.mismatchedTermErrorS)(newEnv)
            (newEnv2, newRes.map { case (t, p) => newPairs2 :+ (t, optParam1.orElse(p)) })
          case ((newEnv, Failure(noType)), _)                                    =>
            (newEnv, noType.failure)
        }
        res2.map {
          instantiatedPairs2 =>
            instantiatedTerm1 match {
              case TypeParamApp(_, args1, _) =>
                val (tmpPairs2, thirdPairs2) = instantiatedPairs2.partition { _._1.isTypeParamApp }
                val (firstPairs2, secondPairs2) = tmpPairs2.partition { 
                  case (TypeParamApp(_, args2, _), _) => args1.size === args2.size
                  case _                              => false 
                }
                (env3, (instantiatedPair1, firstPairs2 ++ secondPairs2 ++ thirdPairs2).success)
              case _                         =>
                val (firstPairs2, thirdPairs2) = instantiatedPairs2.partition { !_._1.isTypeParamApp }
                (env3, (instantiatedPair1, firstPairs2 ++ thirdPairs2).success)
            }
        }.valueOr { nt => (env3, nt.failure) }
    }.valueOr { nt => (env2, nt.failure) }
  }

  private def matchesTypeValueTermsForTypeValueTermSubsetS[T, U, V, E](term1: TypeValueTerm[T], term2: TypeValueTerm[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]): (E, Validation[NoType[T], U]) = {
    partiallyInstantiateTypeValueTermS(term1)(unifier.mismatchedTermErrorS)(env) match {
      case (env2, Success((instantiatedTerm1, optInstantiatedParam1))) =>
        partiallyInstantiateTypeValueTermS(term2)(unifier.mismatchedTermErrorS)(env2) match {
          case (env3, Success((instantiatedTerm2, optInstantiatedParam2))) =>
            envSt.withParamCheckingS(optInstantiatedParam1.toSet ++ optInstantiatedParam2) {
              env4 =>
                (instantiatedTerm1, instantiatedTerm2) match {
                  case (TypeParamApp(_, Seq(), _), TypeConjunction(_) | TypeDisjunction(_)) =>
                    val (env5, res) = unifier.withSaveS {
                      matchesLogicalTypeValueTermsS(instantiatedTerm1, instantiatedTerm2)(z)(f)(_)
                    } (env4)
                    res match {
                      case Success(x) =>
                        (env5, x.success)
                      case Failure(_) =>
                        matchesTypeValueTermsS(instantiatedTerm1, instantiatedTerm2)(z)(f)(env5)
                    }
                  case (TypeConjunction(_) | TypeDisjunction(_), TypeParamApp(_, Seq(), _)) =>
                    val (env5, res) = unifier.withSaveS {
                      matchesLogicalTypeValueTermsS(instantiatedTerm1, instantiatedTerm2)(z)(f)(_)
                    } (env4)
                    res match {
                      case Success(x) =>
                        (env5, x.success)
                      case Failure(_) =>
                        matchesTypeValueTermsS(instantiatedTerm1, instantiatedTerm2)(z)(f)(env5)
                    }
                  case _ =>
                    matchesTypeValueTermsS(instantiatedTerm1, instantiatedTerm2)(z)(f)(env4)
                }
            } (env3)
          case (env3, Failure(noType)) =>
            (env3, noType.failure)
        }
      case (env2, Failure(noType)) =>
        (env2, noType.failure)
    }
  }
  
  private def checkTypeValueTermSubsetS[T, U, V, E](termSubset: Set[TypeValueTerm[T]], termSet: Set[TypeValueTerm[T]], areSwappedTerms: Boolean)(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]) = {
    val (env2, res) = termSubset.toSeq.foldLeft((env, (z, termSet.map { (_, none[Int]) }.toSeq, Seq[(TypeValueTerm[T], Option[Int])]()).success[NoType[T]])) {
      case ((newEnv, Success((x, firstPairs2, secondPairs2))), term1)  =>
        val (newEnv2, savedDelayedErrs) = envSt.delayedErrorsFromEnvironmentS(newEnv)
        val (newEnv3, newRes) = instantiateAndSortPairsS((term1, none), firstPairs2)(newEnv2)
        newRes match {
          case Success(((instantiatedTerm1, optInstantiatedParam1), instantiatedFirstPairs2)) =>
            val pairs2 = instantiatedFirstPairs2 ++ secondPairs2
            val (newEnv6, (newRes3, _)) = (0 until pairs2.size).foldLeft(unifier.mismatchedTermErrorS(newEnv3).mapElements(identity, nt => (nt.failure[(U, Int)], false))) {
              case ((newEnv4, (Failure(_), _) | (Success(_), false)), i) =>
                val (term2, optParam2) = pairs2(i)
                val (tmpTerm1, tmpTerm2) = if(areSwappedTerms) (term2, instantiatedTerm1) else (instantiatedTerm1, term2)
                val (newEnv5, (newRes2, areRestoredDelayedErrs)) = envSt.withDelayedErrorRestoringOrSavingS(savedDelayedErrs) { 
                  envSt.withParamCheckingS(optInstantiatedParam1.toSet ++ optParam2) {
                    matchesTypeValueTermsForTypeValueTermSubsetS(tmpTerm1, tmpTerm2)(x)(f)(_: E) 
                  }
                } (newEnv4)
                (newEnv5, (newRes2.map { (_, i) }, areRestoredDelayedErrs))
              case ((newEnv4, (newRes2, areRestoredDelayedErrs)), _)      =>
                (newEnv4, (newRes2, areRestoredDelayedErrs))
            }
            newRes3.map { 
              case (y, i) => 
                if(i < instantiatedFirstPairs2.size) 
                  (newEnv6, (y, instantiatedFirstPairs2.take(i) ++ instantiatedFirstPairs2.drop(i + 1), secondPairs2 :+ instantiatedFirstPairs2(i)).success)
                else
                  (newEnv6, (y, instantiatedFirstPairs2, secondPairs2).success)
            }.valueOr { nt => (newEnv6, nt.failure) }
          case Failure(noType) =>
            (newEnv3, noType.failure)
        }
      case ((newEnv, Failure(noType)), _) =>
        (newEnv, noType.failure)
    }
    (env2, res.map { _._1 })
  }

  private def evaluateLogicalTypeValueTermsS[T, U, V, E](terms: Set[TypeValueTerm[T]])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T]) =
    terms.foldLeft((env, (Set[TypeValueTerm[T]](), Set[Int]()).success[NoType[T]])) {
      case ((newEnv, Success((newTerms, newInstantiatedParams))), term) =>
        evaluateLogicalTypeValueTermS(term)(newEnv).mapElements(identity, _.map { p => (newTerms + p._1, newInstantiatedParams ++ p._2) } )
      case ((newEnv, Failure(noType)), _)      =>
        (newEnv, noType.failure)
    }
  
  private def evaluateLogicalTypeValueTermS[T, U, V, E](term: TypeValueTerm[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T]): (E, Validation[NoType[T], (TypeValueTerm[T], Set[Int])]) = {
    val (env2, res) = partiallyInstantiateTypeValueTermS(term)(unifier.mismatchedTermErrorS)(env)
    res match {
      case Success((instantiatedTerm, optInstantiatedParam)) =>
        val (env5, res2) = instantiatedTerm match {
          case GlobalTypeApp(loc, args, _) =>
            envSt.withRecursionCheckingS(Set(loc)) {
              env3 =>
                appForGlobalTypeWithAllocatedTypeParamsS(loc, args)(env3) match {
                  case (env4, Success(evaluatedTerm)) =>
                    evaluateLogicalTypeValueTermS(evaluatedTerm)(env4)
                  case (env4, Failure(noType))        =>
                    (env4, noType.failure)
                }
            } (env2)
          case TypeConjunction(terms)      =>
            evaluateLogicalTypeValueTermsS(terms)(env2).mapElements(identity, _.map { p => (TypeConjunction(p._1), p._2) })
          case TypeDisjunction(terms)      =>
            evaluateLogicalTypeValueTermsS(terms)(env2).mapElements(identity, _.map { p => (TypeDisjunction(p._1), p._2) })
          case _                           =>
            (env2, (instantiatedTerm, Set()).success)
        }
        (env5, res2.map { p => (p._1, optInstantiatedParam.toSet ++ p._2) })
      case Failure(noType)           =>
        (env2, noType.failure)
    }
  }
  
  private def distributeTypeDisjunctionOrTypeConjunctionS[T, U, V, E](typeDisj1: TypeDisjunction[T], typeConj2: TypeConjunction[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T]) = {
    val optPair = typeDisj1.distributedTypeValueTerm.map { (_, typeConj2) }.orElse {
      typeConj2.distributedTypeValueTerm.map { (typeDisj1, _) }
    }
    optPair.map { p => (env, (p, Set[Int]()).success) }.getOrElse { 
      val (env2, res) = evaluateLogicalTypeValueTermS(typeDisj1)(env)
      val (env3, res2) = res.map { 
        case (evaluatedTypeDisj1, instantiatedParams) => 
          evaluatedTypeDisj1.distributedTypeValueTerm.map { t => (env2, ((t, typeConj2), instantiatedParams).success) }.getOrElse {
            unifier.mismatchedTermErrorS(env2).mapElements(identity, _.failure)
          }
      }.valueOr { nt => (env2, nt.failure) }
      res2.map { p => (env3, p.success) }.getOrElse {
        val (env4, res3) = evaluateLogicalTypeValueTermS(typeConj2)(env3)
        res3.map { 
          case (evaluatedTypeConj2, instantiatedParams) => 
            evaluatedTypeConj2.distributedTypeValueTerm.map { t => (env4, ((typeDisj1, t), instantiatedParams).success) }.getOrElse {
              unifier.mismatchedTermErrorS(env4).mapElements(identity, _.failure)
            }
        }.valueOr { nt => (env4, nt.failure) }
      }
    }
  }
  
  private def normalizeLogicalTypeValueTerm[T](term: TypeValueTerm[T]) =
    term match {
      case TypeConjunction(terms) if terms.size <= 1 => terms.headOption.toSuccess(NoType.fromError[T](FatalError("no type value term", none, NoPosition)))
      case TypeDisjunction(terms) if terms.size <= 1 => terms.headOption.toSuccess(NoType.fromError[T](FatalError("no type value term", none, NoPosition)))
      case _                                         => term.success
    }
  
  private def matchesLogicalTypeValueTermsS[T, U, V, E](term1: TypeValueTerm[T], term2: TypeValueTerm[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]): (E, Validation[NoType[T], U]) = {
    val (env2, typeMatching) = envSt.currentTypeMatchingFromEnvironmentS(env)
    typeMatching match {
      case TypeMatching.Types             =>
        val (env3, _) = envSt.setCurrentTypeMatchingS(TypeMatching.SupertypeWithType)(env2)
        matchesLogicalTypeValueTermsS(term1, term2)(z)(f)(env3) match {
          case (env4, Success(x))      =>
            val (env5, _) = envSt.setCurrentTypeMatchingS(TypeMatching.TypeWithSupertype)(env4)
            matchesLogicalTypeValueTermsS(term1, term2)(x)(f)(env5)
          case (env4, Failure(noType)) =>
            (env4, noType.failure)
        }
      case TypeMatching.SupertypeWithType =>
        (for { 
          t1 <- normalizeLogicalTypeValueTerm(term1)
          t2 <- normalizeLogicalTypeValueTerm(term2)
        } yield (t1, t2)) match {
          case Success((normalizedTerm1, normalizedTerm2)) =>
            (normalizedTerm1, normalizedTerm2) match {
              case (typeDisj1 @ TypeDisjunction(terms1), typeConj2 @ TypeConjunction(terms2)) =>
                val (env3, res) = distributeTypeDisjunctionOrTypeConjunctionS(typeDisj1, typeConj2)(env2)
                val (env4, res2) = res match {
                  case Success(((distributedTerm1, distributedTerm2), instantiatedParams)) =>
                    envSt.withParamCheckingS(instantiatedParams) {
                      unifier.withSaveS {
                        matchesLogicalTypeValueTermsS(distributedTerm1, distributedTerm2)(z)(f)(_)
                      }
                    } (env3)
                  case Failure(noType)                               =>
                    (env3, noType.failure) 
                }
                res2 match {
                  case Success(x) =>
                    (env4, x.success)
                  case Failure(_) =>
                    val (env5, res3) = unifier.withSaveS {
                      checkTypeValueTermSubsetS(Set(normalizedTerm2), terms1, true)(z)(f)(_)
                    } (env4)
                    res3 match {
                      case Success(x) =>
                        (env5, x.success)
                      case Failure(_) =>
                        checkTypeValueTermSubsetS(Set(normalizedTerm1), terms2, false)(z)(f)(env5)
                    }
                }
              case (_, TypeDisjunction(terms2)) =>
                checkTypeValueTermSubsetS(terms2, Set(normalizedTerm1), true)(z)(f)(env2)
              case (TypeConjunction(terms1), _) =>
                checkTypeValueTermSubsetS(terms1, Set(normalizedTerm2), false)(z)(f)(env2)
              case (GlobalTypeApp(loc1, args1, _), TypeConjunction(terms2)) =>
                val (env3, res) = unifier.withSaveS {
                  checkTypeValueTermSubsetS(Set(normalizedTerm1), terms2, false)(z)(f)(_)
                } (env2)
                res match {
                  case Success(x) =>
                    (env3, x.success)
                  case Failure(_) =>
                    envSt.withRecursionCheckingS(Set(loc1)) { 
                      env4 =>
                        appForGlobalTypeWithAllocatedTypeParamsS(loc1, args1)(env4) match {
                          case (env5, Success(evaluatedTerm1)) =>
                            matchesTypeValueTermsS(evaluatedTerm1, term2)(z)(f)(env5)
                          case (env5, Failure(noType))         =>
                            (env5, noType.failure)
                        }
                    } (env3)
                }
              case (_, TypeConjunction(terms2)) =>
                checkTypeValueTermSubsetS(Set(normalizedTerm1), terms2, false)(z)(f)(env2)
              case (TypeDisjunction(terms1), GlobalTypeApp(loc2, args2, _)) =>
                val (env3, res) = unifier.withSaveS {
                  checkTypeValueTermSubsetS(Set(normalizedTerm2), terms1, true)(z)(f)(_: E)
                } (env2)
                res match {
                  case Success(x) =>
                    (env3, x.success)
                  case Failure(_) =>
                    envSt.withRecursionCheckingS(Set(loc2)) {
                      env4 =>
                       appForGlobalTypeWithAllocatedTypeParamsS(loc2, args2)(env4) match {
                         case (env5, Success(evaluatedTerm1)) =>
                           matchesTypeValueTermsS(evaluatedTerm1, term2)(z)(f)(env5)
                         case (env5, Failure(noType))         =>
                           (env5, noType.failure)
                       }
                    } (env3)
                }
              case (TypeDisjunction(terms1), _) =>
                checkTypeValueTermSubsetS(Set(normalizedTerm2), terms1, true)(z)(f)(env2)
              case _ =>
                matchesTypeValueTermsS(normalizedTerm1, normalizedTerm2)(z)(f)(env2)
            }
          case Failure(noType) =>
            (env2, noType.failure)
        }
      case TypeMatching.TypeWithSupertype =>
        val (env3, _) = reverseTypeMatchingS(env2)
        matchesLogicalTypeValueTermsS(term2, term1)(z)(f)(env3)
    }
  }  
  
  private def matchesBuiltinTypeWithTypeValueTermS[T, U, V, E](term: TypeValueTerm[T])(z: U)(env: E)(implicit envSt: TypeInferenceEnvironmentState[E, V, T]) =
    (for {
      retKindRes <- State(envSt.inferTypeValueTermKindS(term))
      unifiedRetKindRes <- State(envSt.unifyKindsS(InferredKind(Star(KindType, NoPosition)), retKindRes.valueOr { _.toNoKind }))
      _ <- State(envSt.setReturnKindS(unifiedRetKindRes.valueOr { _.toNoKind }))
    } yield (unifiedRetKindRes.map { _ => z })).run(env)
  
  def matchesTypeValueTermsS[T, U, V, E](term1: TypeValueTerm[T], term2: TypeValueTerm[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T], locEqual: Equal[T]): (E, Validation[NoType[T], U]) = {
    val (env2, typeMatching) = envSt.currentTypeMatchingFromEnvironmentS(env)
    val (env7, res) = partiallyInstantiateTypeValueTermS(term1)(unifier.mismatchedTermErrorS)(env2) match {
      case (env3, Success((instantiatedTerm1, optInstantiatedParam1))) =>
        partiallyInstantiateTypeValueTermS(term2)(unifier.mismatchedTermErrorS)(env3) match {
          case (env4, Success((instantiatedTerm2, optInstantiatedParam2))) =>
            envSt.withParamCheckingS(optInstantiatedParam1.toSet ++ optInstantiatedParam2) {
              env5 =>
                (instantiatedTerm1, instantiatedTerm2) match {
                  case (TupleType(args1), TupleType(args2)) if args1.size === args2.size =>
                    matchesTypeValueTermListsWithReturnKindS(args1, args2)(z)(f)(env5)
                  case (BuiltinType(bf1, args1), BuiltinType(bf2, args2)) if bf1 === bf2 && args1.size === args2.size =>
                    matchesTypeValueTermListsWithReturnKindS(args1, args2)(z)(f)(env5)
                  case (Unittype(loc1, args1, _), Unittype(loc2, args2, _)) if loc1 === loc2 &&  args1.size === args2.size =>
                    matchesTypeValueTermListsWithReturnKindS(args1, args2)(z)(f)(env5)
                  case (typeParamApp1: TypeParamApp[T], _) =>
                    matchesTypeParamAppWithTypeValueTermS(typeParamApp1, instantiatedTerm2)(z)(f)(env5)
                  case (_, typeParamApp2: TypeParamApp[T]) =>
                    val (env6, _) = reverseTypeMatchingS(env5)
                    matchesTypeParamAppWithTypeValueTermS(typeParamApp2, instantiatedTerm1)(z)(f)(env6)
                  case (BuiltinType(TypeBuiltinFunction.Any, Seq()), _) if typeMatching === TypeMatching.SupertypeWithType =>
                    matchesBuiltinTypeWithTypeValueTermS(instantiatedTerm2)(z)(env5)
                  case (_, BuiltinType(TypeBuiltinFunction.Nothing, Seq())) if typeMatching === TypeMatching.SupertypeWithType =>
                    matchesBuiltinTypeWithTypeValueTermS(instantiatedTerm1)(z)(env5)
                  case (_, BuiltinType(TypeBuiltinFunction.Any, Seq())) if typeMatching === TypeMatching.TypeWithSupertype =>
                    matchesBuiltinTypeWithTypeValueTermS(instantiatedTerm1)(z)(env5)
                  case (BuiltinType(TypeBuiltinFunction.Nothing, Seq()), _) if typeMatching === TypeMatching.TypeWithSupertype =>
                    matchesBuiltinTypeWithTypeValueTermS(instantiatedTerm2)(z)(env5)
                  case (globalTypeApp1: GlobalTypeApp[T], _) =>
                    matchesGlobalTypeAppWithTypeValueTermS(globalTypeApp1, instantiatedTerm2)(z)(f)(env5)
                  case (_, globalTypeApp2: GlobalTypeApp[T]) =>
                    val (env6, _) = reverseTypeMatchingS(env5)
                    matchesGlobalTypeAppWithTypeValueTermS(globalTypeApp2, instantiatedTerm1)(z)(f)(env6)
                  case (TypeConjunction(_) | TypeDisjunction(_), _) =>
                    matchesLogicalTypeValueTermsS(instantiatedTerm1, instantiatedTerm2)(z)(f)(env5)
                  case (_, TypeConjunction(_) | TypeDisjunction(_)) =>
                    matchesLogicalTypeValueTermsS(instantiatedTerm1, instantiatedTerm2)(z)(f)(env5)
                  case (_, _) =>
                    unifier.mismatchedTermErrorS(env5).mapElements(identity, _.failure)
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
  
  def replaceTypeParamsFromTypeValueTermsS[T, E](terms: Seq[TypeValueTerm[T]])(f: (Int, E) => (E, Validation[NoType[T], Either[Int, TypeValueTerm[T]]]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int]) =
    terms.foldLeft((env, Seq[TypeValueTerm[T]]().success[NoType[T]])) {
      case ((newEnv, Success(ts)), t) => replaceTypeValueTermParamsS(t)(f)(newEnv).mapElements(identity, _.map { ts :+ _ })
      case ((newEnv, Failure(nt)), _) => (newEnv, nt.failure)
    }
  
  def replaceTypeParamsFromTypeValueLambdasS[T, E](lambdas: Seq[TypeValueLambda[T]])(f: (Int, E) => (E, Validation[NoType[T], Either[Int, TypeValueTerm[T]]]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int]) =
    lambdas.foldLeft((env, Seq[TypeValueLambda[T]]().success[NoType[T]])) {
      case ((newEnv, Success(ls)), l) => replaceTypeValueLambdaParamsS(l)(f)(newEnv).mapElements(identity, _.map { ls :+ _ })
      case ((newEnv, Failure(nt)), _) => (newEnv, nt.failure)
    }
  
  def replaceTypeValueLambdaParamsS[T, E](lambda: TypeValueLambda[T])(f: (Int, E) => (E, Validation[NoType[T], Either[Int, TypeValueTerm[T]]]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int]) =
    lambda match {
      case TypeValueLambda(argParams, body) => replaceTypeValueTermParamsS(body)(f)(env).mapElements(identity, _.map { TypeValueLambda(argParams, _) })
    }
  
  def replaceTypeValueTermParamsS[T, E](term: TypeValueTerm[T])(f: (Int, E) => (E, Validation[NoType[T], Either[Int, TypeValueTerm[T]]]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int]): (E, Validation[NoType[T], TypeValueTerm[T]]) =
    term match {
      case TupleType(args) =>
        replaceTypeParamsFromTypeValueTermsS(args)(f)(env).mapElements(identity, _.map { TupleType(_) })
      case BuiltinType(bf, args) =>
        replaceTypeParamsFromTypeValueTermsS(args)(f)(env).mapElements(identity, _.map { BuiltinType(bf, _) })
      case Unittype(loc, args, sym) =>
        replaceTypeParamsFromTypeValueTermsS(args)(f)(env).mapElements(identity, _.map { Unittype(loc, _, sym) })
      case GlobalTypeApp(loc, args, sym) =>
        replaceTypeParamsFromTypeValueLambdasS(args)(f)(env).mapElements(identity, _.map { GlobalTypeApp(loc, _, sym) })
      case TypeParamApp(param, args, paramAppIdx) =>
        val (env2, res) = f(param, env)
        res match {
          case Success(Left(param2))     =>
            val (env3, res2) = replaceTypeParamsFromTypeValueLambdasS(args)(f)(env2)
            (env3, res2.map { TypeParamApp(param2, _, paramAppIdx) })
          case Success(Right(paramTerm)) =>
            paramTerm match {
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
          case Failure(noType)           =>
            (env2, noType.failure)
        }
      case TypeConjunction(terms) =>
        val (env2, res) = terms.foldLeft((env, TypeConjunction[T](Set()).success[NoType[T]])) {
          case ((newEnv, Success(newTypeConj)), term2) => 
            val (newEnv2, newRes) = replaceTypeValueTermParamsS(term2)(f)(newEnv)
            (newEnv2, newRes.map { newTypeConj & _ })
          case ((newEnv, Failure(noType)), _)          =>
            (newEnv, noType.failure)
        }
        (env2, res.flatMap {
          newTypeConj => 
            newTypeConj.terms.headOption.map {
              term2 => (if(newTypeConj.terms.size === 1) term2 else newTypeConj).success
            }.getOrElse(NoType.fromError[T](FatalError("type conjunction doesn't have type value terms", none, NoPosition)).failure)
        })
      case TypeDisjunction(terms) =>
        val (env2, res) = terms.foldLeft((env, TypeDisjunction[T](Set()).success[NoType[T]])) {
          case ((newEnv, Success(newTypeDisj)), term2) =>
            val (newEnv2, newRes) = replaceTypeValueTermParamsS(term2)(f)(newEnv)
            (newEnv2, newRes.map { newTypeDisj | _ })
          case ((newEnv, Failure(noType)), _)          =>
            (newEnv, noType.failure)
        }
        (env2, res.flatMap {
          newTypeDisj =>
            newTypeDisj.terms.headOption.map {
              term2 => (if(newTypeDisj.terms.size === 1) term2 else newTypeDisj).success
            }.getOrElse(NoType.fromError[T](FatalError("type disjunction doesn't have type value terms", none, NoPosition)).failure)
        })
    }
  
  private def unsafeAllocateTypeParamsFromTypeValueTermsS[T, U, E](terms: Seq[TypeValueTerm[T]])(allocatedParams: Map[Int, Int], unallocatedParamAppIdx: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    terms.foldLeft((env, (allocatedParams, Set[Int](), Set[Int](), Seq[TypeValueTerm[T]]()).success[NoType[T]])) {
      case ((newEnv, Success((newAllocatedParams, newAllocatedArgParams, allocatedParamAppIdxs, newTerms))), term) =>
        val (newEnv2, newRes) = unsafeAllocateTypeValueTermParamsS(term)(newAllocatedParams, unallocatedParamAppIdx)(newEnv)
        (newEnv2, newRes.map { 
          _.mapElements(identity, newAllocatedArgParams | _, allocatedParamAppIdxs | _, newTerms :+ _)
        })
      case ((newEnv, Failure(noType)), _)                            =>
        (newEnv, noType.failure)
    }
    
  private def unsafeAllocateTypeParamsFromTypeValueLambdasS[T, U, E](lambdas: Seq[TypeValueLambda[T]])(allocatedParams: Map[Int, Int], unallocatedParamAppIdx: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    lambdas.foldLeft((env, (allocatedParams, Set[Int](), Set[Int](), Seq[TypeValueLambda[T]]()).success[NoType[T]])) {
      case ((newEnv, Success((newAllocatedParams, newAllocatedArgParams, allocatedParamAppIdxs, newTerms))), lambda) =>
        val (newEnv2, newRes) = unsafeAllocateTypeValueLambdaParamsS(lambda)(newAllocatedParams, unallocatedParamAppIdx)(newEnv)
        (newEnv2, newRes.map {
          _.mapElements(identity, newAllocatedArgParams | _, allocatedParamAppIdxs | _, newTerms :+ _)
        })
      case ((newEnv, Failure(noType)), _)                              =>
        (newEnv, noType.failure)
    }

  private def unsafeAllocateTypeValueLambdaParamsS[T, U, E](lambda: TypeValueLambda[T])(allocatedParams: Map[Int, Int], unallocatedParamAppIdx: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    lambda match {
      case TypeValueLambda(argParams, body) =>
        val (env2, res) = argParams.foldLeft((env, (Map[Int, Int](), Seq[Int]()).success[NoType[T]])) {
          case ((newEnv, Success((newAllocatedArgParams, newArgParams))), argParam) =>
            allocatedParams.get(argParam).map { argParam2 => (newEnv, (newAllocatedArgParams, newArgParams :+ argParam2).success) }.getOrElse {
              val (newEnv2, res) = unifier.allocateParamS(newEnv)
              res.map { argParam2 => (newEnv2, (newAllocatedArgParams + (argParam -> argParam2), newArgParams :+ argParam2).success) }.valueOr {
                nt => (newEnv2, nt.failure)
              }
            }
          case ((newEnv, Failure(noType)), _)                                       =>
            (newEnv, noType.failure)
        }
        res match {
          case Success((allocatedArgParams, argParams2)) =>
            val (env3, res2) = unsafeAllocateTypeValueTermParamsS(body)(allocatedParams ++ allocatedArgParams, unallocatedParamAppIdx)(env2)
            val argParamsSet2 = argParams2.toSet
            (env3, res2.map { _.mapElements(_.flatMap { case (p, p2) => if(argParamsSet2.contains(p2)) Map() else some(p -> p2) }, _ => argParamsSet2, identity, TypeValueLambda(argParams2, _)) })
          case Failure(noType)                           =>
            (env2, noType.failure)
        }
    }

  private def unsafeAllocateTypeValueTermParamsS[T, U, E](term: TypeValueTerm[T])(allocatedParams: Map[Int, Int], unallocatedParamAppIdx: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]): (E, Validation[NoType[T], (Map[Int, Int], Set[Int], Set[Int], TypeValueTerm[T])]) =
    term match {
      case TupleType(args) =>
        val (env2, res) = unsafeAllocateTypeParamsFromTypeValueTermsS(args)(allocatedParams, unallocatedParamAppIdx)(env)
        (env2, res.map { _.mapElements(identity, identity, identity, TupleType(_)) })
      case BuiltinType(bf, args) =>
        val (env2, res) = unsafeAllocateTypeParamsFromTypeValueTermsS(args)(allocatedParams, unallocatedParamAppIdx)(env)
        (env2, res.map { _.mapElements(identity, identity, identity, BuiltinType(bf, _)) })
      case Unittype(loc, args, sym) =>
        val (env2, res) = unsafeAllocateTypeParamsFromTypeValueTermsS(args)(allocatedParams, unallocatedParamAppIdx)(env)
        (env2, res.map { _.mapElements(identity, identity, identity, Unittype(loc, _, sym)) })
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
      case TypeConjunction(terms) =>
        val (env2, res) = unsafeAllocateTypeParamsFromTypeValueTermsS(terms.toSeq)(allocatedParams, unallocatedParamAppIdx)(env)
        (env2, res.map { _.mapElements(identity, identity, identity, ts => TypeConjunction(ts.toSet)) })
      case TypeDisjunction(terms) =>
        val (env2, res) = unsafeAllocateTypeParamsFromTypeValueTermsS(terms.toSeq)(allocatedParams, unallocatedParamAppIdx)(env)
        (env2, res.map { _.mapElements(identity, identity, identity, ts => TypeDisjunction(ts.toSet)) })
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
            (for {
              res2 <- State({
                (env4: E) =>
                  kinds.foldLeft((env4, Map[Int, Kind]().success[NoType[T]])) {
                    case ((newEnv, Success(newInferringKinds)), (param, kind)) =>
                      val (newEnv2, inferringKindRes) = envSt.inferringKindFromKindS(kind)(newEnv)
                      (newEnv2, inferringKindRes.flatMap { 
                        inferringKind => 
                          allocatedParams.get(param).map { p => (newInferringKinds + (p -> inferringKind)).success }.getOrElse {
                            NoType.fromError[T](FatalError("unallocated parameter", none, NoPosition)).failure
                          }
                      })
                    case ((newEnv, Failure(noType)), _)                        =>
                      (newEnv, noType.failure)
                  }
              })
              res4 <- res2.map {
                inferringKinds =>
                  for {
                    _ <- State(envSt.setTypeParamKindsS(inferringKinds))
                    res3 <- if(mustInferKinds || !allocatedArgParams.isEmpty)
                      State(envSt.inferTypeValueTermKindS(term2))
                    else
                      State((_: E, ().success))
                  } yield (res3.map { _ => (allocatedParams, allocatedArgParams, allocatedParamAppIdxs, term2) })
              }.valueOr { nt => State((_: E, nt.failure)) }
            } yield res4).run(env3)
        }.valueOr { nt => (env3, nt.failure) }
    } (env)
  
  def checkDefinedTypeS[T, E](definedType: DefinedType[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int]) = {
    val params = definedType.args.flatMap { _.param }.toSet
    val (env2, res) = params.foldLeft((env, BitSet().success[NoType[T]])) {
      case ((newEnv, Success(rps)), p) => unifier.findRootParamS(p)(newEnv).mapElements(identity, _.map { rps + _ })
      case ((newEnv, Failure(nt)), _)  => (newEnv, nt.failure)
    }
    (env2, res.map {
      rootParams => if(rootParams.size === params.size) ().success else NoType.fromError[T](Error("parameters are distinct at defined type " + definedType, none, definedType.pos)).failure
    }.valueOr { _.failure })
  }
  
  def checkDefinedTypesS[T, E](definedTypes: Seq[DefinedType[T]])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int]): (E, Validation[NoType[T], Unit]) =
    definedTypes.foldLeft((env, ().success[NoType[T]])) {
      case ((newEnv, newRes), definedType) =>
        checkDefinedTypeS(definedType)(newEnv).mapElements(identity, r => (newRes |@| r) { (u, _) => u })
    }
  
  private def checkDefinedTypes2[T, E](definedTypes: Seq[DefinedType[T]])(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int]) =
    State(checkDefinedTypesS[T, E](definedTypes))
  
  def checkDefinedTypes[T, E](definedTypes: Seq[DefinedType[T]])(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int]) =
    checkDefinedTypes2[T, E](definedTypes)
  
  def normalizeTypeAppS[T, U, E](typeApp: TypeApp[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    (for {
      res <- State(envSt.inferTypeValueTermKindS(typeApp))
      res7 <- res.map {
        kind =>
          for {
            res2 <- State(envSt.argCountFromKindS(kind))
            res5 <- res2.map {
              argCount =>
                if(argCount > 0) {
                  for {
                    res3 <- State({
                      (env2: E) =>
                        (0 until argCount).foldLeft((env2, Seq[(Int, Int)]().success[NoType[T]])) {
                          case ((newEnv, Success(newPairs)), _) =>
                            val (newEnv2, newRes) = unifier.allocateParamS(newEnv)
                            newRes.map {
                              param =>
                                val (newEnv3, newRes2) = envSt.allocateTypeParamAppIdxS(newEnv2)
                                (newEnv3, newRes2.map {
                                  paramAppIdx => (newPairs :+ (param, paramAppIdx)).success
                                }.valueOr { _.failure })
                            }.valueOr { nt => (newEnv2, nt.failure) }
                          case ((newEnv, Failure(nt)), _) =>
                           (newEnv, nt.failure)
                        }
                    })
                    res4 <- res3.map {
                      ps =>
                        val args2 = ps.map { p => TypeValueLambda[T](Nil, TypeParamApp(p._1, Nil, p._2)) }
                        val typeApp2 = typeApp.withArgs(typeApp.args ++ args2)
                        State(envSt.inferTypeValueTermKindS(typeApp2)).map { _.map { _ => typeApp2 } }
                    }.valueOr { nt => State((_: E, nt.failure)) }
                  } yield res4
                } else
                  State((_: E, typeApp.success))
            }.valueOr { nt => State((_: E, nt.failure)) }
          } yield res5
      }.valueOr { nt => State((_: E, nt.failure)) }
    } yield res7).run(env)
  
  def normalizeTypeValueTermS[T, U, E](term: TypeValueTerm[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, U, T]) =
    partiallyInstantiateTypeValueTermS(term) { (_: E, inifityTypeValueTermNoType) } (env) match {
      case (env2, Success((typeApp: TypeApp[T], _))) => normalizeTypeAppS(typeApp)(env2)
      case (env2, Success(_))                   => (env2, term.success)
      case (env2, Failure(noType))              => (env2, noType.failure)
    }
}