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

object TypeValueTermUnifier
{
  def matchesTypeValueTermListsWithReturnKindS[T, U, E](terms1: Seq[TypeValueTerm[T]], terms2: Seq[TypeValueTerm[T]])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T], locEqual: Equal[T]) = {
    val (env2, savedTypeMatching) = envSt.currentTypeMatchingFromEnvironmentS(env)
    val (env3, _) = envSt.setCurrentTypeMatchingS(TypeMatching.Types)(env2)
    val (env6, res2) = if(terms1.size === terms2.size) {
      val (env4, res) = terms1.zip(terms2).foldLeft((env3, (z, Seq[Kind]()).success[NoType[T]])) {
        case ((newEnv, Success((x, kinds))), (term1, term2)) => 
          val (newEnv2, newRes) = matchesTypeValueTermsS(term1, term2)(x)(f)(newEnv)
          newRes.map {
            x =>
              val (newEnv3, kind) = envSt.returnKindFromEnvironmentS(newEnv2)
              (newEnv3, (x, kinds :+ kind).success)
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
  
  def matchesTypeValueLambdaListsWithReturnKindS[T, U, E](lambdas1: Seq[TypeValueLambda[T]], lambdas2: Seq[TypeValueLambda[T]], funKind: Kind)(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T], locEqual: Equal[T]) = {
    val (env2, savedTypeMatching) = envSt.currentTypeMatchingFromEnvironmentS(env)
    val (env3, _) = envSt.setCurrentTypeMatchingS(TypeMatching.Types)(env2)
    val (env6, res2) = if(lambdas1.size === lambdas2.size) {
      val (env4, res) = lambdas1.zip(lambdas2).foldLeft((env3, (z, Seq[Kind]()).success[NoType[T]])) {
        case ((newEnv, Success((x, kinds))), (lambda1, lambda2)) => 
          val (newEnv2, newRes) = matchesTypeValueLambdasS(lambda1, lambda2)(x)(f)(newEnv)
          newRes.map {
            x =>
              val (newEnv3, kind) = envSt.returnKindFromEnvironmentS(newEnv2)
              (newEnv3, (x, kinds :+ kind).success)
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
  
  private def typeValueLambdasFromTypeParamsS[T, E](params: Seq[Int])(env: E)(implicit envSt: TypeInferenceEnvironmentState[E, T]): (E, Validation[NoType[T], Seq[TypeValueLambda[T]]]) =
    params.foldLeft((env, Seq[TypeValueLambda[T]]().success[NoType[T]])) {
      case ((newEnv, Success(lambdas)), param) =>
        val (newEnv2, res) = envSt.allocateTypeParamAppIdxS(newEnv)
        res.map {
          pai => (newEnv2, (lambdas :+ TypeValueLambda(Nil, TypeParamApp(param, Nil, pai))).success)
        }.valueOr { nt => (newEnv2, nt.failure) }
      case ((newEnv, Failure(noType)), _)      =>
        (newEnv, noType.failure)
    }
  
  private def withTypeLambdaArgsWithReturnKindS[T, U, E](argParams: Seq[Set[Int]])(f: E => (E, Validation[NoType[T], U]))(env: E)(implicit envSt: TypeInferenceEnvironmentState[E, T]): (E, Validation[NoType[T], U]) =
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
               val (env5, retKind) = envSt.returnKindFromEnvironmentS(env4)
               val (env6, lambdaKindRes) = envSt.lambdaKindS(argKinds, retKind)(env5)
               val (env7, _) = envSt.setReturnKindS(lambdaKindRes.valueOr { _.toNoKind })(env6)
               (env7, x.success)
            }.valueOr { nt => (env4, nt.failure) }
          case Failure(noType) =>
            (env3, noType.failure)
        }
    } (env)
  
  @tailrec
  def matchesTypeValueLambdasS[T, U, E](lambda1: TypeValueLambda[T], lambda2: TypeValueLambda[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T], locEqual: Equal[T]): (E, Validation[NoType[T], U]) =
    (lambda1, lambda2) match {
      case (TypeValueLambda(argParams1, body1), TypeValueLambda(argParams2, body2)) if argParams1.size === argParams2.size =>
        val argParams = argParams1.zip(argParams2).map { case (p1, p2) => Set(p1, p2) }
        withTypeLambdaArgsWithReturnKindS(argParams)(matchesTypeValueTermsS(body1, body2)(z)(f))(env)
      case (TypeValueLambda(argParams1, typeApp1: TypeApp[T]), TypeValueLambda(argParams2, body2)) if argParams1.size < argParams2.size =>
        val otherArgParams = argParams2.drop(argParams1.size)
        val argParams = argParams1.zip(argParams2).map { case (p1, p2) => Set(p1, p2) } ++ otherArgParams.map(Set(_))
        withTypeLambdaArgsWithReturnKindS(argParams) {
          (env2: E) =>
            typeValueLambdasFromTypeParamsS(otherArgParams)(env2) match {
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
        matchesTypeValueLambdasS(lambda2, lambda1)(z)(f)(env)
      case (_, _) =>
        unifier.mismatchedTermErrorS(env).mapElements(identity, _.failure)
    }

  def partiallyInstantiateTypeValueTermS[T, E](term: TypeValueTerm[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int]): (E, Validation[NoType[T], TypeValueTerm[T]]) =
    term match {
      case TypeParamApp(param, args, paramAppIdx) =>
        val (env2, rootParamRes) = unifier.findRootParamS(param)(env)
        rootParamRes match {
          case Success(rootParam) =>
          	val (env3, optParamTerm) = unifier.getParamTermS(rootParam)(env2)
          	optParamTerm match {
          	  case Some(paramTerm) =>
          	    val (env4, res) = partiallyInstantiateTypeValueTermS(paramTerm)(env3)
          	    res.map {
          	      case GlobalTypeApp(loc2, args2, sym2) => 
          	        (env4, GlobalTypeApp(loc2, args2 ++ args, sym2).success)
          	      case TypeParamApp(param2, args2, _)   =>
          	        (env4, TypeParamApp(param2, args2 ++ args, paramAppIdx).success)
          	      case term2                            =>
          	        if(args.isEmpty) 
          	          (env4, term2.success)
          	        else
          	          (env4, NoType.fromError[T](FatalError("type value term isn't type application", none, NoPosition)).failure)
          	    }.valueOr { nt => (env4, nt.failure) }
          	  case None            =>
          	    (env3, term.success)
          	}
          case Failure(noType)    =>
            (env2, noType.failure)
        }
      case _                                      =>
        (env, term.success)
    }
  
  private def setReturnKindFromTypeValueTermsS[T, E](term1: TypeValueTerm[T], term2: TypeValueTerm[T])(env: E)(implicit envSt: TypeInferenceEnvironmentState[E, T]) = {
    val (env2, funKindRes1) = envSt.inferTypeValueTermKindS(term1)(env)
    val (env3, funKindRes2) = envSt.inferTypeValueTermKindS(term2)(env2)
    val (env4, unifiedFunKindRes) = envSt.unifyKindsS(funKindRes1.valueOr { _.toNoKind }, funKindRes2.valueOr { _.toNoKind })(env3)
    envSt.setReturnKindS(unifiedFunKindRes.valueOr { _.toNoKind })(env4)      
  }
    
  private def mismatchedTypeValueTermNoTypeWithReturnKindS[T, E](term1: TypeValueTerm[T], term2: TypeValueTerm[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T]) = {
    val (env2, res) = setReturnKindFromTypeValueTermsS(term1, term2)(env)
    unifier.mismatchedTermErrorS(env2)
  }

  private def addDelayedErrorsFromResultS[T, U, E](res: Validation[NoType[T], U], paramAppIdxs: Set[Int])(z: U)(env: E)(implicit envSt: TypeInferenceEnvironmentState[E, T]) =
    res.map { x => (env, x.success) }.valueOr {
      nt => 
        envSt.returnKindFromEnvironmentS(env) match {
          case (env2, noKind: NoKind) => (env2, NoType.fromNoKind[T](noKind).failure)
          case (env2, _)              => envSt.addDelayedErrorsS(paramAppIdxs.map { (_, nt) }.toMap)(env2).mapElements(identity, _ => z.success)
        }
    }
    
  private def reverseTypeMatchingS[T, E](env: E)(implicit envSt: TypeInferenceEnvironmentState[E, T]) = {
    val (env2, oldTypeMatching) = envSt.currentTypeMatchingFromEnvironmentS(env)
    val newTypeMatching = oldTypeMatching match {
      case TypeMatching.Types             => TypeMatching.Types
      case TypeMatching.SupertypeWithType => TypeMatching.TypeWithSupertype
      case TypeMatching.TypeWithSupertype => TypeMatching.SupertypeWithType
    }
    envSt.setCurrentTypeMatchingS(newTypeMatching)(env2)
  }
  
  def appForGlobalTypeWithAllocatedTypeParamsS[T, E](funLoc: T, argLambdas: Seq[TypeValueLambda[T]])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T]) = {
    val (env2, res) = argLambdas.foldLeft((env, Seq[TypeValueLambda[T]]().success[NoType[T]])) {
      case ((newEnv, Success(newArgLambdas)), argLambda) =>
        val (newEnv2, newRes) = instantiateS(argLambda.body)(newEnv)
        (newEnv2, newRes.map { b => newArgLambdas :+ TypeValueLambda(argLambda.argParams, b) })
      case ((newEnv, Failure(noType)), _)                =>
        (newEnv, noType.failure)
    }
    res.map {
      instantiatedArgLambdas =>
        val (env3, unallocatedParamAppIdx) = envSt.nextTypeParamAppIdxFromEnvironmentS(env2)
        val (env4, paramCount) = envSt.nextTypeParamFromEnvironmentS(env3)
        val (env5, res2) = envSt.appForGlobalTypeS(funLoc, instantiatedArgLambdas, paramCount, unallocatedParamAppIdx)(env4)
        res2.map {
          retTerm =>
            val (env6, allocatedParams) = envSt.allocatedTypeParamsFromEnvironmentS(env5)
            val (env7, res3) = allocateTypeValueTermParamsS(retTerm)(allocatedParams.map { p => p -> p }.toMap, unallocatedParamAppIdx)(env6)
            res3.map { 
              case (_, allocatedArgParams, _, retTerm2) =>
                val (env8, res4) = if(!allocatedArgParams.isEmpty)
                  envSt.inferTypeValueTermKindS(retTerm)(env7).mapElements(identity, _.map { _ => () })
                else
                  (env7, ().success)
                (env8, res4.map { _ => retTerm2 })
            }.valueOr { nt => (env7, nt.failure) }
        }.valueOr { nt => (env3, nt.failure) }
    }.valueOr { nt => (env2, nt.failure) }
  }
  
  private def matchesGlobalTypeAppWithTypeValueTermS[T, U, E](globalTypeApp1: GlobalTypeApp[T], term2: TypeValueTerm[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T], locEqual: Equal[T]): (E, Validation[NoType[T], U]) =
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
            appForGlobalTypeWithAllocatedTypeParamsS(loc1, args1)(env3) match {
              case (env4, Success(evaluatedTerm1)) =>
                appForGlobalTypeWithAllocatedTypeParamsS(loc2, args2)(env4) match {
                  case (env5, Success(evaluatedTerm2)) =>
                    envSt.withRecursionCheckingS(Set(loc1, loc2))(matchesTypeValueTermsS(evaluatedTerm1, evaluatedTerm2)(z)(f))(env5)
                  case (env5, Failure(noType))         =>
                    (env5, noType.failure)
                }
              case (env4, Failure(noType))         =>
                (env4, noType.failure)
            }
        }
      case (GlobalTypeApp(loc1, args1, _), _) =>
        appForGlobalTypeWithAllocatedTypeParamsS(loc1, args1)(env) match {
          case (env2, Success(evaluatedTerm1)) =>
            envSt.withRecursionCheckingS(Set(loc1))(matchesTypeValueTermsS(evaluatedTerm1, term2)(z)(f))(env2)
          case (env2, Failure(noType))         =>
            (env2, noType.failure)
        }
      case (_, _) =>
        unifier.mismatchedTermErrorS(env).mapElements(identity, _.failure)
    }
  
  private def matchesTypeParamAppWithTypeValueTermS[T, U, E](typeParamApp1: TypeParamApp[T], term2: TypeValueTerm[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T], locEqual: Equal[T]): (E, Validation[NoType[T], U]) =
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
      case (TypeParamApp(param1, args1, paramAppIdx1), typeConj2: TypeConjunction[T]) =>
        val (env2, _) = reverseTypeMatchingS(env)
        val (env3, res) = unifier.withSaveS(matchesTypeConjunctionWithTypeValueTermS(typeConj2, typeParamApp1)(z)(f))(env2)
        res.map { x => (env3, x.success) }.getOrElse {
          val (env4, res2) = if(args1.isEmpty)
            f(param1, Right(term2), z, env3)
          else
            mismatchedTypeValueTermNoTypeWithReturnKindS(typeParamApp1, term2)(env3).mapElements(identity, _.failure)
          addDelayedErrorsFromResultS(res2, Set(paramAppIdx1))(z)(env4)
        }
      case (TypeParamApp(param1, args1, paramAppIdx1), typeDisj2: TypeDisjunction[T]) =>
        val (env2, _) = reverseTypeMatchingS(env)
        val (env3, res) = unifier.withSaveS(matchesTypeDisjunctionWithTypeValueTermS(typeDisj2, typeParamApp1)(z)(f))(env2)
        res.map { x => (env3, x.success) }.getOrElse {
          val (env4, res2) = if(args1.isEmpty)
            f(param1, Right(term2), z, env3)
          else
            mismatchedTypeValueTermNoTypeWithReturnKindS(typeParamApp1, term2)(env3).mapElements(identity, _.failure)
          addDelayedErrorsFromResultS(res2, Set(paramAppIdx1))(z)(env4)
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
      case (TypeParamApp(param1, args1, paramAppIdx1), _) =>
        val (env2, noType) = mismatchedTypeValueTermNoTypeWithReturnKindS(typeParamApp1, term2)(env)
        addDelayedErrorsFromResultS(noType.failure, Set(paramAppIdx1))(z)(env2)
      case (_, _) =>
        unifier.mismatchedTermErrorS(env).mapElements(identity, _.failure)
    }
  
  private def instantiateAndSortTypeValueTermsS[T, E](term1: TypeValueTerm[T], terms2: Seq[TypeValueTerm[T]])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T], locEqual: Equal[T]): (E, Validation[NoType[T], (TypeValueTerm[T], Seq[TypeValueTerm[T]])]) = {
    val (env2, res) = partiallyInstantiateTypeValueTermS(term1)(env)
    res.map {
      instantiatedTerm1 =>
        val (env3, res2) = terms2.foldLeft((env2, Seq[TypeValueTerm[T]]().success[NoType[T]])) {
          case ((newEnv, Success(newTerms2)), term2) =>
            val (newEnv2, newRes) = partiallyInstantiateTypeValueTermS(term2)(newEnv)
            (newEnv2, newRes.map { newTerms2 :+ _ })
          case ((newEnv, Failure(noType)), _)        =>
            (newEnv, noType.failure)
        }
        res2.map {
          instantiatedTerms2 =>
            instantiatedTerm1 match {
              case TypeParamApp(_, args1, _) =>
                val (tmpTerms2, thirdTerms2) = instantiatedTerms2.partition { _.isTypeParamApp }
                val (firstTerms2, secondTerms2) = tmpTerms2.partition { 
                  case TypeParamApp(_, args2, _) => args1.size === args2.size
                  case _                         => false 
                }
                (env3, (instantiatedTerm1, firstTerms2 ++ secondTerms2 ++ thirdTerms2).success)
              case _                         =>
                val (firstTerms2, thirdTerms2) = instantiatedTerms2.partition { !_.isTypeParamApp }
                (env3, (instantiatedTerm1, firstTerms2 ++ thirdTerms2).success)
            }
        }.valueOr { nt => (env3, nt.failure) }
    }.valueOr { nt => (env2, nt.failure) }
  }
  
  private def checkTypeValueTermSubsetS[T, U, E](termSubset: Set[TypeValueTerm[T]], termSet: Set[TypeValueTerm[T]], areSwappedTerms: Boolean)(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T], locEqual: Equal[T]) = {
    val (env2, res) = termSubset.toSeq.foldLeft((env, (z, termSet.toSeq, Seq[TypeValueTerm[T]]()).success[NoType[T]])) {
      case ((newEnv, Success((x, firstTerms2, secondTerms2))), term1)  =>
        val (newEnv2, savedDelayedErrs) = envSt.delayedErrorsFromEnvironmentS(newEnv)
        val (newEnv3, newRes) = instantiateAndSortTypeValueTermsS(term1, firstTerms2)(newEnv2)
        newRes match {
          case Success((instantiatedTerm1, instantiatedFirstTerms2)) =>
            val terms2 = instantiatedFirstTerms2 ++ secondTerms2
            val (newEnv6, (newRes3, _)) = (0 until terms2.size).foldLeft(unifier.mismatchedTermErrorS(newEnv3).mapElements(identity, nt => (nt.failure[(U, Int)], false))) {
              case ((newEnv4, (Failure(_), _) | (Success(_), false)), i) =>
                val term2 = terms2(i)
                val (tmpTerm1, tmpTerm2) = if(areSwappedTerms) (term2, instantiatedTerm1) else (instantiatedTerm1, term2)
                val (newEnv5, (newRes2, areRestoredDelayedErrs)) = envSt.withDelayedErrorRestoringOrSavingS(savedDelayedErrs)(matchesTypeValueTermsS(tmpTerm1, tmpTerm2)(z)(f))(newEnv4)
                (newEnv5, (newRes2.map { (_, i) }, areRestoredDelayedErrs))
              case ((newEnv4, (newRes2, areRestoredDelayedErrs)), _)         =>
                (newEnv4, (newRes2, areRestoredDelayedErrs))
            }
            newRes3.map { 
              case (y, i) => 
                if(i < instantiatedFirstTerms2.size) 
                  (newEnv6, (y, instantiatedFirstTerms2.take(i) ++ instantiatedFirstTerms2.drop(i + 1), secondTerms2 :+ instantiatedFirstTerms2(i)).success)
                else
                  (newEnv6, (y, instantiatedFirstTerms2, secondTerms2).success)
            }.valueOr { nt => (newEnv6, nt.failure) }
          case Failure(noType) =>
            (newEnv3, noType.failure)
        }
      case ((newEnv, Failure(noType)), _) =>
        (newEnv, noType.failure)
    }
    (env2, res.map { _._1 })
  }
  
  private def matchesTypeConjunctionWithTypeValueTermS[T, U, E](typeConj1: TypeConjunction[T], term2: TypeValueTerm[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T], locEqual: Equal[T]) = {
    val typeConj2 = term2 match {
      case typeConj: TypeConjunction[T] => typeConj
      case _                            => TypeConjunction(Set(term2))
    }
    val (env2, typeMatching) = envSt.currentTypeMatchingFromEnvironmentS(env) 
    typeMatching match {
      case TypeMatching.Types             =>
        val (env3, _) = envSt.setCurrentTypeMatchingS(TypeMatching.SupertypeWithType)(env2)
        val (env4, res) = checkTypeValueTermSubsetS(typeConj1.terms, typeConj2.terms, false)(z)(f)(env3)
        res match {
          case Success(x)      =>
            val (env5, _) = envSt.setCurrentTypeMatchingS(TypeMatching.TypeWithSupertype)(env4)
            checkTypeValueTermSubsetS(typeConj2.terms, typeConj1.terms, true)(x)(f)(env5)
          case Failure(noType) =>
            (env4, noType.failure)
        }
      case TypeMatching.SupertypeWithType =>
        checkTypeValueTermSubsetS(typeConj1.terms, typeConj2.terms, false)(z)(f)(env2)
      case TypeMatching.TypeWithSupertype =>
        checkTypeValueTermSubsetS(typeConj2.terms, typeConj1.terms, true)(z)(f)(env2)
     }
  }

  private def matchesTypeDisjunctionWithTypeValueTermS[T, U, E](typeDisj1: TypeDisjunction[T], term2: TypeValueTerm[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T], locEqual: Equal[T]) = {
    val typeDisj2 = term2 match {
      case typeDisj: TypeDisjunction[T] => typeDisj
      case _                            => TypeDisjunction(Set(term2))
    }
    val (env2, typeMatching) = envSt.currentTypeMatchingFromEnvironmentS(env) 
    typeMatching match {
      case TypeMatching.Types             =>
        val (env3, _) = envSt.setCurrentTypeMatchingS(TypeMatching.SupertypeWithType)(env2)
        val (env4, res) = checkTypeValueTermSubsetS(typeDisj1.terms, typeDisj2.terms, false)(z)(f)(env3)
        res match {
          case Success(x)      =>
            val (env5, _) = envSt.setCurrentTypeMatchingS(TypeMatching.TypeWithSupertype)(env4)
            checkTypeValueTermSubsetS(typeDisj2.terms, typeDisj1.terms, true)(x)(f)(env5)
          case Failure(noType) =>
            (env3, noType.failure)
        }
      case TypeMatching.SupertypeWithType =>
        checkTypeValueTermSubsetS(typeDisj2.terms, typeDisj1.terms, true)(z)(f)(env2)
      case TypeMatching.TypeWithSupertype =>
        checkTypeValueTermSubsetS(typeDisj1.terms, typeDisj2.terms, false)(z)(f)(env2)
    }
  }
  
  private def matchesBuiltinTypeWithTypeValueTermS[T, U, E](term: TypeValueTerm[T])(z: U)(env: E)(implicit envSt: TypeInferenceEnvironmentState[E, T]) = {
    val (env2, retKindRes) = envSt.inferTypeValueTermKindS(term)(env)
    val (env3, unifiedRetKindRes) = envSt.unifyKindsS(InferredKind(Star(KindType, NoPosition)), retKindRes.valueOr { _.toNoKind })(env2)
    val (env4, _) = envSt.setReturnKindS(unifiedRetKindRes.valueOr { _.toNoKind })(env3)
    (env4, unifiedRetKindRes.map { _ => z })
  }
  
  def matchesTypeValueTermsS[T, U, E](term1: TypeValueTerm[T], term2: TypeValueTerm[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T], locEqual: Equal[T]): (E, Validation[NoType[T], U]) = {
    val (env2, typeMatching) = envSt.currentTypeMatchingFromEnvironmentS(env)
    val (env6, res) = partiallyInstantiateTypeValueTermS(term1)(env2) match {
      case (env3, Success(instantiatedTerm1)) =>
        partiallyInstantiateTypeValueTermS(term2)(env3) match {
          case (env4, Success(instantiatedTerm2)) =>
            (instantiatedTerm1, instantiatedTerm2) match {
              case (TupleType(args1), TupleType(args2)) =>
                matchesTypeValueTermListsWithReturnKindS(args1, args2)(z)(f)(env4)
              case (BuiltinType(bf1, args1), BuiltinType(bf2, args2)) if bf1 === bf2 =>
                matchesTypeValueTermListsWithReturnKindS(args1, args2)(z)(f)(env4)
              case (Unittype(loc1, args1, _), Unittype(loc2, args2, _)) if loc1 === loc2 =>
                matchesTypeValueTermListsWithReturnKindS(args1, args2)(z)(f)(env4)
              case (typeParamApp1: TypeParamApp[T], _) =>
                matchesTypeParamAppWithTypeValueTermS(typeParamApp1, instantiatedTerm2)(z)(f)(env4)
              case (_, typeParamApp2: TypeParamApp[T]) =>
                val (env5, _) = reverseTypeMatchingS(env4)
                matchesTypeParamAppWithTypeValueTermS(typeParamApp2, instantiatedTerm1)(z)(f)(env5)
              case (BuiltinType(TypeBuiltinFunction.Any, Seq()), _) if typeMatching === TypeMatching.SupertypeWithType =>
                matchesBuiltinTypeWithTypeValueTermS(instantiatedTerm2)(z)(env4)
              case (_, BuiltinType(TypeBuiltinFunction.Nothing, Seq())) if typeMatching === TypeMatching.SupertypeWithType =>
                matchesBuiltinTypeWithTypeValueTermS(instantiatedTerm1)(z)(env4)
              case (_, BuiltinType(TypeBuiltinFunction.Any, Seq())) if typeMatching === TypeMatching.TypeWithSupertype =>
                matchesBuiltinTypeWithTypeValueTermS(instantiatedTerm1)(z)(env4)
              case (BuiltinType(TypeBuiltinFunction.Nothing, Seq()), _) if typeMatching === TypeMatching.TypeWithSupertype =>
                matchesBuiltinTypeWithTypeValueTermS(instantiatedTerm2)(z)(env4)
              case (globalTypeApp1: GlobalTypeApp[T], _) =>
                matchesGlobalTypeAppWithTypeValueTermS(globalTypeApp1, instantiatedTerm2)(z)(f)(env4)
              case (_, globalTypeApp2: GlobalTypeApp[T]) =>
                val (env5, _) = reverseTypeMatchingS(env4)
                matchesGlobalTypeAppWithTypeValueTermS(globalTypeApp2, instantiatedTerm1)(z)(f)(env5)
              case (typeConj1: TypeConjunction[T], _) =>
                matchesTypeConjunctionWithTypeValueTermS(typeConj1, instantiatedTerm2)(z)(f)(env4)
              case (_, typeConj2: TypeConjunction[T]) =>
                val (env5, _) = reverseTypeMatchingS(env4)
                matchesTypeConjunctionWithTypeValueTermS(typeConj2, instantiatedTerm1)(z)(f)(env5)
              case (typeDisj1: TypeDisjunction[T], _) =>
                matchesTypeDisjunctionWithTypeValueTermS(typeDisj1, instantiatedTerm2)(z)(f)(env4)
              case (_, typeDisj2: TypeDisjunction[T]) =>
                val (env5, _) = reverseTypeMatchingS(env4)
                matchesTypeDisjunctionWithTypeValueTermS(typeDisj2, instantiatedTerm1)(z)(f)(env5)
              case (_, _) =>
                unifier.mismatchedTermErrorS(env4).mapElements(identity, _.failure)
            }
          case (env3, Failure(noType)) =>
            (env3, noType.failure)
        }
      case (env2, Failure(noType)) =>
        (env2, noType.failure)
    }
    envSt.setCurrentTypeMatchingS(typeMatching)(env6).mapElements(identity, _ => res)
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
            (env2, TypeParamApp(param2, args, paramAppIdx).success)
          case Success(Right(paramTerm)) =>
            paramTerm match {
              case TypeParamApp(param2, args2, _)   =>
                (env2, TypeParamApp(param2, args2 ++ args, paramAppIdx).success)
              case GlobalTypeApp(loc2, args2, sym2) =>
                (env2, GlobalTypeApp(loc2, args2 ++ args, sym2).success)
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
  
  private def unsafeAllocateTypeParamsFromTypeValueTermsS[T, E](terms: Seq[TypeValueTerm[T]])(allocatedParams: Map[Int, Int], unallocatedParamAppIdx: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T]) =
    terms.foldLeft((env, (allocatedParams, Set[Int](), Set[Int](), Seq[TypeValueTerm[T]]()).success[NoType[T]])) {
      case ((newEnv, Success((newAllocatedParams, newAllocatedArgParams, allocatedParamAppIdxs, newTerms))), term) =>
        val (newEnv2, newRes) = unsafeAllocateTypeValueTermParamsS(term)(newAllocatedParams, unallocatedParamAppIdx)(newEnv)
        (newEnv2, newRes.map { 
          _.mapElements(identity, newAllocatedArgParams | _, allocatedParamAppIdxs | _, newTerms :+ _)
        })
      case ((newEnv, Failure(noType)), _)                            =>
        (newEnv, noType.failure)
    }
    
  private def unsafeAllocateTypeParamsFromTypeValueLambdasS[T, E](lambdas: Seq[TypeValueLambda[T]])(allocatedParams: Map[Int, Int], unallocatedParamAppIdx: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T]) =
    lambdas.foldLeft((env, (allocatedParams, Set[Int](), Set[Int](), Seq[TypeValueLambda[T]]()).success[NoType[T]])) {
      case ((newEnv, Success((newAllocatedParams, newAllocatedArgParams, allocatedParamAppIdxs, newTerms))), lambda) =>
        val (newEnv2, newRes) = unsafeAllocateTypeValueLambdaParamsS(lambda)(newAllocatedParams, unallocatedParamAppIdx)(newEnv)
        (newEnv2, newRes.map {
          _.mapElements(identity, newAllocatedArgParams | _, allocatedParamAppIdxs | _, newTerms :+ _)
        })
      case ((newEnv, Failure(noType)), _)                              =>
        (newEnv, noType.failure)
    }

  private def unsafeAllocateTypeValueLambdaParamsS[T, E](lambda: TypeValueLambda[T])(allocatedParams: Map[Int, Int], unallocatedParamAppIdx: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T]) =
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
            (env3, res2.map { _.mapElements(_ -- argParams2, _ => argParams2.toSet, identity, TypeValueLambda(argParams2, _)) })
          case Failure(noType)                         =>
            (env2, noType.failure)
        }
    }

  private def unsafeAllocateTypeValueTermParamsS[T, E](term: TypeValueTerm[T])(allocatedParams: Map[Int, Int], unallocatedParamAppIdx: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T]): (E, Validation[NoType[T], (Map[Int, Int], Set[Int], Set[Int], TypeValueTerm[T])]) =
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

  def allocateTypeValueTermParamsS[T, E](term: TypeValueTerm[T])(allocatedParams: Map[Int, Int], unallocatedParamAppIdx: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T]) =
    unifier.withSaveS(unsafeAllocateTypeValueTermParamsS(term)(allocatedParams, unallocatedParamAppIdx))(env)
  
  def allocateTypeValueTermParamsWithKindsS[T, E](term: TypeValueTerm[T], kinds: Map[Int, InferredKind])(allocatedParams: Map[Int, Int], unallocatedParamAppIdx: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T]) =
    unifier.withSaveS {
      env2 =>
        val (env3, res) = unsafeAllocateTypeValueTermParamsS(term)(allocatedParams, unallocatedParamAppIdx)(env2)
        res.map {
          case (allocatedParams, allocatedArgParams, allocatedParamAppIdxs, term2) =>
            val (env4, res2) = kinds.foldLeft((env3, Map[Int, Kind]().success[NoType[T]])) {
              case ((newEnv, Success(newInferringKinds)), (param, kind)) =>
                val (newEnv2, inferringKindRes) = envSt.inferringKindFromInferredKindS(kind)(newEnv)
                (newEnv2, inferringKindRes.map { k => newInferringKinds + (param -> k) })
              case ((newEnv, Failure(noType)), _)                        =>
                (newEnv, noType.failure)
            }
            res2.map { 
              inferringKinds =>
                val (env5, _) = envSt.setTypeParamKindsS(inferringKinds)(env4)
                val (env6, res4) = if(!allocatedArgParams.isEmpty)
                  envSt.inferTypeValueTermKindS(term2)(env5)
                else
                  (env5, ().success)
                (env6, res4.map { _ => (allocatedParams, allocatedArgParams, allocatedParamAppIdxs, term2) })
            }.valueOr { nt => (env4, nt.failure) }
        }.valueOr { nt => (env3, nt.failure) }
    } (env)
  
  def checkDefinedTypeS[T, E](definedType: DefinedType[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int]) = {
    val params = definedType.args.flatMap { _.param }.toSet
    val (env2, res) = params.foldLeft((env, BitSet().success[NoType[T]])) {
      case ((newEnv, Success(rps)), p) => unifier.findRootParamS(p)(newEnv).mapElements(identity, _.map { rps + _ })
      case ((newEnv, Failure(nt)), _)  => (newEnv, nt.failure)
    }
    (env2, res.map {
      rootParams => if(rootParams.size === params.size) ().success else NoType.fromError[T](FatalError("parameters are distinct at defined type " + definedType, none, definedType.pos)).failure
    }.valueOr { _.failure })
  }
  
  def checkDefinedTypesS[T, E](definedTypes: Seq[DefinedType[T]])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int]): (E, Validation[NoType[T], Unit]) =
    definedTypes.foldLeft((env, ().success[NoType[T]])) {
      case ((newEnv, newRes), definedType) =>
        val (newEnv2, newRes2) = checkDefinedTypeS(definedType)(newEnv)
        (newEnv2, (newRes |@| newRes2) { (u, _) => u })
    }
  
  def normalizeTypeAppS[T, E](typeApp: TypeApp[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T]) = {
    val (env2, res) = envSt.inferTypeValueTermKindS(typeApp)(env)
    res.map {
      kind =>
        val (env3, res2) = envSt.maxArgCountFromKindS(kind)(env2)
        res2.map {
          maxArgCount =>
            if(maxArgCount > 0) {
              val (env4, res3) = (0 until maxArgCount).foldLeft((env3, Seq[(Int, Int)]().success[NoType[T]])) {
                case ((newEnv, Success(newPairs)), _) => 
                  val (newEnv2, newRes) = unifier.allocateParamS(newEnv)
                  newRes.map {
                    param =>
                      val (newEnv3, newRes2) = envSt.allocateTypeParamAppIdxS(newEnv2)
                      newRes2.map {
                        paramAppIdx => (newEnv3, (newPairs :+ (param, paramAppIdx)).success)
                      }.valueOr { nt =>  (newEnv3, nt.failure) }
                  }.valueOr { nt => (newEnv2, nt.failure) }
                case ((newEnv, Failure(nt)), _) =>
                  (newEnv, nt.failure)
              }
              res3.map {
                ps => 
                  val args2 = ps.map { p => TypeValueLambda[T](Nil, TypeParamApp(p._1, Nil, p._2)) }
                  val typeApp2 = typeApp.withArgs(typeApp.args ++ args2)
                  val (env5, res4) = envSt.inferTypeValueTermKindS(typeApp2)(env4)
                  (env5, res4.map { _ => typeApp2 })
              }.valueOr { nt => (env4, nt.failure) }
            } else
              (env3, typeApp.success)
        }.valueOr { nt => (env3, nt.failure) }
    }.valueOr { nt => (env2, nt.failure) }
  }
  
  def normalizeTypeValueTermS[T, E](term: TypeValueTerm[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T]) =
    partiallyInstantiateTypeValueTermS(term)(env) match {
      case (env2, Success(typeApp: TypeApp[T])) => normalizeTypeAppS(typeApp)(env2)
      case (env2, Success(_))                   => (env2, term.success)
      case (env2, Failure(noType))              => (env2, noType.failure)
    }
}