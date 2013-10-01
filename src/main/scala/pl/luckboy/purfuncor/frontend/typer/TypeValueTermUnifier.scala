package pl.luckboy.purfuncor.frontend.typer
import scala.annotation.tailrec
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.kinder.Kind
import pl.luckboy.purfuncor.frontend.kinder.NoKind
import pl.luckboy.purfuncor.frontend.kinder.InferredKind

object TypeValueTermUnifier
{
  def matchesTypeValueTermListsWithReturnKindS[T, U, E](terms1: Seq[TypeValueTerm[T]], terms2: Seq[TypeValueTerm[T]])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T], locEqual: Equal[T]) =
    if(terms1.size === terms2.size) {
      val (env2, res) = terms1.zip(terms2).foldLeft((env, (z, Seq[Kind]()).success[NoType[T]])) {
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
          val (env3, res2) = envSt.appStarKindS(argKinds)(env2)
          res2.map { envSt.setReturnKindS(_)(env3).mapElements(identity, _ => x.success) }
      }.valueOr { nt => (env2, nt.failure) }
    } else
      (env, NoType.fromError[T](FatalError("unequal list lengths", none, NoPosition)).failure)
  
  def matchesTypeValueLambdaListsWithReturnKindS[T, U, E](lambdas1: Seq[TypeValueLambda[T]], lambdas2: Seq[TypeValueLambda[T]], funKind: Kind)(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T], locEqual: Equal[T]) =
    if(lambdas1.size === lambdas2.size) {
      val (env2, res) = lambdas1.zip(lambdas2).foldLeft((env, (z, Seq[Kind]()).success[NoType[T]])) {
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
          val (env3, res2) = envSt.appKindS(funKind, argKinds)(env2)
          res2.map { envSt.setReturnKindS(_)(env3).mapElements(identity, _ => x.success) }
      }.valueOr { nt => (env2, nt.failure) }
    } else
      (env, NoType.fromError[T](FatalError("unequal list lengths", none, NoPosition)).failure)
  
  private def typeValueLambdasFromTypeParamsS[T, E](params: Seq[Int])(env: E)(implicit envSt: TypeInferenceEnvironmentState[E, T]): (E, Validation[NoType[T], Seq[TypeValueLambda[T]]]) =
    params.foldLeft((env, Seq[TypeValueLambda[T]]().success[NoType[T]])) {
      case ((newEnv, Success(lambdas)), param) =>
        val (newEnv2, res) = envSt.allocateTypeParamAppIdx(newEnv)
        res.map {
          pai => (newEnv2, (lambdas :+ TypeValueLambda(Nil, TypeParamApp(param, Nil, pai))).success)
        }.valueOr { nt => (newEnv2, nt.failure) }
      case ((newEnv, Failure(noType)), _)      =>
        (newEnv, noType.failure)
    }
  
  @tailrec
  def matchesTypeValueLambdasS[T, U, E](lambda1: TypeValueLambda[T], lambda2: TypeValueLambda[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T], locEqual: Equal[T]): (E, Validation[NoType[T], U]) =
    (lambda1, lambda2) match {
      case (TypeValueLambda(argParams1, body1), TypeValueLambda(argParams2, body2)) if argParams1.size === argParams2.size =>
        val argParams = argParams1.zip(argParams2).map { case (p1, p2) => Set(p1, p2) }
        envSt.withTypeLambdaArgsS(argParams)(matchesTypeValueTermsS(body1, body2)(z)(f))(env)
      case (TypeValueLambda(argParams1, typeApp1: TypeApp[T]), TypeValueLambda(argParams2, body2)) if argParams1.size < argParams2.size =>
        val otherArgParams = argParams2.drop(argParams1.size)
        val argParams = argParams1.zip(argParams2).map { case (p1, p2) => Set(p1, p2) } ++ otherArgParams.map(Set(_))
        envSt.withTypeLambdaArgsS(argParams) {
          env2 =>
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

  private def instantiateFunctionTypeValueTermS[T, E](term: TypeValueTerm[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int]): (E, Validation[NoType[T], TypeValueTerm[T]]) =
    term match {
      case TypeParamApp(param, args, paramAppIdx) =>
        val (env2, rootParamRes) = unifier.findRootParamS(param)(env)
        rootParamRes match {
          case Success(rootParam) =>
          	val (env3, optParamTerm) = unifier.getParamTermS(rootParam)(env2)
          	optParamTerm match {
          	  case Some(paramTerm) =>
          	    val (env4, res) = instantiateFunctionTypeValueTermS(paramTerm)(env3)
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
    
  private def mismatchedTypeValueTermErrorWithReturnKindS[T, E](term1: TypeValueTerm[T], term2: TypeValueTerm[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T]) = {
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
            envSt.appForGlobalTypeS(loc1, args1)(env3) match {
              case (env4, Success(evaluatedTerm1)) =>
                envSt.appForGlobalTypeS(loc2, args2)(env4) match {
                  case (env5, Success(evaluatedTerm2)) =>
                    envSt.withRecursionCheckS(Set(loc1, loc2))(matchesTypeValueTermsS(evaluatedTerm1, evaluatedTerm2)(z)(f))(env5)
                  case (env5, Failure(noType))         =>
                    (env5, noType.failure)
                }
              case (env4, Failure(noType))         =>
                (env4, noType.failure)
            }
        }
      case (GlobalTypeApp(loc1, args1, _), _) =>
        envSt.appForGlobalTypeS(loc1, args1)(env) match {
          case (env2, Success(evaluatedTerm1)) =>
            envSt.withRecursionCheckS(Set(loc1))(matchesTypeValueTermsS(evaluatedTerm1, term2)(z)(f))(env2)
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
        val (param, paramOrTerm, tmpArgs1, tmpArgs2) = if(args1.size === args2.size)
          (param1, Left(param2), args1, args2)
        else if(args1.size < args2.size)
          (param1, Right(TypeParamApp(param2, args2.take(args2.size - args1.size), paramAppIdx2)), args1, args2.drop(args2.size - args1.size))
        else
          (param2, Right(TypeParamApp(param1, args1.take(args1.size - args2.size), paramAppIdx1)),  args2, args1.drop(args1.size - args2.size))
        val (env5, res) = unifier.withSaveS {
          env2 =>
            f(param, paramOrTerm, z, env2) match {
              case (env3, Success(x))      =>
                val (env4, funKind) = envSt.returnKindFromEnvironmentS(env3)
                matchesTypeValueLambdaListsWithReturnKindS(tmpArgs1, tmpArgs2, funKind)(x)(f)(env4)
              case (env3, Failure(noType)) =>
                (setReturnKindFromTypeValueTermsS(typeParamApp1, term2)(env3)._1, noType.failure)
            }
        } (env)
        addDelayedErrorsFromResultS(res, Set(paramAppIdx1, paramAppIdx2))(z)(env5)
      case (TypeParamApp(param1, Seq(), paramAppIdx1), _) =>
        val (env2, res) = f(param1, Right(term2), z, env)
        addDelayedErrorsFromResultS(res, Set(paramAppIdx1))(z)(env2)
      case (TypeParamApp(param1, args1, paramAppIdx1), GlobalTypeApp(loc2, args2, sym2)) if args1.size <= args2.size =>
        val (env10, res) = unifier.withSaveS {
          env2 =>
            f(param1, Right(GlobalTypeApp(loc2, args2.take(args2.size - args1.size), sym2)), z, env2) match {
              case (env3, Success(x))      =>
                val (env4, funKind) = envSt.returnKindFromEnvironmentS(env3)
                matchesTypeValueLambdaListsWithReturnKindS(args1, args2.drop(args2.size - args1.size), funKind)(x)(f)(env4)
              case (env3, Failure(noType)) =>
                (setReturnKindFromTypeValueTermsS(typeParamApp1, term2)(env3)._1, noType.failure)
            }
        } (env)
        addDelayedErrorsFromResultS(res, Set(paramAppIdx1))(z)(env10)
      case (_, globalTypeApp2: GlobalTypeApp[T]) =>
        matchesGlobalTypeAppWithTypeValueTermS(globalTypeApp2, typeParamApp1)(z)(f)(env)
      case (TypeParamApp(param1, args1, paramAppIdx1), _) =>
        val (env2, noType) = mismatchedTypeValueTermErrorWithReturnKindS(typeParamApp1, term2)(env)
        addDelayedErrorsFromResultS(noType.failure, Set(paramAppIdx1))(z)(env2)
      case (_, _) =>
        unifier.mismatchedTermErrorS(env).mapElements(identity, _.failure)
    }

  def matchesTypeValueTermsS[T, U, E](term1: TypeValueTerm[T], term2: TypeValueTerm[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T], locEqual: Equal[T]): (E, Validation[NoType[T], U]) =
    instantiateFunctionTypeValueTermS(term1)(env) match {
      case (env2, Success(instantiatedTerm1)) =>
        instantiateFunctionTypeValueTermS(term2)(env2) match {
          case (env3, Success(instantiatedTerm2)) =>
            (instantiatedTerm1, instantiatedTerm2) match {
              case (TupleType(args1), TupleType(args2)) =>
                matchesTypeValueTermListsWithReturnKindS(args1, args2)(z)(f)(env3)
              case (BuiltinType(bf1, args1), BuiltinType(bf2, args2)) if bf1 === bf2 =>     
                matchesTypeValueTermListsWithReturnKindS(args1, args2)(z)(f)(env3)
              case (Unittype(loc1, args1, _), Unittype(loc2, args2, _)) if loc1 === loc2 =>
                matchesTypeValueTermListsWithReturnKindS(args1, args2)(z)(f)(env3)
              case (typeParamApp1: TypeParamApp[T], _) =>
                matchesTypeParamAppWithTypeValueTermS(typeParamApp1, instantiatedTerm2)(z)(f)(env3)
              case (_, typeParamApp2: TypeParamApp[T]) =>
                matchesTypeParamAppWithTypeValueTermS(typeParamApp2, instantiatedTerm1)(z)(f)(env3)
              case (globalTypeApp1: GlobalTypeApp[T], _) =>
                matchesGlobalTypeAppWithTypeValueTermS(globalTypeApp1, instantiatedTerm2)(z)(f)(env3)
              case (_, globalTypeApp2: GlobalTypeApp[T]) =>
                matchesGlobalTypeAppWithTypeValueTermS(globalTypeApp2, instantiatedTerm1)(z)(f)(env3)
              case (_, _) =>
                unifier.mismatchedTermErrorS(env3).mapElements(identity, _.failure)
            }
          case (env3, Failure(noType)) =>
            (env3, noType.failure)
        }
      case (env2, Failure(noType)) =>
        (env2, noType.failure)
    }
}