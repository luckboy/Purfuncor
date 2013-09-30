package pl.luckboy.purfuncor.frontend.typer
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
      
  def matchesTypeValueLambdasS[T, U, E](lambda1: TypeValueLambda[T], lambda2: TypeValueLambda[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T], locEqual: Equal[T]) =
    (lambda1, lambda2) match {
      case (TypeValueLambda(argParams1, body1), TypeValueLambda(argParams2, body2)) if argParams1.size === argParams2.size =>
        val argParams = argParams1.zip(argParams2).map { case (p1, p2) => Set(p1, p2) }
        envSt.withTypeLambdaArgsS(argParams)(matchesTypeValueTermsS(body1, body2)(z)(f))(env)
      case (TypeValueLambda(argParams1, GlobalTypeApp(loc1, args1, sym1)), TypeValueLambda(argParams2, body2)) if argParams1.size < argParams2.size =>
        val otherArgParams = argParams2.drop(argParams1.size)
        val argParams = argParams1.zip(argParams2).map { case (p1, p2) => Set(p1, p2) } ++ otherArgParams.map(Set(_))
        envSt.withTypeLambdaArgsS(argParams) {
          env2 =>
            typeValueLambdasFromTypeParamsS(otherArgParams)(env2) match {
              case (env3, Success(otherArgs)) =>
                val term1 = GlobalTypeApp(loc1, args1 ++ otherArgs, sym1)
                envSt.inferTypeValueTermKindS(term1)(env3) match {
                  case (env4, Success(_))      => matchesTypeValueTermsS(term1, body2)(z)(f)(env4)
                  case (env4, Failure(noType)) => (env4, noType.failure)
                }
              case (env3, Failure(noType))    => (env3, noType.failure)
            }
        } (env)
      case (TypeValueLambda(argParams1, TypeParamApp(param1, args1, paramAppIdx1)), TypeValueLambda(argParams2, body2)) if argParams1.size < argParams2.size =>
        val otherArgParams = argParams2.drop(argParams1.size)
        val argParams = argParams1.zip(argParams2).map { case (p1, p2) => Set(p1, p2) } ++ otherArgParams.map(Set(_))
        envSt.withTypeLambdaArgsS(argParams) {
          env2 =>
            typeValueLambdasFromTypeParamsS(otherArgParams)(env2) match {
              case (env3, Success(otherArgs)) => 
              	val term1 = TypeParamApp(param1, args1 ++ otherArgs, paramAppIdx1)
              	envSt.inferTypeValueTermKindS(term1)(env3) match {
              	  case (env4, Success(_))      => matchesTypeValueTermsS(term1, body2)(z)(f)(env4)
              	  case (env4, Failure(noType)) => (env4, noType.failure)
              	}
              case (env3, Failure(noType))    => (env3, noType.failure)
            }
        } (env)
      case (TypeValueLambda(argParams1, body1), TypeValueLambda(argParams2, GlobalTypeApp(loc2, args2, sym2))) if argParams1.size > argParams2.size =>
        val otherArgParams = argParams1.drop(argParams2.size)
        val argParams = argParams1.zip(argParams2).map { case (p1, p2) => Set(p1, p2) } ++ otherArgParams.map(Set(_))
        envSt.withTypeLambdaArgsS(argParams) {
          env2 =>
            typeValueLambdasFromTypeParamsS(otherArgParams)(env2) match {
              case (env3, Success(otherArgs)) =>
                val term2 = GlobalTypeApp(loc2, args2 ++ otherArgs, sym2)
                envSt.inferTypeValueTermKindS(term2)(env3) match {
                  case (env4, Success(_))      => matchesTypeValueTermsS(body1, term2)(z)(f)(env4)
                  case (env4, Failure(noType)) => (env4, noType.failure)
                }
              case (env3, Failure(noType))    => (env3, noType.failure)
            }
        } (env)
      case (TypeValueLambda(argParams1, body1), TypeValueLambda(argParams2, TypeParamApp(loc2, args2, paramAppIdx2))) if argParams1.size > argParams2.size =>
        val otherArgParams = argParams1.drop(argParams2.size)
        val argParams = argParams1.zip(argParams2).map { case (p1, p2) => Set(p1, p2) } ++ otherArgParams.map(Set(_))
        envSt.withTypeLambdaArgsS(argParams) {
          env2 =>
            typeValueLambdasFromTypeParamsS(otherArgParams)(env2) match {
              case (env3, Success(otherArgs)) =>
                val term2 = TypeParamApp(loc2, args2 ++ otherArgs, paramAppIdx2)
                envSt.inferTypeValueTermKindS(term2)(env3) match {
                  case (env4, Success(_))      => matchesTypeValueTermsS(body1, term2)(z)(f)(env4)
                  case (env4, Failure(noType)) => (env4, noType.failure)
                }
              case (env3, Failure(noType))    => (env3, noType.failure)
            }
        } (env)
      case (_, _) =>
        unifier.mismatchedTermErrorS(env).mapElements(identity, _.failure)
    }

  def instantiateFunctionTypeValueTermS[T, E](term: TypeValueTerm[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int]): (E, Validation[NoType[T], TypeValueTerm[T]]) =
    throw new UnsupportedOperationException
    
  def setReturnKindFromTypeValueTermsS[T, E](term1: TypeValueTerm[T], term2: TypeValueTerm[T])(env: E)(implicit envSt: TypeInferenceEnvironmentState[E, T]) = {
    val (env2, funKindRes1) = envSt.inferTypeValueTermKindS(term1)(env)
    val (env3, funKindRes2) = envSt.inferTypeValueTermKindS(term2)(env2)
    val (env4, unifiedFunKindRes) = envSt.unifyKindsS(funKindRes1.valueOr { _.toNoKind }, funKindRes2.valueOr { _.toNoKind })(env3)
    envSt.setReturnKindS(unifiedFunKindRes.valueOr { _.toNoKind })(env4)      
  }
    
  def mismatchedTypeValueTermErrorWithReturnKindS[T, E](term1: TypeValueTerm[T], term2: TypeValueTerm[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T]) = {
    val (env2, res) = setReturnKindFromTypeValueTermsS(term1, term2)(env)
    unifier.mismatchedTermErrorS(env2)
  }

  def addDelayedErrorsFromResultS[T, U, E](res: Validation[NoType[T], U], paramAppIdxs: Set[Int])(z: U)(env: E)(implicit envSt: TypeInferenceEnvironmentState[E, T]) =
    res.map { x => (env, x.success) }.valueOr {
      nt => 
        envSt.returnKindFromEnvironmentS(env) match {
          case (env2, noKind: NoKind) => (env2, NoType.fromNoKind[T](noKind).failure)
          case (env2, _)              => envSt.addDelayedErrorsS(paramAppIdxs.map { (_, nt) }.toMap)(env2).mapElements(identity, _ => z.success)
        }
    }

  def matchesTypeValueTermsS[T, U, E](term1: TypeValueTerm[T], term2: TypeValueTerm[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T], locEqual: Equal[T]): (E, Validation[NoType[T], U]) = {
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
              case (GlobalTypeApp(loc1, args1, sym1), GlobalTypeApp(loc2, args2, _)) =>
                val (env5, res) = if(loc1 === loc2) {
                  val (env4, funKindRes) = envSt.inferTypeValueTermKindS(GlobalTypeApp(loc1, Nil, sym1))(env3)
                  unifier.withSaveS {
                    matchesTypeValueLambdaListsWithReturnKindS(args1, args2, funKindRes.valueOr { _.toNoKind })(z)(f)(_)
                  } (env4)
                } else
                  unifier.mismatchedTermErrorS(env3).mapElements(identity, _.failure)
                res match {
                  case Success(x) =>
                    (env5, x.success)
                  case Failure(_) =>
                    envSt.appForGlobalTypeS(loc1, args1)(env5) match {
                      case (env6, Success(evaluatedTerm1)) =>
                        envSt.appForGlobalTypeS(loc2, args2)(env6) match {
                          case (env7, Success(evaluatedTerm2)) =>
                            envSt.withRecursionCheckS(Set(loc1, loc2))(matchesTypeValueTermsS(evaluatedTerm1, evaluatedTerm2)(z)(f))(env7)
                          case (env7, Failure(noType))         =>
                            (env7, noType.failure)
                        }
                      case (env6, Failure(noType))         =>
                        (env6, noType.failure)
                    }
                }
              case (TypeParamApp(param1, Seq(), paramAppIdx1), TypeParamApp(param2, Seq(), paramAppIdx2)) =>
                val (env4, res) = f(param1, Left(param2), z, env3)
                addDelayedErrorsFromResultS(res, Set(paramAppIdx1, paramAppIdx2))(z)(env4)
              case (TypeParamApp(param1, args1, paramAppIdx1), TypeParamApp(param2, args2, paramAppIdx2)) if args1.size === args2.size =>
                val (param, paramOrTerm, tmpArgs1, tmpArgs2) = if(args1.size === args2.size)
                  (param1, Left(param2), args1, args2)
                else if(args1.size < args2.size)
                  (param1, Right(TypeParamApp(param2, args2.take(args2.size - args1.size), paramAppIdx2)), args1, args2.drop(args2.size - args1.size))
                else
                  (param2, Right(TypeParamApp(param1, args1.take(args1.size - args2.size), paramAppIdx1)),  args2, args1.drop(args1.size - args2.size))
                val (env10, res) = unifier.withSaveS {
                  env4 =>
                    f(param, paramOrTerm, z, env4) match {
                      case (env5, Success(x))      =>
                        val (env6, funKind) = envSt.returnKindFromEnvironmentS(env5)
                        matchesTypeValueLambdaListsWithReturnKindS(tmpArgs1, tmpArgs2, funKind)(x)(f)(env6)
                      case (env5, Failure(noType)) =>
                        (setReturnKindFromTypeValueTermsS(instantiatedTerm1, instantiatedTerm2)(env5)._1, noType.failure)
                    }
                } (env3)
                addDelayedErrorsFromResultS(res, Set(paramAppIdx1, paramAppIdx2))(z)(env10)
              case (TypeParamApp(param1, Seq(), paramAppIdx1), _) =>
                val (env4, res) = f(param1, Right(term2), z, env3)
                addDelayedErrorsFromResultS(res, Set(paramAppIdx1))(z)(env4)
              case (TypeParamApp(param1, args1, paramAppIdx1), GlobalTypeApp(loc2, args2, sym2)) if args1.size <= args2.size =>
                val (env10, res) = unifier.withSaveS {
                  env4 =>
                    f(param1, Right(GlobalTypeApp(loc2, args2.take(args2.size - args1.size), sym2)), z, env4) match {
                      case (env5, Success(x))      =>
                        val (env6, funKind) = envSt.returnKindFromEnvironmentS(env5)
                        matchesTypeValueLambdaListsWithReturnKindS(args1, args2.drop(args2.size - args1.size), funKind)(x)(f)(env6)
                      case (env5, Failure(noType)) =>
                        (setReturnKindFromTypeValueTermsS(instantiatedTerm1, instantiatedTerm2)(env5)._1, noType.failure)
                    }
                } (env3)
                addDelayedErrorsFromResultS(res, Set(paramAppIdx1))(z)(env10)
              case (TypeParamApp(param1, args1, paramAppIdx1), _) =>
                val (env4, noType) = mismatchedTypeValueTermErrorWithReturnKindS(instantiatedTerm1, instantiatedTerm2)(env3)
                addDelayedErrorsFromResultS(noType.failure, Set(paramAppIdx1))(z)(env4)
              case (_, TypeParamApp(param2, Seq(), paramAppIdx2)) =>
                val (env4, res) = f(param2, Right(term1), z, env3)
                addDelayedErrorsFromResultS(res, Set(paramAppIdx2))(z)(env4)
              case (GlobalTypeApp(loc1, args1, sym1), TypeParamApp(param2, args2, paramAppIdx2)) if args1.size >= args2.size =>
                val (env10, res) = unifier.withSaveS {
                  env4 =>
                    f(param2, Right(GlobalTypeApp(loc1, args1.take(args1.size - args2.size), sym1)), z, env4) match {
                      case (env5, Success(x))      =>
                        val (env6, funKind) = envSt.returnKindFromEnvironmentS(env5)
                        matchesTypeValueLambdaListsWithReturnKindS(args1.drop(args1.size - args2.size), args2, funKind)(x)(f)(env6)
                      case (env5, Failure(noType)) =>
                        (setReturnKindFromTypeValueTermsS(instantiatedTerm1, instantiatedTerm2)(env5)._1, noType.failure)
                    }
                } (env3)
                addDelayedErrorsFromResultS(res, Set(paramAppIdx2))(z)(env10)
              case (_, TypeParamApp(param2, args2, paramAppIdx2)) =>
                val (env4, noType) = mismatchedTypeValueTermErrorWithReturnKindS(instantiatedTerm1, instantiatedTerm2)(env3)
                addDelayedErrorsFromResultS(noType.failure, Set(paramAppIdx2))(z)(env4)
              case (GlobalTypeApp(loc1, args1, _), _) =>
                envSt.appForGlobalTypeS(loc1, args1)(env3) match {
                  case (env4, Success(evaluatedTerm1)) =>
                    envSt.withRecursionCheckS(Set(loc1))(matchesTypeValueTermsS(evaluatedTerm1, instantiatedTerm2)(z)(f))(env4)
                  case (env4, Failure(noType))         =>
                    (env4, noType.failure)
                }
              case (_, GlobalTypeApp(loc2, args2, _)) =>
                envSt.appForGlobalTypeS(loc2, args2)(env3) match {
                  case (env4, Success(evaluatedTerm2)) =>
                    envSt.withRecursionCheckS(Set(loc2))(matchesTypeValueTermsS(instantiatedTerm1, evaluatedTerm2)(z)(f))(env4)
                  case (env4, Failure(noType))         =>
                    (env4, noType.failure)
                }
              case (_, _) =>
                unifier.mismatchedTermErrorS(env).mapElements(identity, _.failure)
            }
          case (env3, Failure(noType)) =>
            (env3, noType.failure)
        }
      case (env2, Failure(noType)) =>
        (env2, noType.failure)
    }
  }
}