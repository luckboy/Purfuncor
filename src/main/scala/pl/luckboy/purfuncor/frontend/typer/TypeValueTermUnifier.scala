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
  def matchesTypeValueTermListsWithReturnKindS[T, U, E](terms1: Seq[TypeValueTerm[T]], terms2: Seq[TypeValueTerm[T]])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T], locEqual: Equal[T]): (E, Validation[NoType[T], U]) =
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
      (env, NoType.fromError(FatalError("unequal list lengths", none, NoPosition)).failure)
  
  def matchesTypeValueLambdaListsWithReturnKindS[T, U, E](lambdas1: Seq[TypeValueLambda[T]], lambdas2: Seq[TypeValueLambda[T]], funKind: Kind)(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T], locEqual: Equal[T]): (E, Validation[NoType[T], U]) =
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
      (env, NoType.fromError(FatalError("unequal list lengths", none, NoPosition)).failure)
  
  def matchesTypeValueLambdasS[T, U, E](lambda1: TypeValueLambda[T], lambda2: TypeValueLambda[T])(z: U)(f: (Int, Either[Int, TypeValueTerm[T]], U, E) => (E, Validation[NoType[T], U]))(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T], locEqual: Equal[T]): (E, Validation[NoType[T], U]) =
    throw new UnsupportedOperationException

  def instantiateFunctionTypeValueTermS[T, E](term: TypeValueTerm[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int]): (E, Validation[NoType[T], TypeValueTerm[T]]) =
    throw new UnsupportedOperationException
    
  private def addOrNotDelayedErrorParamsS[T, U, E](res: Validation[NoType[T], U], terms: List[TypeValueTerm[T]])(z: U)(env: E)(implicit envSt: TypeInferenceEnvironmentState[E, T]) = {
    val paramRes = terms.foldLeft(Set[Int]().success[NoType[T]]) {
      case (newRes, TypeParamApp(param, Seq())) => newRes.map { _ + param }
      case (newRes, _)                          => NoType.fromError[T](FatalError("no type parameter", none, NoPosition)).failure
    }
    paramRes.map {
      params =>
        res.map { x => (env, x.success) }.valueOr {
          nt => 
            envSt.returnKindFromEnvironmentS(env) match {
              case (env2, noKind: NoKind) => (env2, NoType.fromNoKind[T](noKind).failure)
              case (env2, _)              => envSt.addDelayedErrorParamsS(params.map { (_, nt) }.toMap)(env2).mapElements(identity, _ => z.success)
            }
        }
    }.valueOr { nt => (env, nt.failure)}
  }
  
  private def unifyReturnKindsS[T, E](kind1: Kind, argCount1: Int, kind2: Kind, argCount2: Int)(env: E)(implicit envSt: TypeInferenceEnvironmentState[E, T]) = {
    val (env2, retKind1) = envSt.returnKindFromKindS(kind1, argCount1)(env)
    val (env3, retKind2) = envSt.returnKindFromKindS(kind2, argCount1)(env2)
    envSt.unifyKindsS(retKind1, retKind2)(env3)
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
              case (GlobalTypeApp(loc1, args1, _), GlobalTypeApp(loc2, args2, _)) =>
                val (env7, res2) = if(loc1 === loc2) {
                  val (env4, funKind) = envSt.globalTypeVarKindFromEnvironmentS(loc1)(env3)
                  unifier.withSaveS {
                    matchesTypeValueLambdaListsWithReturnKindS(args1, args2, funKind)(z)(f)(_).mapElements(identity, res => (res, funKind).success)
                  } (env4)
                } else {
                  val (env4, funKind1) = envSt.globalTypeVarKindFromEnvironmentS(loc1)(env3)
                  val (env5, funKind2) = envSt.globalTypeVarKindFromEnvironmentS(loc2)(env4)
                  val (env6, res) = envSt.unifyKindsS(funKind1, funKind2)(env5)
                  res.map {
                    unifiedFunKind => unifier.mismatchedTermErrorS(env6).mapElements(identity, nt => (nt.failure, unifiedFunKind).success)
                  }.valueOr { nt => (env6, nt.failure) }
                }
                res2 match {
                  case Success((Success(x), _))       =>
                    (env7, x.success)
                  case Success((Failure(_), funKind)) =>
                    envSt.appForGlobalTypeS(loc1, args1)(env7) match {
                      case (env8, Success(evaluatedTerm1)) =>
                        envSt.appForGlobalTypeS(loc2, args2)(env8) match {
                          case (env9, Success(evaluatedTerm2)) =>
                            envSt.withRecursionCheckS(Set(loc1, loc2))(matchesTypeValueTermsS(evaluatedTerm1, evaluatedTerm2)(z)(f))(env9)
                          case (env9, Failure(noType))         =>
                             (env9, noType.failure)
                        }
                      case (env8, Failure(noType))         =>
                        (env8, noType.failure)
                    }
                  case Failure(noType)                =>
                    (env7, noType.failure)
                }
              case (TypeParamApp(param1, Seq()), TypeParamApp(param2, Seq())) =>
                val (env4, res) = f(param1, Left(param2), z, env3)
                addOrNotDelayedErrorParamsS(res, List(term1, term2))(z)(env4)
              case (TypeParamApp(param1, args1), TypeParamApp(param2, args2)) if args1.size === args2.size =>
                val (env10, res2) = unifier.withSaveS {
                  env4 =>
                    val (env7, (res, funKind, argCount)) = if(args1.size === args2.size) {
                      val (env5, tmpRes) = f(param1, Left(param2), z, env4)
                      val (env6, tmpFunKind) = envSt.returnKindFromEnvironmentS(env5)
                      (env6, (tmpRes.map { (_, args1, args2) }, tmpFunKind, args1.size))
                    } else if(args1.size < args2.size) {
                      val tmpArgCount = args2.size - args1.size
                      val (env5, tmpRes) = f(param1, Right(TypeParamApp(param2, args2.take(tmpArgCount))), z, env4)
                      val (env6, tmpFunKind) = envSt.returnKindFromEnvironmentS(env5)
                      (env6, (tmpRes.map { (_, args1, args2.drop(tmpArgCount)) }, tmpFunKind, tmpArgCount))
                    } else {
                      val tmpArgCount = args1.size - args2.size
                      val (env5, tmpRes) = f(param2, Right(TypeParamApp(param1, args1.take(args2.size))), z, env4)
                      val (env6, tmpFunKind) = envSt.returnKindFromEnvironmentS(env5)
                      (env6, (tmpRes.map { (_, args2, args1.drop(tmpArgCount)) }, tmpFunKind, tmpArgCount))
                    }
                    res match {
                      case Success((x, tmpArgs1, tmpArgs2)) =>
                        matchesTypeValueLambdaListsWithReturnKindS(tmpArgs1, tmpArgs2, funKind)(x)(f)(env7)
                      case Failure(noType)                  =>
                        val (env8, retKind) = envSt.returnKindFromKindS(funKind, argCount)(env7)
                        val (env9, _) = envSt.setReturnKindS(retKind)(env8)
                        (env9, noType.failure)
                    }
                } (env3)
                addOrNotDelayedErrorParamsS(res2, List(term1, term2))(z)(env10)
              case (TypeParamApp(param1, Seq()), _) =>
                val (env4, res) = f(param1, Right(term2), z, env3)
                addOrNotDelayedErrorParamsS(res, List(term1, term2))(z)(env4)
              case (TypeParamApp(param1, args1), GlobalTypeApp(loc2, args2, sym2)) if args1.size <= args2.size =>
                val (env10, res2) = unifier.withSaveS {
                  env4 =>
                    val argCount = args2.size - args1.size
                    val (env5, res) = f(param1, Right(GlobalTypeApp(loc2, args2.take(argCount), sym2)), z, env4)
                    val (env6, funKind) = envSt.returnKindFromEnvironmentS(env5)
                    res match {
                      case Success(x) =>
                        matchesTypeValueLambdaListsWithReturnKindS(args1, args2.drop(argCount), funKind)(x)(f)(env6)
                      case Failure(noType)                           =>
                        val (env7, retKind) = envSt.returnKindFromKindS(funKind, argCount)(env6)
                        val (env8, _) = envSt.setReturnKindS(retKind)(env7)
                        (env8, noType.failure)
                    }
                } (env3)
                addOrNotDelayedErrorParamsS(res2, List(term1, term2))(z)(env10)
              case (TypeParamApp(param1, args1), _) =>
                val (env4, funKind1) = envSt.paramKindFromEnvironmentS(param1)(env3)
                val (env5, retKind1) = envSt.returnKindFromKindS(funKind1, args1.size)(env4)
                val (env6, res) = envSt.inferTypeValueTermKindS(instantiatedTerm2)(env5)
                val (env7, res2) = envSt.unifyKindsS(retKind1, res.valueOr { _.toNoKind })(env6)
                val (env8, _) = envSt.setReturnKindS(res2.valueOr { _.toNoKind })(env7)
                val (env9, noType) = unifier.mismatchedTermErrorS(env8)
                addOrNotDelayedErrorParamsS(noType.failure, List(term1, term2))(z)(env9)
              case (_, TypeParamApp(param2, Seq())) =>
                val (env4, res) = f(param2, Right(term1), z, env3)
                addOrNotDelayedErrorParamsS(res, List(term1, term2))(z)(env4)
              case (GlobalTypeApp(loc1, args1, sym1), TypeParamApp(param2, args2)) if args1.size >= args2.size =>
                val (env10, res2) = unifier.withSaveS {
                  env4 =>
                    val argCount = args1.size - args2.size
                    val (env5, res) = f(param2, Right(GlobalTypeApp(loc1, args1.take(argCount), sym1)), z, env4)
                    val (env6, funKind) = envSt.returnKindFromEnvironmentS(env5)
                    res match {
                      case Success(x)      =>
                        matchesTypeValueLambdaListsWithReturnKindS(args1.drop(argCount), args2, funKind)(x)(f)(env6)
                      case Failure(noType) =>
                        val (env7, retKind) = envSt.returnKindFromKindS(funKind, argCount)(env6)
                        val (env8, _) = envSt.setReturnKindS(retKind)(env7)
                        (env8, noType.failure)

                    }
                } (env3)
                addOrNotDelayedErrorParamsS(res2, List(term1, term2))(z)(env10)
              case (_, TypeParamApp(param2, args2)) =>
                val (env4, res) = envSt.inferTypeValueTermKindS(instantiatedTerm1)(env3)
                val (env5, funKind2) = envSt.paramKindFromEnvironmentS(param2)(env4)
                val (env6, retKind2) = envSt.returnKindFromKindS(funKind2, args2.size)(env5)
                val (env7, res2) = envSt.unifyKindsS(res.valueOr { _.toNoKind }, retKind2)(env6)
                val (env8, _) = envSt.setReturnKindS(res2.valueOr { _.toNoKind })(env7)
                val (env9, noType) = unifier.mismatchedTermErrorS(env8)
                addOrNotDelayedErrorParamsS(noType.failure, List(term1, term2))(z)(env9)
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