/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.instant
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.typer.TypeBuiltinFunction
import pl.luckboy.purfuncor.frontend.typer.TypeValue
import pl.luckboy.purfuncor.frontend.typer.NoTypeValue
import pl.luckboy.purfuncor.frontend.typer.TypeValueTerm
import pl.luckboy.purfuncor.frontend.typer.TupleType
import pl.luckboy.purfuncor.frontend.typer.FieldType
import pl.luckboy.purfuncor.frontend.typer.BuiltinType
import pl.luckboy.purfuncor.frontend.typer.Unittype
import pl.luckboy.purfuncor.frontend.typer.TypeApp
import pl.luckboy.purfuncor.frontend.typer.GlobalTypeApp
import pl.luckboy.purfuncor.frontend.typer.TypeParamApp
import pl.luckboy.purfuncor.frontend.typer.TypeConjunction
import pl.luckboy.purfuncor.frontend.typer.TypeDisjunction
import pl.luckboy.purfuncor.frontend.typer.TypeValueLambda
import pl.luckboy.purfuncor.frontend.typer.TypeIdentity
import pl.luckboy.purfuncor.frontend.typer.TypeValueTermIdentity
import pl.luckboy.purfuncor.frontend.typer.TupleTypeIdentity
import pl.luckboy.purfuncor.frontend.typer.FieldTypeIdentity
import pl.luckboy.purfuncor.frontend.typer.BuiltinTypeIdentity
import pl.luckboy.purfuncor.frontend.typer.UnittypeIdentity
import pl.luckboy.purfuncor.frontend.typer.TypeParamAppIdentity
import pl.luckboy.purfuncor.frontend.typer.TypeValueTermUtils._

object TypeValueTermUtils
{
  def typeValueTermIdentitiesFromTypeValueTerm[T](term: TypeValueTerm[T]): Set[TypeValueTermIdentity[T]] =
    term match {
      case TupleType(_)           => Set(TupleTypeIdentity)
      case FieldType(i, _)        => Set(FieldTypeIdentity(i))
      case BuiltinType(bf, _)     => Set(BuiltinTypeIdentity(bf))
      case Unittype(loc, _, _)    => Set(UnittypeIdentity(loc))
      case TypeParamApp(_, _, _)  => Set(TypeParamAppIdentity)
      case TypeConjunction(terms) => terms.flatMap(typeValueTermIdentitiesFromTypeValueTerm) - BuiltinTypeIdentity(TypeBuiltinFunction.Any)
      case TypeDisjunction(terms) => terms.flatMap(typeValueTermIdentitiesFromTypeValueTerm) - BuiltinTypeIdentity(TypeBuiltinFunction.Nothing)
      case _                      => Set()
    }
  
  private def typeIdentityFromTypeValueTermsForMarkedLocsS[T, U, V, W, E](terms: Seq[TypeValueTerm[T]])(markedLocs: Set[T], nextParam: Int)(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]], envSt: typer.TypeEnvironmentState[E, T, TypeValue[T, U, V, W]], envSt2: TypeEnvironmentState[E, T]): (E, Validation[NoTypeValue[T, U, V, W], TypeIdentity[T]]) =
    terms.foldLeft((env, TypeIdentity[T](Set(), Vector()).success[NoTypeValue[T, U, V, W]])) {
      case ((newEnv, Success(TypeIdentity(idents, paramApps))), term) =>
        val (newEnv2, newRes) = typeIdentityFromTypeValueTermForMarkedLocsS(term)(markedLocs,nextParam)(newEnv)
        newRes.map {
          case TypeIdentity(idents2, paramApps2) => (newEnv2, TypeIdentity(idents | idents2, paramApps ++ paramApps2).success)
        }.valueOr { nv => (newEnv2, nv.failure) }
      case ((newEnv, Failure(noValue)), _)                         =>
        (newEnv, noValue.failure)
    }
  
  private def appForTypeValueLambda[T, U, V, W](funLambda: TypeValueLambda[T], argLambdas: Seq[TypeValueLambda[T]], nextArgParam: Int) =
    funLambda match {
      case TypeValueLambda(argParams, body) =>
        if(argParams.size <= argLambdas.size) {
          substituteTypeValueLambdas(body, argParams.zip(argLambdas).toMap, nextArgParam).map {
            case body2: TypeApp[T] =>
              body2.withArgs(argLambdas.drop(argParams.size)).success
            case body2             =>
              if(argParams.size === argLambdas.size)
                body2.success
              else
                NoTypeValue.fromError[T, U, V, W](FatalError("no applicable", none, NoPosition)).failure
          }.getOrElse {
            NoTypeValue.fromError[T, U, V, W](FatalError("can't substitute type value lambdas", none, NoPosition)).failure
          }
        } else
          NoTypeValue.fromError[T, U, V, W](FatalError("too more arguments of lambda", none, NoPosition)).failure
    }
  
  private def typeIdentityFromTypeValueTermForMarkedLocsS[T, U, V, W, E](term: TypeValueTerm[T])(markedLocs: Set[T], nextArgParam: Int)(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]], envSt: typer.TypeEnvironmentState[E, T, TypeValue[T, U, V, W]], envSt2: TypeEnvironmentState[E, T]): (E, Validation[NoTypeValue[T, U, V, W], TypeIdentity[T]]) =
    term match {
      case GlobalTypeApp(loc, args, _) =>
        if(!markedLocs.contains(loc)) {
          val (env2, optIdent) = envSt2.getTypeIdentityFromEnvironmentS(loc)(env)
          val (env5, typeIdentRes) = optIdent match {
            case Some(typeIdent) =>
              (env2, typeIdent.success)
            case None        =>
              val (env3, termRes) = envSt2.withPartialEvaluationS(false) {
                newEnv =>
                  envSt.withTypeParamsS(args.size) {
                    (newParam1, newParamN, newEnv2) =>
                      val (newEnv3, paramAppIdx) = envSt.currentTypeParamAppIdxFromEnvironmentS(newEnv2)
                      val argLambdas = (newParam1 until newParamN).map {
                        i => TypeValueLambda[T](Nil, TypeParamApp(i, Nil, paramAppIdx))
                      }
                      TypeValueTerm.appForGlobalTypeS(loc, argLambdas)(newEnv3)
                  } (newEnv)
              } (env2)
              val (env4, typeIdentRes2) = termRes.map {
                typeIdentityFromTypeValueTermForMarkedLocsS(_)(markedLocs, nextArgParam)(env3)
              }.valueOr { nv => (env3, nv.failure) }
              typeIdentRes2.map {
                typeIdent2 => 
                  envSt2.addTypeIdentityS(loc, typeIdent2)(env4).mapElements(identity, _ => typeIdent2.success)
              }.valueOr { nv => (env4, nv.failure) }
          }
          typeIdentRes.map {
            case typeIdent @ TypeIdentity(idents, paramApps) =>
              paramApps.foldLeft((env5, TypeIdentity[T](idents, Vector()).success[NoTypeValue[T, U, V, W]])) {
                case ((newEnv, Success(TypeIdentity(idents, paramApps))), TypeParamApp(param2, args2, _)) =>
                  args.lift(param2).map {
                    funLambda =>
                      appForTypeValueLambda(funLambda, args2, nextArgParam).map {
                        term2 =>
                          val (newEnv2, newRes) = typeIdentityFromTypeValueTermForMarkedLocsS(term2)(markedLocs, nextArgParam)(newEnv)
                          (newEnv2, newRes.map { case TypeIdentity(is, pas) => TypeIdentity(idents | is, paramApps ++ pas) })
                      }.valueOr { nv => (newEnv, nv.failure) }
                  }.getOrElse((newEnv, NoTypeValue.fromError[T, U, V, W](FatalError("index out of bound", none, NoPosition)).failure))
                case ((newEnv, Failure(noValue)), _)        =>
                  (newEnv, noValue.failure)
              }
          }.valueOr { nv => (env5, nv.failure) }
        } else
          (env, TypeIdentity(typeValueTermIdentitiesFromTypeValueTerm(term), Vector()).success)
      case paramApp: TypeParamApp[T]   =>
        (env, TypeIdentity(Set[TypeValueTermIdentity[T]](TypeParamAppIdentity), Vector(paramApp)).success)
      case TypeConjunction(terms)      =>
        val (env2, res) = typeIdentityFromTypeValueTermsForMarkedLocsS(terms.toSeq)(markedLocs, nextArgParam)(env)
        (env2, res.map {
          case TypeIdentity(is, ps) => TypeIdentity(is - BuiltinTypeIdentity(TypeBuiltinFunction.Any), ps)
        })
      case TypeDisjunction(terms)      =>
        val (env2, res) = typeIdentityFromTypeValueTermsForMarkedLocsS(terms.toSeq)(markedLocs, nextArgParam)(env)
        (env2, res.map {
          case TypeIdentity(is, ps) => TypeIdentity(is - BuiltinTypeIdentity(TypeBuiltinFunction.Nothing), ps)
        })
      case _ =>
        (env, TypeIdentity(typeValueTermIdentitiesFromTypeValueTerm(term), Vector()).success)
    }

  def typeIdentityFromTypeValueTermS[T, U, V, W, E](term: TypeValueTerm[T])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]], envSt: typer.TypeEnvironmentState[E, T, TypeValue[T, U, V, W]], envSt2: TypeEnvironmentState[E, T]) = {
    val params = typeParamsFromTypeValueTerm(term) | typeArgParamsFromTypeValueTerm(term)
    typeIdentityFromTypeValueTermForMarkedLocsS(term)(Set(), params.maximum.getOrElse(0))(env)
  }
  
  def typeIdentityFromTypeValueTerm[T, U, V, W, E](term: TypeValueTerm[T])(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]], envSt: typer.TypeEnvironmentState[E, T, TypeValue[T, U, V, W]], envSt2: TypeEnvironmentState[E, T]) =
    State(typeIdentityFromTypeValueTermS[T, U, V, W, E](term))
}
