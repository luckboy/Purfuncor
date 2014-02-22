/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.kinder.InferredKind
import pl.luckboy.purfuncor.frontend.kinder.TypeLambdaInfo
import pl.luckboy.purfuncor.common.Evaluator._
import pl.luckboy.purfuncor.frontend.KindTermUtils._
import TypeValueTermUtils._

case class DefinedType[T](args: List[DefinedTypeArg], term: TypeValueTerm[T], pos: Position)
{
  override def toString = {
    val argsWithOptIndexes = args.foldLeft((Vector[(DefinedTypeArg, Option[Int])](), 0)) {
      case ((as, i), a) => a.param.map { _ => (as :+ (a, some(i)), i + 1) }.getOrElse((as :+ (a, none), i)) 
    }._1
    val params = argsWithOptIndexes.flatMap { case (a, o) => o.flatMap { i => a.param.map { (_, i) } }.toSeq }.toMap
    val term2 = normalizeTypeParamsForParams(term, args.count { _.param.isDefined })(params)
    if(!args.isEmpty) {      
      "\\" + argsWithOptIndexes.map { 
        case (arg, optIdx) => 
          val s = arg.toStringForName(optIdx.map { i => "t" + (i + 1) }.getOrElse("_"))
          arg.kind.map { _ => "(" + s + ")" }.getOrElse(s)
      }.mkString(" ") + " => " + term2
    } else
      term2.toString
  }
}

object DefinedType
{
  private def appForDefinedTypeValueAndKindsS[T, U, V, W, X, E](funValue: TypeValue[T, U, TypeLambdaInfo[V, W], X])(kinds: Seq[(Option[KindTerm[StarKindTerm[Int]]], InferredKind)])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, TypeLambdaInfo[V, W]], E, TypeValue[T, U, TypeLambdaInfo[V, W], X]], envSt: TypeEnvironmentState[E, T, TypeValue[T, U, TypeLambdaInfo[V, W], X]], argTabular: ArgTabular[TypeLambda[U, TypeLambdaInfo[V, W]], W]): (E, Validation[NoTypeValue[T, U, TypeLambdaInfo[V, W], X], (TypeValueTerm[T], Seq[(Option[KindTerm[StarKindTerm[Int]]], InferredKind)])]) =
    funValue match {
      case lambdaValue: TypeLambdaValue[T, U, TypeLambdaInfo[V, W], X] =>
        envSt.withTypeParamsS(funValue.argCount) {
          (newParam1, newParamN, newEnv) =>
            val (newEnv2, paramAppIdx) = envSt.currentTypeParamAppIdxFromEnvironmentS(newEnv)
            val paramValues = (newParam1 until newParamN).map { i => EvaluatedTypeValue[T, U, TypeLambdaInfo[V, W], X](TypeParamApp(i, Nil, paramAppIdx)) }
            val definedKinds = lambdaValue.lambda.args.map { _.kind.map(intKindTermFromKindTerm) }.list
            val lambdaInfo = lambdaValue.lambda.lambdaInfo
            val newRes = argTabular.getArgLocationsFromTable(lambdaValue.lambda).zip(definedKinds).foldLeft(Vector[InferredKind]().success[NoTypeValue[T, U, TypeLambdaInfo[V, W], X]]) {
              case (Success(ks), (Some(l), _)) =>
                lambdaInfo.kindTable.kinds.get(l).map { k => (ks :+ k).success }.getOrElse {
                  NoTypeValue.fromError[T, U, TypeLambdaInfo[V, W], X](FatalError("not found kind at local kind table", none, NoPosition)).failure
                }
              case (Success(ks), (None, kt))   =>
                (ks :+ kt.map { InferredKind(_) }.getOrElse(InferredKind(Star(KindParam(0), NoPosition)))).success
              case (Failure(nv), _)            =>
                nv.failure
            }
            newRes.map {
              inferredKinds =>
                val newKinds = kinds ++ definedKinds.zip(inferredKinds)
                envSt.withClearS(appS(funValue, paramValues))(newEnv2) match {
                  case (newEnv3, noValue: NoTypeValue[T, U, TypeLambdaInfo[V, W], X]) =>
                    (newEnv3, noValue.failure)
                  case (newEnv3, newRetValue)                                         =>
                    appForDefinedTypeValueAndKindsS(newRetValue)(newKinds)(newEnv3)
                }
            }.valueOr { nv => (newEnv, nv.failure) }
        } (env)
      case EvaluatedTypeValue(term) =>
        (env, (term, kinds).success[NoTypeValue[T, U, TypeLambdaInfo[V, W], X]])
      case noValue: NoTypeValue[T, U, TypeLambdaInfo[V, W], X] =>
        (env, noValue.failure) 
      case _ =>
        (env, NoTypeValue.fromError[T, U, TypeLambdaInfo[V, W], X](FatalError("type value isn't type lambda value", none, NoPosition)).failure)
    }
  
  def evaluateDefinedTypeTermS[T, U, V, W, X, E](term: Term[TypeSimpleTerm[U, TypeLambdaInfo[V, W]]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, TypeLambdaInfo[V, W]], E, TypeValue[T, U, TypeLambdaInfo[V, W], X]], envSt: TypeEnvironmentState[E, T, TypeValue[T, U, TypeLambdaInfo[V, W], X]], argTabular: ArgTabular[TypeLambda[U, TypeLambdaInfo[V, W]], W]) = {
    val (env2, value) = evaluateS(term)(env)
    appForDefinedTypeValueAndKindsS(value)(Seq())(env2).mapElements(identity, _.map { _.mapElements(identity, _.toList) })
  }
  
  def evaluateDefinedTypeTerm[T, U, V, W, X, E](term: Term[TypeSimpleTerm[U, TypeLambdaInfo[V, W]]])(implicit eval: Evaluator[TypeSimpleTerm[U, TypeLambdaInfo[V, W]], E, TypeValue[T, U, TypeLambdaInfo[V, W], X]], envSt: TypeEnvironmentState[E, T, TypeValue[T, U, TypeLambdaInfo[V, W], X]], argTabular: ArgTabular[TypeLambda[U, TypeLambdaInfo[V, W]], W]) =
    State(evaluateDefinedTypeTermS[T, U, V, W, X, E](term))
    
  def fromInferringType[T](typ: InferringType[T]) =
    DefinedType(typeParamsFromTypeValueTerm(typ.typeValueTerm).toList.map { p => DefinedTypeArg(some(p), none) }, typ.typeValueTerm, NoPosition)
}

case class DefinedTypeArg(param: Option[Int], kind: Option[KindTerm[StarKindTerm[Int]]])
{
  def toStringForName(name: String) = param.map { _ => name }.getOrElse("_") + kind.map { kt => ": " + intKindTermShowing.stringFrom(kt) }.getOrElse("")
}
