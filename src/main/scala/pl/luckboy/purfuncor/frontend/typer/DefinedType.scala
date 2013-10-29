package pl.luckboy.purfuncor.frontend.typer
import scala.annotation.tailrec
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
    val term2 = normalizeTypeParamsForParams(term, args.size)(args.zipWithIndex.flatMap { case (a, i) => a.param.map { (_, i) } }.toMap)
    if(!args.isEmpty)
      "\\" +
      args.map {
        arg => 
          arg.kind.map { 
            kt => "(" + arg.param.map { _.toString }.getOrElse("_") + ": " + intKindTermShowing.stringFrom(kt) + ")"
          }.getOrElse(arg.param.map { _.toString }.getOrElse("_"))
      }.mkString(" ") + " => " +
      term2
    else
      term2.toString
  }
}

object DefinedType
{
  @tailrec
  private def appForDefinedTypeValueAndKindsS[T, U, V, W, X, E](funValue: TypeValue[T, U, TypeLambdaInfo[V, W], X])(kinds: Seq[(Option[KindTerm[StarKindTerm[Int]]], InferredKind)])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, TypeLambdaInfo[V, W]], E, TypeValue[T, U, TypeLambdaInfo[V, W], X]], envSt: TypeEnvironmentState[E, T, TypeValue[T, U, TypeLambdaInfo[V, W], X]], argTabular: ArgTabular[TypeLambda[U, TypeLambdaInfo[V, W]], W]): (E, Validation[NoTypeValue[T, U, TypeLambdaInfo[V, W], X], (TypeValueTerm[T], Seq[(Option[KindTerm[StarKindTerm[Int]]], InferredKind)])]) =
    funValue match {
      case lambdaValue: TypeLambdaValue[T, U, TypeLambdaInfo[V, W], X] =>
        val (env2, res) = envSt.withTypeParamsS(funValue.argCount) {
          (newParam1, newParamN, newEnv) =>
            val (newEnv2, paramAppIdx) = envSt.currentTypeParamAppIdxFromEnvironmentS(newEnv)
            val paramValues = (newParam1 until newParamN).map { i => EvaluatedTypeValue[T, U, TypeLambdaInfo[V, W], X](TypeParamApp(i, Nil, paramAppIdx)) }
            val definedKinds = lambdaValue.lambda.args.map { _.kind.map(intKindTermFromKindTerm) }.list
            val lambdaInfo = lambdaValue.lambda.lambdaInfo
            val newRes = argTabular.getArgLocationsFromTable(lambdaValue.lambda).zip(definedKinds).foldLeft(Seq[InferredKind]().success[NoTypeValue[T, U, TypeLambdaInfo[V, W], X]]) {
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
                appS(funValue, paramValues)(newEnv2) match {
                  case (newEnv2, noValue: NoTypeValue[T, U, TypeLambdaInfo[V, W], X]) =>
                    (newEnv2, noValue.failure)
                  case (newEnv2, newRetValue)                                         =>
                    (newEnv2, (newRetValue, newKinds).success)
                }
            }.valueOr { nv => (newEnv, nv.failure) }
        } (env)
        res match {
          case Success((retValue, kinds2)) => appForDefinedTypeValueAndKindsS(retValue)(kinds2)(env2)
          case Failure(noValue)            => (env2, noValue.failure)
        }
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
}

case class DefinedTypeArg(param: Option[Int], kind: Option[KindTerm[StarKindTerm[Int]]])