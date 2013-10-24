package pl.luckboy.purfuncor.frontend.typer
import scala.annotation.tailrec
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.kinder.InferredKind
import pl.luckboy.purfuncor.common.Evaluator._
import pl.luckboy.purfuncor.frontend.KindTermUtils._

case class DefinedType[T](args: List[DefinedTypeArg], term: TypeValueTerm[T], pos: Position)

object DefinedType
{
  @tailrec
  private def appForDefinedTypeValueAndKindsS[T, U, V, W, E](funValue: TypeValue[T, U, V, W])(kinds: Seq[Option[KindTerm[StarKindTerm[Int]]]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]], envSt: TypeEnvironmentState[E, T, TypeValue[T, U, V, W]]): (E, Validation[NoTypeValue[T, U, V, W], (TypeValueTerm[T], Seq[Option[KindTerm[StarKindTerm[Int]]]])]) =
    funValue match {
      case lambdaValue: TypeLambdaValue[T, U, V, W] =>
        val (env2, (retValue, kinds2)) = envSt.withTypeParamsS(funValue.argCount) {
          (newParam1, newParamN, newEnv) =>
            val (newEnv2, paramAppIdx) = envSt.currentTypeParamAppIdxFromEnvironmentS(newEnv)
            val paramValues = (newParam1 until newParamN).map { i => EvaluatedTypeValue[T, U, V, W](TypeParamApp(i, Nil, paramAppIdx)) }
            val newKinds = kinds ++ lambdaValue.lambda.args.map { _.kind.map(intKindTermFromKindTerm) }.list
            appS(funValue, paramValues)(newEnv2).mapElements(identity, (_, newKinds))
        } (env)
        appForDefinedTypeValueAndKindsS(retValue)(kinds2)(env2)
      case EvaluatedTypeValue(term)                 =>
        (env, (term, kinds).success[NoTypeValue[T, U, V, W]])
      case noValue: NoTypeValue[T, U, V, W]         =>
        (env, noValue.failure)
      case _                                        =>
        (env, NoTypeValue.fromError[T, U, V, W](FatalError("type value isn't type lambda value", none, NoPosition)).failure)
    }
  
  def evaluateDefinedTypeTermS[T, U, V, W, E](term: Term[TypeSimpleTerm[U, V]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]], envSt: TypeEnvironmentState[E, T, TypeValue[T, U, V, W]]) = {
    val (env2, value) = evaluateS(term)(env)
    appForDefinedTypeValueAndKindsS(value)(Seq())(env2)
  }
  
  def evaluateDefinedTypeTerm[T, U, V, W, E](term: Term[TypeSimpleTerm[U, V]])(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]], envSt: TypeEnvironmentState[E, T, TypeValue[T, U, V, W]]) =
    State(evaluateDefinedTypeTermS[T, U, V, W, E](term))
}

case class DefinedTypeArg(param: Option[Int], kind: Option[KindTerm[StarKindTerm[Int]]])