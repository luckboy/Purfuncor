package pl.luckboy.purfuncor.frontend.typer
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.kinder.Kind
import pl.luckboy.purfuncor.frontend.kinder.NoKind
import pl.luckboy.purfuncor.frontend.kinder.InferredKind
import pl.luckboy.purfuncor.common.Inferrer._

object TypeValueTermKindInferrer
{  
  def inferTypeValueTermKindS[T, U, E](term: TypeValueTerm[T])(env: E)(implicit inferrer: Inferrer[U, E, Kind], envSt: KindInferrenceEnvironmentState[E, T]): (E, Kind) =
    term match {
      case TupleType(args) =>
        val (env2, res) = inferTypeValueTermKindsS(args)(env)
        res.map { appStarKindS(_)(env2) }.valueOr { (env2, _) }
      case BuiltinType(bf, args) =>
        val (env2, res) = inferTypeValueTermKindsS(args)(env)
        res.map { appStarKindS(_)(env2) }.valueOr { (env2, _) }
      case Unittype(loc, args, _) =>
        val (env2, res) = inferTypeValueTermKindsS(args)(env)
        res.map { appStarKindS(_)(env2) }.valueOr { (env2, _) }
      case GlobalTypeApp(loc, args, _) =>
        val (env2, funKind) = envSt.globalTypeVarKindFromEnvironmentS(loc)(env)
        val (env3, res) = inferTypeValueLambdaKindsS(args)(env2)
        res.map { appInfoS(funKind, _)(env3) }.valueOr { (env3, _) }
      case TypeParamApp(param, args, _) =>
        val (env2, funKind) = envSt.typeParamKindFromEnvironmentS(param)(env)
        val (env3, res) = inferTypeValueLambdaKindsS(args)(env2)
        res.map { appInfoS(funKind, _)(env3) }.valueOr { (env3, _) }
      case TypeConjunction(terms) =>
        val (env2, res) = inferTypeValueTermKindsS(terms.toSeq)(env)
        res.map { appStarKindS(_)(env2) }.valueOr { (env2, _) }
      case TypeDisjunction(terms) =>
        val (env2, res) = inferTypeValueTermKindsS(terms.toSeq)(env)
        res.map { appStarKindS(_)(env2) }.valueOr { (env2, _) }
    }

  def inferTypeValueTermKindsS[T, U, E](terms: Seq[TypeValueTerm[T]])(env: E)(implicit inferrer: Inferrer[U, E, Kind], envSt: KindInferrenceEnvironmentState[E, T]) =
    terms.foldLeft((env, Seq[Kind]().success[NoKind])) {
      case ((newEnv, Success(kinds)), term) => 
        inferTypeValueTermKindS(term)(newEnv) match {
          case (newEnv2, noKind: NoKind) => (newEnv2, noKind.failure)
          case (newEnv2, kind)           => (newEnv2, (kinds :+ kind).success)
        }
      case ((newEnv, Failure(noKind)), _)     =>
        (newEnv, noKind.failure)
    }
  
  def inferTypeValueLambdaKindS[T, U, E](lambda: TypeValueLambda[T])(env: E)(implicit inferrer: Inferrer[U, E, Kind], envSt: KindInferrenceEnvironmentState[E, T]) = {
    val (env2, kind) = inferTypeValueTermKindS(lambda.body)(env)
    kind match {
      case noKind: NoKind =>
        (env2, noKind)
      case _              =>
       val (env3, argParamKinds) = lambda.argParams.foldLeft((env2, Seq[Kind]())) {
         case ((newEnv, ks), p) => envSt.typeParamKindFromEnvironmentS(p)(newEnv).mapElements(identity, ks :+ _)
       }
       val (env4, retKind) = appInfoS(inferrer.functionInfo(argParamKinds.size), argParamKinds)(env3)
       inferrer.unifyInfosS(kind, retKind)(env4)
    }
  }

  def inferTypeValueLambdaKindsS[T, U, E](lambdas: Seq[TypeValueLambda[T]])(env: E)(implicit inferrer: Inferrer[U, E, Kind], envSt: KindInferrenceEnvironmentState[E, T]) =
    lambdas.foldLeft((env, Seq[Kind]().success[NoKind])) {
      case ((newEnv, Success(kinds)), lambda) =>
        inferTypeValueLambdaKindS(lambda)(newEnv) match {
          case (newEnv2, noKind: NoKind) => (newEnv2, noKind.failure)
          case (newEnv2, kind)           => (newEnv2, (kinds :+ kind).success)
        }
      case ((newEnv, Failure(noKind)), _)        =>
        (newEnv, noKind.failure)
    }
 
  def appStarKindS[T, E](argKinds: Seq[Kind])(env: E)(implicit envSt: KindInferrenceEnvironmentState[E, T]) = {
    val (env2, res) = argKinds.foldLeft((env, ().success[NoKind])) {
      case ((newEnv, Success(_)), kind) =>
        envSt.unifyStarKindWithKindS(kind)(newEnv) match {
          case (newEnv2, noKind: NoKind) => (newEnv2, noKind.failure)
          case (newEnv2, _)              => (newEnv2, ().success)
        }
    }
    (env2, res.map { _ => InferredKind(Star(KindType, NoPosition)) }.valueOr(identity))
  }
}