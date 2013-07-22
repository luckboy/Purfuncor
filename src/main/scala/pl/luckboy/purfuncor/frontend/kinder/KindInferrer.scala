package pl.luckboy.purfuncor.frontend.kinder
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.common.Arrow
import pl.luckboy.purfuncor.common.Unifier._
import KindTermUnifier._

object KindInferrer
{
  def unifyKindsS[E](kind1: Kind, kind2: Kind)(env: E)(implicit unifier: Unifier[NoKind, KindTerm[StarKindTerm[Int]], E, Int]) =
    (kind1, kind2) match {
      case (InferredKind(kindTerm1), InferredKind(kindTerm2)) =>
        val (env2, res1) = allocateKindTermParamsS(kindTerm1)(Map())(env)
        val (env3, res2) = allocateKindTermParamsS(kindTerm2)(Map())(env2)
        ((res1 |@| res2) {
          case ((_, inferringKindTerm1), (_, inferringKindTerm2)) =>
            val (env4, res3) = unifyS(inferringKindTerm1, inferringKindTerm2)(env3)
            (env4, res3.map(InferringKind).valueOr(identity))
        }).valueOr { (env3, _) }
      case (InferredKind(kindTerm1), InferringKind(inferringKindTerm2)) =>  
        val (env2, res) = allocateKindTermParamsS(kindTerm1)(Map())(env)
        res.map {
          case (_, inferringKindTerm1) =>
            val (env3, res2) = unifyS(inferringKindTerm1, inferringKindTerm2)(env2)
            (env3, res2.map(InferringKind).valueOr(identity))
        }.valueOr { (env2, _) }
      case (InferringKind(inferringKindTerm1), InferredKind(kindTerm2)) =>
        val (env2, res) = allocateKindTermParamsS(kindTerm2)(Map())(env)
        res.map {
          case (_, inferringKindTerm2) =>
            val (env3, res2) = unifyS(inferringKindTerm1, inferringKindTerm2)(env2)
            (env3, res2.map(InferringKind).valueOr(identity))
        }.valueOr { (env2, _) }
      case (InferringKind(inferringKindTerm1), InferringKind(inferringKindTerm2)) =>
        val (env2, res) = unifyS(inferringKindTerm1, inferringKindTerm2)(env)
        (env2, res.map(InferringKind).valueOr(identity))
      case (_: TypeRecCombinatorKind, _) | (_, _: TypeRecCombinatorKind) =>
        (env, NoKind.fromError(FatalError("kind of recursive type combinator", none, NoPosition)))
      case (noKind: NoKind, _) =>
        (env, noKind)
      case (_, noKind: NoKind) =>
        (env, noKind)
    }
  
  def argKindsFromKindS[E](kind: Kind, argCount: Int)(env: E)(implicit unifier: Unifier[NoKind, KindTerm[StarKindTerm[Int]], E, Int]) =
    kind match {
      case InferredKind(kindTerm) =>
        val res = (0 until argCount).foldLeft((kindTerm, List[KindTerm[StarKindTerm[Int]]]()).success[NoKind]) {
          case (Success((Arrow(a, r, _), kts)), _)  => (r, a :: kts).success
          case (Success((Star(_, _), kts)), _)      => NoKind.fromError(FatalError("kind term isn't arrow", none, NoPosition)).failure
          case (Failure(nk), _)                     => nk.failure
        }
        (env, res.map { _._2.reverse.map(InferredKind) })
      case InferringKind(kindTerm) =>
        val (env2, res) = (0 until argCount).foldLeft((env, (kindTerm, List[KindTerm[StarKindTerm[Int]]]()).success[NoKind])) {
          case ((newEnv, Success((Arrow(a, r, _), kts))), _)        =>
            (newEnv, (r, a :: kts).success)
          case ((newEnv, Success((Star(KindParam(p), _), kts))), _) =>
            val (newEnv2, rootParamRes) = unifier.findRootParamS(p)(newEnv)
            rootParamRes.map {
              rootParam =>
                val (newEnv3, paramKindTermOpt) = unifier.getParamTermS(rootParam)(newEnv2)
                paramKindTermOpt.map {
                  case Arrow(a, r, _) => (newEnv3, (r, a :: kts).success)
                  case _              => (newEnv3, NoKind.fromError(FatalError("kind term isn't arrow", none, NoPosition)).failure)
                }.getOrElse((newEnv3, NoKind.fromError(FatalError("kind term isn't arrow", none, NoPosition)).failure))
            }.valueOr { nk => (newEnv2, nk.failure) }
          case ((newEnv, Success((Star(KindType, _), kts))), _)     =>
            (newEnv, NoKind.fromError(FatalError("kind term isn't arrow", none, NoPosition)).failure)
          case ((newEnv, Failure(nk)), _)                           =>
            (newEnv, nk.failure)
        }
        (env2, res.map { _._2.reverse.map(InferringKind) })
      case TypeRecCombinatorKind(_) =>
        (env, NoKind.fromError(FatalError("kind of recursive type combinator", none, NoPosition)).failure)
      case noKind: NoKind =>
        (env, noKind.failure)
    }

  def returnKindFromKindS[E](kind: Kind, argCount: Int)(env: E)(implicit unifier: Unifier[NoKind, KindTerm[StarKindTerm[Int]], E, Int]) =
    kind match {
      case InferredKind(kindTerm) =>
        val res = (0 until argCount).foldLeft(kindTerm.success[NoKind]) {
          case (Success(Arrow(_, r, _)), _) => r.success
          case (Success(Star(_, _)), _)     => NoKind.fromError(FatalError("kind term isn't arrow", none, NoPosition)).failure
          case (Failure(nk), _)             => nk.failure
        }
        (env, res.map(InferredKind).valueOr(identity))
      case InferringKind(kindTerm) =>
        val (env2, res) = (0 until argCount).foldLeft((env, kindTerm.success[NoKind])) {
          case ((newEnv, Success(Arrow(_, r, _))), _)        =>
            (newEnv, r.success)
          case ((newEnv, Success(Star(KindParam(p), _))), _) =>
            val (newEnv2, rootParamRes) = unifier.findRootParamS(p)(newEnv)
            rootParamRes.map {
              rootParam =>
                val (newEnv3, paramKindTermOpt) = unifier.getParamTermS(rootParam)(newEnv2)
                paramKindTermOpt.map {
                  case Arrow(_, r, _) => (newEnv3, r.success)
                  case _              => (newEnv3, NoKind.fromError(FatalError("kind term isn't arrow", none, NoPosition)).failure)
                }.getOrElse((newEnv3, NoKind.fromError(FatalError("kind term isn't arrow", none, NoPosition)).failure))
            }.valueOr { nk => (newEnv2, nk.failure) }
        }
        (env2, res.map(InferringKind).valueOr(identity))
      case TypeRecCombinatorKind(_) =>
        (env, NoKind.fromError(FatalError("kind of recursive type combinator", none, NoPosition)))
      case noKind: NoKind =>
        (env, noKind)
    }
}