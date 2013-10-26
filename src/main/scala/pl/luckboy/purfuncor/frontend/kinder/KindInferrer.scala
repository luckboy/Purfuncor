package pl.luckboy.purfuncor.frontend.kinder
import scala.annotation.tailrec
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
      case (UninferredKind, _) | (_, UninferredKind) =>
        (env, NoKind.fromError(FatalError("uninferred kind", none, NoPosition)))
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
        (env, res.map { _._2.reverse.map { InferredKind(_) } })
      case InferringKind(kindTerm) =>
        val (env2, res) = (0 until argCount).foldLeft((env, (kindTerm, List[KindTerm[StarKindTerm[Int]]]()).success[NoKind])) {
          case ((newEnv, Success((Arrow(a, r, _), kts))), _)        =>
            (newEnv, (r, a :: kts).success)
          case ((newEnv, Success((Star(KindParam(p), _), kts))), _) =>
            val (newEnv2, rootParamRes) = unifier.findRootParamS(p)(newEnv)
            rootParamRes.map {
              rootParam =>
                val (newEnv3, optParamKindTerm) = unifier.getParamTermS(rootParam)(newEnv2)
                optParamKindTerm.map {
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
      case UninferredKind =>
        (env, NoKind.fromError(FatalError("uninferred kind", none, NoPosition)).failure)
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
        (env, res.map{ InferredKind(_) }.valueOr(identity))
      case InferringKind(kindTerm) =>
        val (env2, res) = (0 until argCount).foldLeft((env, kindTerm.success[NoKind])) {
          case ((newEnv, Success(Arrow(_, r, _))), _)        =>
            (newEnv, r.success)
          case ((newEnv, Success(Star(KindParam(p), _))), _) =>
            val (newEnv2, rootParamRes) = unifier.findRootParamS(p)(newEnv)
            rootParamRes.map {
              rootParam =>
                val (newEnv3, optParamKindTerm) = unifier.getParamTermS(rootParam)(newEnv2)
                optParamKindTerm.map {
                  case Arrow(_, r, _) => (newEnv3, r.success)
                  case _              => (newEnv3, NoKind.fromError(FatalError("kind term isn't arrow", none, NoPosition)).failure)
                }.getOrElse((newEnv3, NoKind.fromError(FatalError("kind term isn't arrow", none, NoPosition)).failure))
            }.valueOr { nk => (newEnv2, nk.failure) }
        }
        (env2, res.map(InferringKind).valueOr(identity))
      case UninferredKind =>
        (env, NoKind.fromError(FatalError("uninferred kind", none, NoPosition)))
      case noKind: NoKind =>
        (env, noKind)
    }
  
  def functionKind(argCount: Int) =
    InferredKind((0 until argCount).foldRight(Star(KindParam(argCount), NoPosition): KindTerm[StarKindTerm[Int]]) { (p, kt) => Arrow(Star(KindParam(p), NoPosition), kt, NoPosition) })
    
  private def noKindFromKind(kind: Kind) =
    kind match {
      case noKind: NoKind => noKind
      case _              => NoKind.fromError(FatalError("uninferred kind", none, NoPosition))
    }
    
  def functionKindFromKindsS[E](argKinds: Seq[Kind], retKind: Kind)(env: E)(implicit unifier: Unifier[NoKind, KindTerm[StarKindTerm[Int]], E, Int]) =
    argKinds.foldRight((env, retKind)) {
      case (InferredKind(argKindTerm), (newEnv, InferredKind(kindTerm)))                          =>
        val (newEnv2, argRes) = allocateKindTermParamsS(argKindTerm)(Map())(newEnv)
        val (newEnv3, retRes) = allocateKindTermParamsS(kindTerm)(Map())(newEnv2)
        (newEnv3, (argRes |@| retRes) { (p1, p2) => InferringKind(Arrow(p1._2, p2._2, NoPosition)) }.valueOr(identity))
      case (InferredKind(argKindTerm), (newEnv, InferringKind(inferringKindTerm)))                =>
        val (newEnv2, res) = allocateKindTermParamsS(argKindTerm)(Map())(newEnv)
        (newEnv2, res.map { p => InferringKind(Arrow(p._2, inferringKindTerm, NoPosition)) }.valueOr(identity))
      case (InferringKind(argInferringKindTerm), (newEnv, InferredKind(kindTerm)))                =>
        val (newEnv2, res) = allocateKindTermParamsS(kindTerm)(Map())(newEnv)
        (newEnv2, res.map { p => InferringKind(Arrow(argInferringKindTerm, p._2, NoPosition)) }.valueOr(identity))
      case (InferringKind(argInferringKindTerm), (newEnv, InferringKind(inferringKindTerm)))      =>
        (newEnv, InferringKind(Arrow(argInferringKindTerm, inferringKindTerm, NoPosition)))
      case (kind1 @ (UninferredKind | _: NoKind), (newEnv, kind2 @ (UninferredKind | _: NoKind))) =>
        (newEnv, noKindFromKind(kind1) |+| noKindFromKind(kind2))
      case (kind @ (UninferredKind | _: NoKind), (newEnv, _))                                     =>
        (newEnv, noKindFromKind(kind))
      case (_, (newEnv, kind @ (UninferredKind | _: NoKind)))                                     =>
        (newEnv, noKindFromKind(kind))
    }
  
  def instantiateKindMapS[T, E](kinds: Map[T, Kind])(env: E)(implicit unifier: Unifier[NoKind, KindTerm[StarKindTerm[Int]], E, Int]) =
    kinds.foldLeft((env, Map[T, Kind]().success[NoKind])) {
      case ((newEnv, Success(ks)), (l, k)) => 
        k.instantiatedKindS(newEnv) match {
          case (newEnv2, noKind: NoKind) => (newEnv2, noKind.failure)
          case (newEnv2, kind: Kind)     => (newEnv2, (ks + (l -> kind)).success)
        }
      case ((newEnv, Failure(nk)), _)      =>
        (newEnv, nk.failure)
    }
  
  @tailrec
  private def argCountFromInferredKindTerm(kindTerm: KindTerm[StarKindTerm[Int]])(argCount: Int): Int =
    kindTerm match {
      case Arrow(_, r, _) => argCountFromInferredKindTerm(r)(argCount + 1)
      case _              => argCount
    }
  
  @tailrec
  private def argCountFromInferringKindTermS[E](kindTerm: KindTerm[StarKindTerm[Int]])(argCount: Int)(env: E)(implicit unifier: Unifier[NoKind, KindTerm[StarKindTerm[Int]], E, Int]): (E, Validation[NoKind, Int]) =
    kindTerm match {
      case Arrow(_, r, _)        =>
        argCountFromInferringKindTermS(r)(argCount + 1)(env)
      case Star(KindParam(p), _) =>
        val (env2, rootParamRes) = unifier.findRootParamS(p)(env)
        rootParamRes match {
          case Success(rootParam) =>
            val (env3, optParamKindTerm) = unifier.getParamTermS(rootParam)(env2)
            optParamKindTerm match {
              case Some(paramKindTerm) => argCountFromInferringKindTermS(paramKindTerm)(argCount)(env3)
              case None                => (env3, argCount.success)
            }
          case Failure(noKind)    =>
            (env2, noKind.failure)
        }
      case Star(KindType, _)     =>
        (env, argCount.success)
    }
  
  def argCountFromKindS[E](kind: Kind)(env: E)(implicit unifier: Unifier[NoKind, KindTerm[StarKindTerm[Int]], E, Int]): (E, Validation[NoKind, Int]) =
    kind match {
      case InferredKind(kindTerm)  => (env, argCountFromInferredKindTerm(kindTerm)(0).success)
      case InferringKind(kindTerm) => argCountFromInferringKindTermS(kindTerm)(0)(env)
      case UninferredKind          => (env, NoKind.fromError(FatalError("uninferred kind", none, NoPosition)).failure)
      case noKind: NoKind          => (env, noKind.failure) 
    }
}