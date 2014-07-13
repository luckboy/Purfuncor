/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer.range
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend.typer._
import pl.luckboy.purfuncor.frontend.kinder.Kind
import pl.luckboy.purfuncor.frontend.kinder.NoKind
import pl.luckboy.purfuncor.frontend.kinder.InferredKind
import pl.luckboy.purfuncor.frontend.kinder.InferringKind
import pl.luckboy.purfuncor.common.Inferrer._
import pl.luckboy.purfuncor.util.CollectionUtils._
import TypeValueTermKindInferrer._

object LogicalTypeValueTermKindInferrer
{
  private def inferTypeValueNodeKindsS[T, U, E](nodes: Iterable[TypeValueNode[T]], argMap: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]])(newKindMap: Map[TypeValueIdentity[T], Kind])(env: E)(implicit inferrer: Inferrer[U, E, Kind], envSt: KindInferrenceEnvironmentState[E, T]) =
    stFoldLeftValidationS(nodes)((newKindMap, Vector[Kind]()).success[NoKind]) {
      (pair, node, newEnv: E) =>
        val (newKindMap2, newNodes) = pair
        val (newEnv2, newRes) = inferTypeValueNodeKindS(node, argMap)(newKindMap2)(newEnv)
        (newEnv2, newRes.map { _.mapElements(identity, newNodes :+ _) })
    } (env)
  
  private def inferTypeValueNodeKindS[T, U, E](node: TypeValueNode[T], argMap: Map[TypeValueIdentity[T], Seq[TypeValueLambda[T]]])(newKindMap: Map[TypeValueIdentity[T], Kind])(env: E)(implicit inferrer: Inferrer[U, E, Kind], envSt: KindInferrenceEnvironmentState[E, T]): (E, Validation[NoKind, (Map[TypeValueIdentity[T], Kind], Kind)]) =
    node match {
      case TypeValueBranch(childs, tupleTypes, _) =>
        val (env2, res) = inferTypeValueNodeKindsS(childs, argMap)(newKindMap)(env)
        res match {
          case Success((newKindMap, kinds)) =>
            val (env3, res2) = inferTypeValueTermKindsS(tupleTypes)(env2)
            res2.map { 
              kinds2 => appStarKindS(kinds ++ kinds2)(env3) match {
                case (env2, noKind: NoKind) => (env2, noKind.failure)
                case (env2, kind)           => (env2, (newKindMap, kind).success)
              }
            }.valueOr { nk => (env3, nk.failure) }
          case Failure(noKind)              =>
            (env2, noKind.failure)
        }
      case leaf @ TypeValueLeaf(ident, _, _) =>
        newKindMap.get(ident) match {
          case Some(kind) =>
            (env, (newKindMap, kind).success)
          case None       =>
            argMap.get(ident).flatMap(leaf.typeValueTerm) match {
              case Some(term) =>
                inferTypeValueTermKindS(term)(env) match {
                  case (env2, noKind: NoKind) => (env2, noKind.failure)
                  case (env2, kind)           => (env2, (newKindMap + (ident -> kind), kind).success)
                }
              case None       =>
                (env, NoKind.fromError(FatalError("not found arguments", none, NoPosition)).failure)
            }
        }
      case GlobalTypeAppNode(loc, childs, tupleTypes, leafCount, sym) =>
        inferTypeValueNodeKindS(TypeValueLeaf(UnexpandedGlobalTypeAppIdentity(loc, sym), 0, leafCount), argMap)(newKindMap)(env)
    }
  
  def inferLogicalTypeValueTermKindS[T, U, E](term: LogicalTypeValueTerm[T])(env: E)(implicit inferrer: Inferrer[U, E, Kind], envSt: KindInferrenceEnvironmentState[E, T]): (E, Kind) =
    inferTypeValueNodeKindS(term.conjNode, term.args)(Map())(env).mapElements(identity, _.map { _._2 }.valueOr(identity))
}
