package pl.luckboy.purfuncor.frontend.typer
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.common.Unifier._
import pl.luckboy.purfuncor.frontend.typer.TypeValueTermUnifier._

object TypeInferrer
{
  def unifyTypesS[T, E](type1: Type[T], type2: Type[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T]) =
    (type1, type2) match {
      case (InferredType(typeValueTerm1, argKinds1), InferredType(typeValueTerm2, argKinds2)) =>
        val argKindMap1 = argKinds1.zipWithIndex.map { _.swap }.toMap
        val (env2, res1) = allocateTypeValueTermParamsWithKindsS(typeValueTerm1, argKindMap1)(Map(), 0)(env)
        val argKindMap2 = argKinds2.zipWithIndex.map { _.swap }.toMap
        val (env3, res2) = allocateTypeValueTermParamsWithKindsS(typeValueTerm2, argKindMap2)(Map(), 0)(env2)
        ((res1 |@| res2) {
          case ((_, _, _, inferringTypeValueTerm1), (_, _, _, inferringTypeValueTerm2)) =>
            val (env4, res2) = unifyS(inferringTypeValueTerm1, inferringTypeValueTerm2)(env3)
            (env4, res2.map { InferringType(_) }.valueOr(identity))
        }).valueOr { (env3, _) }
      case (InferredType(typeValueTerm1, argKinds1), InferringType(inferringTypeValueTerm2)) =>
        val argKindMap1 = argKinds1.zipWithIndex.map { _.swap }.toMap
        val (env2, res) = allocateTypeValueTermParamsWithKindsS(typeValueTerm1, argKindMap1)(Map(), 0)(env)
        res.map {
          case (_, _, _, inferringTypeValueTerm1) =>
            val (env3, res2) = unifyS(inferringTypeValueTerm1, inferringTypeValueTerm2)(env2)
            (env3, res2.map { InferringType(_) }.valueOr(identity))
        }.valueOr { (env2, _) }
      case (InferringType(inferringTypeValueTerm1), InferredType(typeValueTerm2, argKinds2)) =>
        val argKindMap2 = argKinds2.zipWithIndex.map { _.swap }.toMap
        val (env2, res) = allocateTypeValueTermParamsWithKindsS(typeValueTerm2, argKindMap2)(Map(), 0)(env)
        res.map {
          case (_, _, _, inferringTypeValueTerm2) =>
            val (env3, res2) = unifyS(inferringTypeValueTerm1, inferringTypeValueTerm2)(env2)
            (env3, res2.map { InferringType(_) }.valueOr(identity))
        }.valueOr { (env2, _) }
      case (InferringType(inferringTypeValueTerm1), InferringType(inferringTypeValueTerm2)) =>
        val (env2, res) = unifyS(inferringTypeValueTerm1, inferringTypeValueTerm2)(env)
        (env2, res.map { InferringType(_) }.valueOr(identity))
      case (UninferredType(), _) | (_, UninferredType()) =>
        (env, NoType.fromError(FatalError("uninferred type", none, NoPosition)))
      case (noType: NoType[T], _) =>
        (env, noType)
      case (_, noType: NoType[T]) =>
        (env, noType)
    }
}