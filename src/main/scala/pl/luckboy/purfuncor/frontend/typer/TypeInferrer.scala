package pl.luckboy.purfuncor.frontend.typer
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.kinder.InferredKind
import pl.luckboy.purfuncor.common.Unifier._
import pl.luckboy.purfuncor.frontend.typer.TypeValueTermUnifier._
import pl.luckboy.purfuncor.frontend.typer.TypeValueTermUtils._

object TypeInferrer
{
  private def normalizeInferredTypeValueTermS[T, E](term: TypeValueTerm[T], kinds: Map[Int, InferredKind])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T]) = {
    val (env2, res ) = allocateTypeValueTermParamsWithKindsS(term, kinds)(Map(), 0)(env)
    res.map { 
      case (_, _, _, term2) => normalizeTypeValueTermS(term2)(env2)
    }.valueOr { nt => (env2, nt.failure) }
  }
  
  def unifyTypesS[T, E](type1: Type[T], type2: Type[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T]) =
    (type1, type2) match {
      case (InferredType(typeValueTerm1, argKinds1), InferredType(typeValueTerm2, argKinds2)) =>
        val argKindMap1 = argKinds1.zipWithIndex.map { _.swap }.toMap
        val (env2, res1) = normalizeInferredTypeValueTermS(typeValueTerm1, argKindMap1)(env)
        val argKindMap2 = argKinds2.zipWithIndex.map { _.swap }.toMap
        val (env3, res2) = normalizeInferredTypeValueTermS(typeValueTerm2, argKindMap2)(env2)
        ((res1 |@| res2) {
          case (inferringTypeValueTerm1, inferringTypeValueTerm2) =>
            val (env4, res2) = unifyS(inferringTypeValueTerm1, inferringTypeValueTerm2)(env3)
            (env4, res2.map { InferringType(_) }.valueOr(identity))
        }).valueOr { (env3, _) }
      case (InferredType(typeValueTerm1, argKinds1), InferringType(inferringTypeValueTerm2)) =>
        val argKindMap1 = argKinds1.zipWithIndex.map { _.swap }.toMap
        val (env2, res) = normalizeInferredTypeValueTermS(typeValueTerm1, argKindMap1)(env)
        res.map {
          case inferringTypeValueTerm1 =>
            val (env3, res2) = unifyS(inferringTypeValueTerm1, inferringTypeValueTerm2)(env2)
            (env3, res2.map { InferringType(_) }.valueOr(identity))
        }.valueOr { (env2, _) }
      case (InferringType(inferringTypeValueTerm1), InferredType(typeValueTerm2, argKinds2)) =>
        val argKindMap2 = argKinds2.zipWithIndex.map { _.swap }.toMap
        val (env2, res) = normalizeInferredTypeValueTermS(typeValueTerm2, argKindMap2)(env)
        res.map {
          case inferringTypeValueTerm2 =>
            val (env3, res2) = unifyS(inferringTypeValueTerm1, inferringTypeValueTerm2)(env2)
            (env3, res2.map { InferringType(_) }.valueOr(identity))
        }.valueOr { (env2, _) }
      case (InferringType(inferringTypeValueTerm1), InferringType(inferringTypeValueTerm2)) =>
        val (env2, res) = unifyS(inferringTypeValueTerm1, inferringTypeValueTerm2)(env)
        (env2, res.map { InferringType(_) }.valueOr(identity))
      case (UninferredType(), _) | (_, UninferredType()) =>
        (env, NoType.fromError[T](FatalError("uninferred type", none, NoPosition)))
      case (noType: NoType[T], _) =>
        (env, noType)
      case (_, noType: NoType[T]) =>
        (env, noType)
    }
  
  private def evaluateInferredTypeValueTermS[T, E](term: TypeValueTerm[T], paramCount: Int)(env: E)(implicit envSt: TypeInferenceEnvironmentState[E, T]) =
    term match {
      case GlobalTypeApp(loc, args, _) => envSt.appForGlobalTypeS(loc, args, paramCount, 0)(env)
      case _                           => (env, term.success)
    }
  
  private def evaluateInferringTypeValueTermS[T, E](term: TypeValueTerm[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T]) = {
    val (env2, res) = partiallyInstantiateTypeValueTermS(term)(env)
    res.map {
      case GlobalTypeApp(loc, args, _) => appForGlobalTypeWithAllocatedTypeParamsS(loc, args)(env2)
      case _                           => (env2, term.success)
    }.valueOr { nt => (env, nt.failure) }
  }
  
  private def argTypeValueTermFromTypeValueTermS1[T, E](term: TypeValueTerm[T], argCount: Int)(env: E)(evaluate: (TypeValueTerm[T], E) => (E, Validation[NoType[T], TypeValueTerm[T]])) = {
    val (env2, res) = (0 until argCount).foldLeft((env, (term, List[TypeValueTerm[T]]()).success[NoType[T]])) {
      case ((newEnv, Success((typeFun, newTypes))), _) =>
        val (newEnv2, newRes) = evaluate(typeFun, newEnv)
        (newEnv2, newRes.flatMap {
          case BuiltinType(TypeBuiltinFunction.Fun, Seq(arg, ret)) =>
            (ret, arg :: newTypes).success
          case _                                                   =>
            NoType.fromError[T](FatalError("type value term isn't function", none, NoPosition)).failure
        })
      case ((newEnv, Failure(noType)), _)          =>
        (newEnv, noType.failure)
    }
    (env, res.map { _._2.reverse })
  }
    
  def argTypesFromTypeS[T, E](typ: Type[T], argCount: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T]): (E, Validation[NoType[T], List[Type[T]]]) =
    typ match {
      case InferredType(typeValueTerm, kinds) =>
        val (env2, res) = argTypeValueTermFromTypeValueTermS1(typeValueTerm, argCount)(env)(evaluateInferredTypeValueTermS(_, kinds.size)(_))
        (env2, res.map { _.map { InferredType(_, kinds) } })
      case InferringType(typeValueTerm) =>
        val (env2, res) = argTypeValueTermFromTypeValueTermS1(typeValueTerm, argCount)(env)(evaluateInferringTypeValueTermS(_)(_))
        (env2, res.map { _.map { InferringType(_) } })
      case UninferredType() =>
        (env, NoType.fromError[T](FatalError("uninferred type", none, NoPosition)).failure)
      case noType: NoType[T] =>
        (env, noType.failure)
    }
  
  private def returnTypeValueTermFromTypeValueTermS1[T, E](term: TypeValueTerm[T], argCount: Int)(env: E)(evaluate: (TypeValueTerm[T], E) => (E, Validation[NoType[T], TypeValueTerm[T]])) =
    (0 until argCount).foldLeft((env, term.success[NoType[T]])) {
      case ((newEnv, Success(typeFun)), _)    =>
        val (newEnv2, newRes) = evaluate(typeFun, newEnv)
        (newEnv2, newRes.flatMap {
          case BuiltinType(TypeBuiltinFunction.Fun, Seq(_, ret)) =>
            ret.success
          case _                                                 =>
            NoType.fromError[T](FatalError("type value term isn't function", none, NoPosition)).failure
        })
      case ((newEnv, Failure(noType)), _) => 
        (newEnv, noType.failure)
    }
  
  def returnTypeFromTypeS[T, E](typ: Type[T], argCount: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T]) =
    typ match {
      case InferredType(typeValueTerm, kinds) =>
        val (env2, res) = returnTypeValueTermFromTypeValueTermS1(typeValueTerm, argCount)(env)(evaluateInferredTypeValueTermS(_, kinds.size)(_))
        (env2, res.map { InferredType(_, kinds) }.valueOr(identity))
      case InferringType(typeValueTerm) =>
        val (env2, res) = returnTypeValueTermFromTypeValueTermS1(typeValueTerm, argCount)(env)(evaluateInferringTypeValueTermS(_)(_))
        (env2, res.map { InferringType(_) }.valueOr(identity))
      case UninferredType() =>
        (env, NoType.fromError[T](FatalError("uninferred type", none, NoPosition)))
      case noType: NoType[T] =>
        (env, noType)
    }  
}