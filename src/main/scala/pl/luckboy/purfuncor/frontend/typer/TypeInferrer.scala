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
    res.map { f => normalizeTypeValueTermS(f._4)(env2) }.valueOr { nt => (env2, nt.failure) }
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
  
  def normalizeTypeS[T, E](typ: Type[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T]) =
    typ match {
      case InferredType(typeApp: TypeApp[T], argKinds) =>
        val argKindMap = argKinds.zipWithIndex.map { _.swap }.toMap
        val (env2, res) = normalizeInferredTypeValueTermS(typeApp, argKindMap)(env)
        (env2, res.map { InferringType(_) }.valueOr(identity))
      case _                                           =>
        (env, typ)
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
    (env2, res.map { _._2.reverse })
  }
    
  def argTypesFromTypeS[T, E](typ: Type[T], argCount: Int)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T]): (E, Validation[NoType[T], List[Type[T]]]) =
    normalizeTypeS(typ)(env) match {
      case (env2, InferredType(typeValueTerm, argKinds)) =>
        val (env3, res) = argTypeValueTermFromTypeValueTermS1(typeValueTerm, argCount)(env2)(evaluateInferredTypeValueTermS(_, argKinds.size)(_))
        (env3, res.map { _.map { InferredType(_, argKinds) } })
      case (env2, InferringType(typeValueTerm)) =>
        val (env3, res) = argTypeValueTermFromTypeValueTermS1(typeValueTerm, argCount)(env2)(evaluateInferringTypeValueTermS(_)(_))
        (env3, res.map { _.map { InferringType(_) } })
      case (env2, UninferredType()) =>
        (env2, NoType.fromError[T](FatalError("uninferred type", none, NoPosition)).failure)
      case (env2, noType: NoType[T]) =>
        (env2, noType.failure)
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
    normalizeTypeS(typ)(env) match {
      case (env2, InferredType(typeValueTerm, argKinds)) =>
        val (env3, res) = returnTypeValueTermFromTypeValueTermS1(typeValueTerm, argCount)(env2)(evaluateInferredTypeValueTermS(_, argKinds.size)(_))
        (env3, res.map { InferredType(_, argKinds) }.valueOr(identity))
      case (env2, InferringType(typeValueTerm)) =>
        val (env3, res) = returnTypeValueTermFromTypeValueTermS1(typeValueTerm, argCount)(env2)(evaluateInferringTypeValueTermS(_)(_))
        (env3, res.map { InferringType(_) }.valueOr(identity))
      case (env2, UninferredType()) =>
        (env2, NoType.fromError[T](FatalError("uninferred type", none, NoPosition)))
      case (env2, noType: NoType[T]) =>
        (env2, noType)
    }
  
  def functionType[T](argCount: Int) = {
    val typeValueTerm = (0 until argCount).foldRight(TypeParamApp(argCount, Nil, 0): TypeValueTerm[T]) {
      (p, t) => BuiltinType(TypeBuiltinFunction.Fun, Seq(TypeParamApp(p, Nil, 0), t))
    }
    val argKinds = (0 to argCount).map { _ => InferredKind(Star(KindType, NoPosition)) }
    InferredType(typeValueTerm, argKinds)
  }
  
  private def noTypeFromType[T](typ: Type[T]) =
    typ match {
      case noType: NoType[T] => noType
      case _                 => NoType.fromError[T](FatalError("uninferred type", none, NoPosition))
    }
  
  def functionTypeFromTypesS[T, E](argTypes: Seq[Type[T]], retType: Type[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, T]) =
    argTypes.foldRight((env, retType)) {
      case (argType, (newEnv, newRetType)) =>
        val (newEnv2, argType2) = normalizeTypeS(argType)(newEnv)
        val (newEnv3, newRetType2) = normalizeTypeS(newRetType)(newEnv2)
        (argType2, newRetType2) match {
          case (InferredType(argTypeValueTerm, argArgKinds), InferredType(retTypeValueTerm, retArgKinds)) =>
            val argArgKindMap = argArgKinds.zipWithIndex.map { _.swap }.toMap
            val (newEnv4, argRes) = normalizeInferredTypeValueTermS(argTypeValueTerm, argArgKindMap)(newEnv3)
            val retArgKindMap = retArgKinds.zipWithIndex.map { _.swap }.toMap
            val (newEnv5, retRes) = normalizeInferredTypeValueTermS(retTypeValueTerm, retArgKindMap)(newEnv4)
            (argRes |@| retRes) {
              (argInferringTypeValueTerm, retInferringTypeValueTerm) =>
                (newEnv5, InferringType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argInferringTypeValueTerm, retInferringTypeValueTerm))))
            }.valueOr { (newEnv5, _) }
          case (InferredType(argTypeValueTerm, argArgKinds), InferringType(retInferringTypeValueTerm))    =>
            val argArgKindMap = argArgKinds.zipWithIndex.map { _.swap }.toMap
            val (newEnv4, res) = normalizeInferredTypeValueTermS(argTypeValueTerm, argArgKindMap)(newEnv3)
            res.map {
              argInferringTypeValueTerm =>
                (newEnv4, InferringType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argInferringTypeValueTerm, retInferringTypeValueTerm))))
            }.valueOr { (newEnv4, _) }
          case (InferringType(argInferringTypeValueTerm), InferredType(retTypeValueTerm, retArgKinds))    =>
            val retArgKindMap = retArgKinds.zipWithIndex.map { _.swap }.toMap
            val (newEnv4, res) = normalizeInferredTypeValueTermS(retTypeValueTerm, retArgKindMap)(newEnv3)
            res.map {
              retInferringTypeValueTerm =>
                (newEnv4, InferringType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argInferringTypeValueTerm, retInferringTypeValueTerm))))
            }.valueOr { (newEnv4, _) }
          case (InferringType(argInferringTypeValueTerm), InferringType(retInferringTypeValueTerm))       =>
            (newEnv3, InferringType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argInferringTypeValueTerm, retInferringTypeValueTerm))))
          case ((type1 @ (UninferredType() | _: NoType[T])), (type2 @ (UninferredType() | _: NoType[T]))) =>
            (newEnv3, noTypeFromType(type1) |+| noTypeFromType(type2))
          case ((typ @ (UninferredType() | _: NoType[T])), _)                                             =>
            (newEnv3, noTypeFromType(typ))
          case (_, (typ @ (UninferredType() | _: NoType[T])))                                             =>
            (newEnv3, noTypeFromType(typ))
        }
    }
}