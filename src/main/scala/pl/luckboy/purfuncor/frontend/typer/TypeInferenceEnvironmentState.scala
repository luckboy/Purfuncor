package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend.kinder.Kind
import pl.luckboy.purfuncor.frontend.kinder.InferredKind
import pl.luckboy.purfuncor.frontend.kinder.InferringKind

trait TypeInferenceEnvironmentState[E, L, M]
{
  def appForGlobalTypeS(funLoc: M, argLambdas: Seq[TypeValueLambda[M]], paramCount: Int, paramAppIdx: Int)(env: E): (E, Validation[NoType[M], TypeValueTerm[M]])
  
  def inferTypeValueTermKindS(term: TypeValueTerm[M])(env: E): (E, Validation[NoType[M], Kind])
  
  def appKindS(funKind: Kind, argKinds: Seq[Kind])(env: E): (E, Validation[NoType[M], Kind])

  def appStarKindS(argKinds: Seq[Kind])(env: E): (E, Validation[NoType[M], Kind])
  
  def lambdaKindS(argKinds: Seq[Kind], retKind: Kind)(env: E): (E, Validation[NoType[M], Kind])
  
  def unifyKindsS(kind1: Kind, kind2: Kind)(env: E): (E, Validation[NoType[M], Kind])

  def returnKindFromEnvironmentS(env: E): (E, Kind)
  
  def setReturnKindS(kind: Kind)(env: E): (E, Unit)
  
  def withRecursionCheckingS[T](locs: Set[M])(f: E => (E, Validation[NoType[M], T]))(env: E): (E, Validation[NoType[M], T])
  
  def addDelayedErrorsS(errs: Map[Int, NoType[M]])(env: E): (E, Unit)
  
  def delayedErrorsFromEnvironmentS(env: E): (E, Map[Int, NoType[M]])
  
  def withDelayedErrorRestoringOrSavingS[T](errs: Map[Int, NoType[M]])(f: E => (E, Validation[NoType[M], T]))(env: E): (E, (Validation[NoType[M], T], Boolean))
  
  def allocateTypeParamAppIdxS(env: E): (E, Validation[NoType[M], Int])
  
  def nextTypeParamAppIdxFromEnvironmentS(env: E): (E, Int)
  
  def allocatedTypeParamsFromEnvironmentS(env: E): (E, Set[Int])
  
  def nextTypeParamFromEnvironmentS(env: E): (E, Int)
  
  def withTypeLambdaArgsS[T](argParams: Seq[Set[Int]])(f: E => (E, Validation[NoType[M], T]))(env: E): (E, Validation[NoType[M], T])
  
  def currentTypeMatchingFromEnvironmentS(env: E): (E, TypeMatching.Value)
  
  def setCurrentTypeMatchingS(typeMatching: TypeMatching.Value)(env: E): (E, Unit)
  
  def inferringKindFromKindS(kind: Kind)(env: E): (E, Validation[NoType[M], InferringKind])
  
  def setTypeParamKindsS(kinds: Map[Int, Kind])(env: E): (E, Unit)
  
  def argCountFromKindS(kind: Kind)(env: E): (E, Validation[NoType[M], Int])
  
  def inferredKindFromKindS(kind: Kind)(env: E): (E, Validation[NoType[M], InferredKind])
  
  def withClearS[T](f: E => (E, T))(env: E): (E, T)
  
  def withCombinatorLocationS[T](loc: Option[L])(f: E => (E, T))(env: E): (E, T)
  
  def instantiateTypesFromLambdaInfosS(env: E): (E, Validation[NoType[M], Unit])
  
  def instantiateTypeS(typ: Type[M])(env: E): (E, Type[M])
}