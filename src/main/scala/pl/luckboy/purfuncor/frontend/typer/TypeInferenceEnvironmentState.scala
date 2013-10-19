package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend.kinder.Kind
import pl.luckboy.purfuncor.frontend.kinder.InferredKind
import pl.luckboy.purfuncor.frontend.kinder.InferringKind

trait TypeInferenceEnvironmentState[E, L]
{
  def appForGlobalTypeS(funLoc: L, argLambdas: Seq[TypeValueLambda[L]], paramCount: Int, paramAppIdx: Int)(env: E): (E, Validation[NoType[L], TypeValueTerm[L]])
  
  def inferTypeValueTermKindS(term: TypeValueTerm[L])(env: E): (E, Validation[NoType[L], Kind])
  
  def appKindS(funKind: Kind, argKinds: Seq[Kind])(env: E): (E, Validation[NoType[L], Kind])

  def appStarKindS(argKinds: Seq[Kind])(env: E): (E, Validation[NoType[L], Kind])
  
  def lambdaKindS(argKinds: Seq[Kind], retKind: Kind)(env: E): (E, Validation[NoType[L], Kind])
  
  def unifyKindsS(kind1: Kind, kind2: Kind)(env: E): (E, Validation[NoType[L], Kind])

  def returnKindFromEnvironmentS(env: E): (E, Kind)
  
  def setReturnKindS(kind: Kind)(env: E): (E, Unit)
  
  def withRecursionCheckingS[T](locs: Set[L])(f: E => (E, Validation[NoType[L], T]))(env: E): (E, Validation[NoType[L], T])
  
  def addDelayedErrorsS(errs: Map[Int, NoType[L]])(env: E): (E, Unit)
  
  def delayedErrorsFromEnvironmentS(env: E): (E, Map[Int, NoType[L]])
  
  def withDelayedErrorRestoringOrSavingS[T](errs: Map[Int, NoType[L]])(f: E => (E, Validation[NoType[L], T]))(env: E): (E, (Validation[NoType[L], T], Boolean))
  
  def allocateTypeParamAppIdxS(env: E): (E, Validation[NoType[L], Int])
  
  def nextTypeParamAppIdxFromEnvironmentS(env: E): (E, Int)
  
  def allocatedTypeParamsFromEnvironmentS(env: E): (E, Set[Int])
  
  def nextTypeParamFromEnvironmentS(env: E): (E, Int)
  
  def withTypeLambdaArgsS[T](argParams: Seq[Set[Int]])(f: E => (E, Validation[NoType[L], T]))(env: E): (E, Validation[NoType[L], T])
  
  def currentTypeMatchingFromEnvironmentS(env: E): (E, TypeMatching.Value)
  
  def setCurrentTypeMatchingS(typeMatching: TypeMatching.Value)(env: E): (E, Unit)
  
  def inferringKindFromInferredKindS(kind: InferredKind)(env: E): (E, Validation[NoType[L], InferringKind])
  
  def setTypeParamKindsS(kinds: Map[Int, Kind])(env: E): (E, Unit)
  
  def maxArgCountFromKindS(kind: Kind)(env: E): (E, Validation[NoType[L], Int])
}