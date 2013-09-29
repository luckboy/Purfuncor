package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend.kinder.Kind

trait TypeInferenceEnvironmentState[E, L]
{
  def appForGlobalTypeS(funLoc: L, argLambdas: Seq[TypeValueLambda[L]])(env: E): (E, Validation[NoType[L], TypeValueTerm[L]])
  
  def inferTypeValueTermKindS(term: TypeValueTerm[L])(env: E): (E, Validation[NoType[L], Kind])
  
  def appKindS(funKind: Kind, argKinds: Seq[Kind])(env: E): (E, Validation[NoType[L], Kind])

  def appStarKindS(argKinds: Seq[Kind])(env: E): (E, Validation[NoType[L], Kind])
  
  def unifyKindsS(kind1: Kind, kind2: Kind)(env: E): (E, Validation[NoType[L], Kind])

  def returnKindFromEnvironmentS(env: E): (E, Kind)
  
  def setReturnKindS(kind: Kind)(env: E): (E, Unit)
  
  def withRecursionCheckS[T, U](locs: Set[L])(f: E => (E, Validation[T, U]))(env: E): (E, Validation[T, U])
  
  def addDelayedErrorsS(errs: Map[Int, NoType[L]])(env: E): (E, Unit)
  
  def allocateTypeParamAppIdx(env: E): (E, Validation[NoType[L], Int])
}