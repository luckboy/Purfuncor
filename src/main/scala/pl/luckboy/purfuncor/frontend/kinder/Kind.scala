package pl.luckboy.purfuncor.frontend.kinder
import scala.collection.immutable.IntMap
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.common.Arrow
import pl.luckboy.purfuncor.common.Unifier._

sealed trait Kind
{
  def isNoKind = isInstanceOf[NoKind]  
  
  def instantiatedKindTermS[E](env: E)(implicit unifier: Unifier[NoKind, KindTerm[StarKindTerm[Int]], E, Int]) =
    this match {
      case noKind: NoKind          => (env, noKind.failure)
      case InferredKind(kindTerm)  => (env, kindTerm.success)
      case InferringKind(kindTerm) => instantiateS(kindTerm)(env)
      case UninferredKind          => (env, NoKind.fromError(FatalError("uninferred kind", none, NoPosition)).failure)
    }
}

case class NoKind(errs: NonEmptyList[AbstractError]) extends Kind

object NoKind
{
  def fromError(err: AbstractError) = NoKind(NonEmptyList(err))
  
  def fromErrors(errs: NonEmptyList[AbstractError]) = NoKind(errs)
}

case class InferredKind(kindTerm: KindTerm[StarKindTerm[Int]]) extends Kind

object InferredKind
{
  def fromTypeBuiltinFunction(bf: TypeBuiltinFunction.Value) =
    TypeBuiltinFunKindTerms.typeBuiltinFunKindTerms.get(bf).map { InferredKind(_) }.getOrElse(NoKind.fromError(FatalError("unsupported built-in type function", none, NoPosition)))
    
  def tupleTypeFunKind(n: Int) =
    InferredKind((0 until n).foldRight(Star(KindType, NoPosition): KindTerm[StarKindTerm[Int]]) { (_, kt) => Arrow(Star(KindType, NoPosition), kt, NoPosition) })
    
  def unittypeCombinatorKind(n: Int) = tupleTypeFunKind(n)
}

case class InferringKind(kindTerm: KindTerm[StarKindTerm[Int]]) extends Kind
case object UninferredKind extends Kind