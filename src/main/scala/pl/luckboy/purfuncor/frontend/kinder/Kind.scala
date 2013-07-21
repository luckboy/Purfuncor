package pl.luckboy.purfuncor.frontend.kinder
import scala.collection.immutable.IntMap
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.common.Unifier._

sealed trait Kind
{
  def isNoKind = isInstanceOf[NoKind]  
  
  def instantiatedKindTermS[E](env: E)(implicit unifier: Unifier[NoKind, KindTerm[StarKindTerm[Int]], E, Int]) =
    this match {
      case noKind: NoKind              => (env, noKind.failure)
      case InferredKind(kindTerm)      => (env, kindTerm.success)
      case InferringKind(kindTerm)     => instantiateS(kindTerm)(env)
      case RecursiveTypeKind(kindTerm) => (env, kindTerm.success)
    }
}

case class NoKind(errs: NonEmptyList[AbstractError]) extends Kind

object NoKind
{
  def fromError(err: AbstractError) = NoKind(NonEmptyList(err))
}

case class InferredKind(kindTerm: KindTerm[StarKindTerm[Int]]) extends Kind
case class InferringKind(kindTerm: KindTerm[StarKindTerm[Int]]) extends Kind
case class RecursiveTypeKind(kindTerm: KindTerm[StarKindTerm[Int]]) extends Kind