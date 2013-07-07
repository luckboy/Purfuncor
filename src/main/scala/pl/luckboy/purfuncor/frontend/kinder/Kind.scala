package pl.luckboy.purfuncor.frontend.kinder
import scala.collection.immutable.IntMap
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

sealed trait Kind

case class NoKind(errs: NonEmptyList[AbstractError]) extends Kind

object NoKind
{
  def fromError(err: AbstractError) = NoKind(NonEmptyList(err))
}

case class InferredKind(kindTerm: KindTerm[StarKindTerm[Int]]) extends Kind
case class InferringKind(param: Int) extends Kind
case class RecursiveKind(param: Int) extends Kind
case class TupleTypeFunKind(n: Int) extends Kind
case class TypeBuiltinFunKind(bf: TypeBuiltinFunction.Value) extends Kind