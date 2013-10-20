package pl.luckboy.purfuncor.frontend.kinder
import scala.collection.immutable.IntMap
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.common.Arrow
import pl.luckboy.purfuncor.common.Unifier._
import pl.luckboy.purfuncor.frontend.KindTermUtils._
import pl.luckboy.purfuncor.frontend.kinder.KindTermUnifier._

sealed trait Kind
{
  def isNoKind = isInstanceOf[NoKind]
  
  def isInferringKind = isInstanceOf[InferringKind]
  
  def isUninferredKind =
    this match {
      case UninferredKind => true
      case _              => false
    }
  
  def instantiatedKindTermS[E](env: E)(implicit unifier: Unifier[NoKind, KindTerm[StarKindTerm[Int]], E, Int]) =
    this match {
      case noKind: NoKind          => (env, noKind.failure)
      case InferredKind(kindTerm)  => (env, kindTerm.success)
      case InferringKind(kindTerm) => instantiateS(kindTerm)(env).mapElements(identity, _.map(intKindTermFromKindTerm))
      case UninferredKind          => (env, NoKind.fromError(FatalError("uninferred kind", none, NoPosition)).failure)
    }
  
  def instantiatedKindS[E](env: E)(implicit unifier: Unifier[NoKind, KindTerm[StarKindTerm[Int]], E, Int]) =
    instantiatedKindTermS(env).mapElements(identity, _.map { InferredKind(_) }.valueOr(identity))
  
  def uninstantiatedKindTermS[E](env: E)(implicit unifier: Unifier[NoKind, KindTerm[StarKindTerm[Int]], E, Int]) =
    this match {
      case noKind: NoKind          => (env, noKind.failure)
      case InferredKind(kindTerm)  => allocateKindTermParamsS(kindTerm)(Map())(env).mapElements(identity, _.map { _._2 })
      case InferringKind(kindTerm) => (env, kindTerm.success)
      case UninferredKind          => (env, NoKind.fromError(FatalError("uninferred kind", none, NoPosition)).failure)
    }
  
  def uninstantiatedKindS[E](env: E)(implicit unifier: Unifier[NoKind, KindTerm[StarKindTerm[Int]], E, Int]) =
    uninstantiatedKindTermS(env).mapElements(identity, _.map { InferringKind(_) }.valueOr(identity))
  
  def withPos(pos: Position): Kind =
    this match {
      case noKind: NoKind =>
        NoKind(prevErrs = noKind.prevErrs ++ noKind.currentErrs.map { _.withPos(pos) }, currentErrs = Nil)
      case _              =>
        this
    }
  
  override def toString =
    this match {
      case noKind: NoKind          => "<no kind>\n" + noKind.errs.map { (" " * 8) + _ }.mkString("\n") + "\n"
      case InferredKind(kindTerm)  => intKindTermShowing.stringFrom(kindTerm)
      case InferringKind(kindTerm) => "<inferring kind: " + intKindTermShowing.stringFrom(kindTerm) + ">"
      case UninferredKind          => "<uninferred kind>"
    }
}

case class NoKind(prevErrs: List[AbstractError], currentErrs: List[AbstractError]) extends Kind
{
  def errs = prevErrs ++ currentErrs
  
  def forFile(file: Option[java.io.File]) = NoKind(prevErrs = prevErrs.map { _.withFile(file) }, currentErrs = currentErrs.map { _.withFile(file) })
}

object NoKind
{
  def fromError(err: AbstractError) = NoKind(Nil, List(err))
  
  def fromErrors(errs: NonEmptyList[AbstractError]) = NoKind(Nil, errs.list)
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