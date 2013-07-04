package pl.luckboy.purfuncor.frontend.kinder
import scala.collection.immutable.IntMap
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

sealed trait Kind[+T]
case class NoKind[+T](errs: NonEmptyList[AbstractError]) extends Kind[T]
case class InferredKind[+T](kindTerm: KindTerm[StarKindTerm[T]]) extends Kind[T]
case class InferringKind[+T](paramKindIdx: Int) extends Kind[T]