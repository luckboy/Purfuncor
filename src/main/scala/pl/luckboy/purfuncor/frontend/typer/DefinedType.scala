package pl.luckboy.purfuncor.frontend.typer
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

case class DefinedType[T](args: List[DefinedTypeArg], term: TypeValueTerm[T], pos: Position)

case class DefinedTypeArg(param: Option[Int], kind: Option[KindTerm[StarKindTerm[Int]]])