package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

case class DefinedType[T](args: List[DefinedTypeArg], term: TypeValueTerm[T])

case class DefinedTypeArg(param: Int, kind: Option[KindTerm[StarKindTerm[Int]]])