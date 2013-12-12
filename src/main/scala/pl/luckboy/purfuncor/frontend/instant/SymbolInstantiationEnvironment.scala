package pl.luckboy.purfuncor.frontend.instant
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.typer._

case class SymbolInstantiationEnvironment[T, U](
    typeInferenceEnv: SymbolTypeInferenceEnvironment[T, U])