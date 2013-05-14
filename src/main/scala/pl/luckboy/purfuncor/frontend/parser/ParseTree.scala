package pl.luckboy.purfuncor.frontend.parser
import scala.util.parsing.input.NoPosition
import scala.util.parsing.input.Position
import scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

case class ParseTree(defs: List[Def])

sealed trait Def
case class ImportDef(sym: Symbol) extends Def
case class CombinatorDef(sym: Symbol, args: List[Arg], body: Term[SimpleTerm[Symbol, Unit]]) extends Def
case class ModuleDef(sym: Symbol, defs: List[Def]) extends Def

case class Symbol(names: NonEmptyList[String], pos: Position)