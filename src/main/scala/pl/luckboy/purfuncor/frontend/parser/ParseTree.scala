package pl.luckboy.purfuncor.frontend.parser
import scala.util.parsing.input.NoPosition
import scala.util.parsing.input.Position
import scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

case class ParseTree(defs: List[Def])

sealed trait Def
case class CombinatorDef(sym: Symbol, args: List[Arg], body: Term[SimpleTerm]) extends Def
case class ModuleDef(sym: Symbol, defs: List[Def]) extends Def

sealed trait SimpleTerm
case class Let(binds: NonEmptyList[Bind], body: Term[SimpleTerm]) extends SimpleTerm
case class Lambda(args: NonEmptyList[Arg], body: Term[SimpleTerm]) extends SimpleTerm
case class Var(sym: Symbol) extends SimpleTerm
case class Literal(value: LiteralValue) extends SimpleTerm

case class Bind(name: String, body: Term[SimpleTerm], pos: Position)

case class Arg(name: String, pos: Position)

case class Symbol(names: NonEmptyList[String], pos: Position)