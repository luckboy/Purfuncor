package pl.luckboy.purfuncor.frontend.parser
import scala.util.parsing.input.NoPosition
import scala.util.parsing.input.Position
import scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

case class ParseTree(defs: List[Def])
{
  override def toString = defs.mkString("\n\n")
}

sealed trait Def
{
  override def toString = defIndenting.indentedStringFrom(this)(0)
}
case class ImportDef(sym: ModuleSymbol) extends Def
case class CombinatorDef(sym: Symbol, typ: Option[Term[TypeSimpleTerm[Symbol, TypeLambdaInfo]]], args: List[Arg[TypeSimpleTerm[Symbol, TypeLambdaInfo]]], body: Term[SimpleTerm[Symbol, LambdaInfo, TypeSimpleTerm[Symbol, TypeLambdaInfo]]]) extends Def
case class PolyCombinatorDef(sym: Symbol, typ: Option[Term[TypeSimpleTerm[Symbol, TypeLambdaInfo]]]) extends Def
case class TypeCombinatorDef(sym: Symbol, kind: Option[KindTerm[StarKindTerm[String]]], args: List[TypeArg], body: Term[TypeSimpleTerm[Symbol, TypeLambdaInfo]]) extends Def
case class UnittypeCombinatorDef(n: Int, sym: Symbol, kind: Option[KindTerm[StarKindTerm[String]]]) extends Def
case class ModuleDef(sym: ModuleSymbol, defs: List[Def]) extends Def
case class InstanceDef(polyCombSym: Symbol, instCombSym: Symbol) extends Def
case class SelectConstructInstanceDef(supertype: Term[TypeSimpleTerm[Symbol, TypeLambdaInfo]], types: NonEmptyList[Term[TypeSimpleTerm[Symbol, TypeLambdaInfo]]]) extends Def

sealed trait LambdaInfo
{
  override def toString = ""
}
case object LambdaInfo extends LambdaInfo

sealed trait TypeLambdaInfo
{
  override def toString = ""
}
object TypeLambdaInfo extends TypeLambdaInfo

sealed trait Symbol
{
  def pos: Position
  
  override def toString =
    this match {
      case GlobalSymbol(names, _) => "#." + names.list.mkString(".")
      case NormalSymbol(names, _) => names.list.mkString(".")
    }
}
case class GlobalSymbol(names: NonEmptyList[String], pos: Position) extends Symbol
case class NormalSymbol(names: NonEmptyList[String], pos: Position) extends Symbol

sealed trait ModuleSymbol
{
  def pos: Position

  override def toString =
    this match {
      case GlobalModuleSymbol(names, _) => "#." + names.mkString(".")
      case NormalModuleSymbol(names, _) => names.list.mkString(".")
    }
}
case class GlobalModuleSymbol(names: List[String], pos: Position) extends ModuleSymbol
case class NormalModuleSymbol(names: NonEmptyList[String], pos: Position) extends ModuleSymbol