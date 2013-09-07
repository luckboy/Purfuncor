package pl.luckboy.purfuncor.typer
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol

case class SymbolTypeEnvironment[T](
    globalTypeVarValues: Map[GlobalSymbol, TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]],
    typeClosureStack: List[SymbolTypeClosure[T]],
    typeParamCount: Int)
    
case class SymbolTypeClosure[T](
    localTypeVarValues: Map[LocalSymbol, NonEmptyList[TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]]])