package pl.luckboy.purfuncor.backend.interp
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol

case class SymbolEnvironment[+T](
    globalVarValues: Map[GlobalSymbol, Value[Symbol, T, GlobalSymbol, LocalSymbol]],
    localVarValues: Map[LocalSymbol, NonEmptyList[Value[Symbol, T, GlobalSymbol, LocalSymbol]]])