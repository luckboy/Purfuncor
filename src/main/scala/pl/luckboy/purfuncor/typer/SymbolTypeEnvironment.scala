package pl.luckboy.purfuncor.typer
import scala.util.parsing.input.NoPosition
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
{
  def currentTypeClosure = typeClosureStack.headOption.getOrElse(SymbolTypeClosure(Map()))
  
  def typeVarValue(sym: Symbol) =
    sym match {
      case globalSym: GlobalSymbol =>
        globalTypeVarValues.getOrElse(globalSym, NoTypeValue.fromError(FatalError("undefined global type variable", none, NoPosition)))
      case localSym: LocalSymbol   =>
        typeClosureStack.headOption.map {
          _.localTypeVarValues.get(localSym).map { _.head }.getOrElse(NoTypeValue.fromError(FatalError("undefined local type variable", none, NoPosition)))
        }.getOrElse(NoTypeValue.fromError(FatalError("empty type closure stack", none, NoPosition)))
    }
}
    
case class SymbolTypeClosure[T](
    localTypeVarValues: Map[LocalSymbol, NonEmptyList[TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]]])