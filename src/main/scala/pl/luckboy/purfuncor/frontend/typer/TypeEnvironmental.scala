package pl.luckboy.purfuncor.frontend.typer
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol

trait TypeEnvironmental[T, U]
{
  def globalTypeVarValueFromEnvironment(env: T)(sym: GlobalSymbol): U
  
  def withGlobalTypeVar(env: T)(sym: GlobalSymbol, value: U): T
  
  def withPartialEvaluation[U](env: T)(isPartial: Boolean)(f: T => (T, U)): (T, U)
}