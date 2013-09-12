package pl.luckboy.purfuncor.frontend.typer
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol

trait TypeEnvironmental[-T, +U]
{
  def globalTypeVarValueFromEnvironment(env: T)(sym: GlobalSymbol): U
}