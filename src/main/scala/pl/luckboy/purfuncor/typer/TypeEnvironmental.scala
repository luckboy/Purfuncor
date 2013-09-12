package pl.luckboy.purfuncor.typer
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol

trait TypeEnvironmental[-T, +U]
{
  def globalTypeVarValueFromEnvironment(env: T)(sym: GlobalSymbol): U
}