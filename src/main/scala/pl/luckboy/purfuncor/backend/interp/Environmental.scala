package pl.luckboy.purfuncor.backend.interp
import pl.luckboy.purfuncor.frontend.resolver.NameTree
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol

trait Environmental[-T, +U]
{
  def globalVarValueFromEnvironment(env: T)(sym: GlobalSymbol): U
}