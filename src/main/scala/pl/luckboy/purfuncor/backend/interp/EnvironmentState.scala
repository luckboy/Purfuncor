package pl.luckboy.purfuncor.backend.interp
import pl.luckboy.purfuncor.frontend.resolver.NameTree

trait EnvironmentState[E, L, V, W]
{
  def nameTreeFromEnvironmentS(env: E): (E, NameTree)
  
  def globalVarValueFromEnvironmentS(loc: L)(env: E): (E, V)
  
  def currentLocalInstanceValuesFromEnvironmentS(env: E): (E, Seq[W])
}