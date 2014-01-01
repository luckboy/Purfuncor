package pl.luckboy.purfuncor.backend.interp
import pl.luckboy.purfuncor.frontend.resolver.NameTree

trait EnvironmentState[E]
{
  def nameTreeFromEnvironmentS(env: E): (E, NameTree)
}