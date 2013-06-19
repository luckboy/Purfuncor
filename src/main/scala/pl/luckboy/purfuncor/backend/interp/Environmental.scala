package pl.luckboy.purfuncor.backend.interp
import pl.luckboy.purfuncor.frontend.resolver.NameTree

trait Environmental[T]
{
  def nameTreeFromEnvironment(env: T): NameTree
}