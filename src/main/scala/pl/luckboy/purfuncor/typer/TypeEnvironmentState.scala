package pl.luckboy.purfuncor.typer

trait TypeEnvironmentState[E]
{
  def withTypeParamsS[T](paramCount: Int)(f: (Int, Int, E) => (E, T))(env: E): (E, T)
}