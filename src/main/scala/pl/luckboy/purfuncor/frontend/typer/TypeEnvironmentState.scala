package pl.luckboy.purfuncor.frontend.typer

trait TypeEnvironmentState[E, L, V]
{
  def typeParamCountFromEnvironmentS(env: E): (E, Int)
  
  def withTypeParamsS[T](paramCount: Int)(f: (Int, Int, E) => (E, T))(env: E): (E, T)
  
  def currentTypeParamAppIdxFromEnvironmentS(env: E): (E, Int)
  
  def globalTypeVarValueFromEnvironmentS(loc: L)(env: E): (E, V)
}