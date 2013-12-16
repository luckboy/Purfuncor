package pl.luckboy.purfuncor.frontend.instant
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.typer.Type
import pl.luckboy.purfuncor.frontend.typer.NoType

trait TypeInferenceEnvironmentState[E, L, M]
{  
  def globalVarTypeFromEnvironmentS(loc: L)(env: E): (E, Type[M])
  
  def notFoundInstanceNoTypeS(instArg: InstanceArg[L, M])(env: E): (E, NoType[M])
  
  def ambiguousInstanceNoTypeS(instArg: InstanceArg[L, M])(env: E): (E, NoType[M])
}