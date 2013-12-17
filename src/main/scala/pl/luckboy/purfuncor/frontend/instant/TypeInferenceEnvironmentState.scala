package pl.luckboy.purfuncor.frontend.instant
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.typer.DefinedType
import pl.luckboy.purfuncor.frontend.typer.Type
import pl.luckboy.purfuncor.frontend.typer.NoType

trait TypeInferenceEnvironmentState[E, L, M]
{  
  def globalVarTypeFromEnvironmentS(loc: L)(env: E): (E, Type[M])
  
  def notFoundInstanceNoTypeS(instArg: InstanceArg[L, M])(env: E): (E, NoType[M])
  
  def ambiguousInstanceNoTypeS(instArg: InstanceArg[L, M])(env: E): (E, NoType[M])
  
  def withInstanceTypeClearingS[T](f: E => (E, T))(env: E): (E, T)
  
  def definedTypesFromEnvironmentS(env: E): (E, List[DefinedType[M]])
  
  def addDefinedTypeS(definedType: DefinedType[M])(env: E): (E, Unit)
}