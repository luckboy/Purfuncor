package pl.luckboy.purfuncor.backend.interp
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol

case class SymbolEnvironment[T](
    globalVarValues: Map[GlobalSymbol, Value[Symbol, T, LocalSymbol]],
    localVarStack: NonEmptyList[Map[LocalSymbol, NonEmptyList[Value[Symbol, T, LocalSymbol]]]],
    currentFile: Option[java.io.File])
{
  def localVarValues = localVarStack.head.mapValues { _.head }
  
  def varValue(sym: Symbol): Value[Symbol, T, LocalSymbol] =
    sym match {
      case globalSym: GlobalSymbol =>
        globalVarValues.getOrElse(globalSym, NoValue.fromString("undefined global variable"))
      case localSym: LocalSymbol   =>
        localVarStack.head.get(localSym).map { _.head }.getOrElse(NoValue.fromString("undefined local variable"))
    }
  
  def pushLocalVars(values: Map[LocalSymbol, Value[Symbol, T, LocalSymbol]]) =
    copy(localVarStack = NonEmptyList.nel(localVarStack.head |+| values.mapValues { NonEmptyList(_) }, localVarStack.tail))
    
  def popLocalVars(syms: Set[LocalSymbol]) =
    copy(localVarStack = NonEmptyList.nel(localVarStack.head.flatMap { case (s, vs) => if(syms.contains(s)) vs.tail.toNel.map { (s, _) } else some(s, vs) }.toMap,  localVarStack.tail))
  
  def withLocalVars(values: Map[LocalSymbol, Value[Symbol, T, LocalSymbol]])(f: SymbolEnvironment[T] => (SymbolEnvironment[T], Value[Symbol, T, LocalSymbol])) = {
    val (newEnv, value) = f(this.pushLocalVars(values))
    (newEnv.popLocalVars(values.keySet), value)
  }

  def withGlobalVar(sym: GlobalSymbol, value: Value[Symbol, T, LocalSymbol]) = copy(globalVarValues = globalVarValues + (sym -> value))
    
  def withCurrentFile(file: Option[java.io.File]) = copy(currentFile = file)
}