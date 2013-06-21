package pl.luckboy.purfuncor.backend.interp
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol

case class SymbolEnvironment[T](
    globalVarValues: Map[GlobalSymbol, Value[Symbol, T, SymbolClosure[T]]],
    closureStack: List[SymbolClosure[T]],
    currentFile: Option[java.io.File])
{
  def localVarValues = closureStack.headOption.map { _.localVarValues.mapValues { _.head } }.getOrElse(Map())
  
  def currentClosure = closureStack.headOption.getOrElse(SymbolClosure(Map()))
  
  def varValue(sym: Symbol): Value[Symbol, T, SymbolClosure[T]] =
    sym match {
      case globalSym: GlobalSymbol =>
        globalVarValues.getOrElse(globalSym, NoValue.fromString("undefined global variable "))
      case localSym: LocalSymbol   =>
        closureStack.headOption.map {
          _.localVarValues.get(localSym).map { _.head }.getOrElse(NoValue.fromString("undefined local variable"))
        }.getOrElse(NoValue.fromString("closure stack is empty"))
    }
  
  def pushLocalVars(values: Map[LocalSymbol, Value[Symbol, T, SymbolClosure[T]]]): SymbolEnvironment[T] =
    copy(closureStack = closureStack.headOption.map { closure => SymbolClosure(closure.localVarValues |+| values.mapValues { NonEmptyList(_) }) :: closureStack.tail }.getOrElse(Nil))
    
  def popLocalVars(syms: Set[LocalSymbol]): SymbolEnvironment[T] =
    copy(closureStack = closureStack.headOption.map { closure => SymbolClosure(closure.localVarValues.flatMap { case (s, vs) => if(syms.contains(s)) vs.tail.toNel.map { (s, _) } else some(s, vs) }.toMap) :: closureStack.tail }.getOrElse(Nil))
  
  def pushClosure(closure: SymbolClosure[T]): SymbolEnvironment[T] =
    copy(closureStack = closure :: closureStack)
    
  def popClosure: SymbolEnvironment[T] =
    copy(closureStack = closureStack.headOption.map { _ => closureStack.tail }.getOrElse(Nil))
  
  def withLocalVars(values: Map[LocalSymbol, Value[Symbol, T, SymbolClosure[T]]])(f: SymbolEnvironment[T] => (SymbolEnvironment[T], Value[Symbol, T, SymbolClosure[T]])) = {
    val (newEnv, value) = f(pushLocalVars(values))
    (newEnv.popLocalVars(values.keySet), value)
  }
  
  def withClosure(closure: SymbolClosure[T])(f: SymbolEnvironment[T] => (SymbolEnvironment[T], Value[Symbol, T, SymbolClosure[T]])) = {
    val (newEnv, value) = f(pushClosure(closure))
    (newEnv.popClosure, value)
  }

  def withGlobalVar(sym: GlobalSymbol, value: Value[Symbol, T, SymbolClosure[T]]) = copy(globalVarValues = globalVarValues + (sym -> value))
  
  def withCurrentFile(file: Option[java.io.File]) = copy(currentFile = file)
}

object SymbolEnvironment
{
  def empty[T] = SymbolEnvironment[T](
      globalVarValues = Map(),
      closureStack = Nil,
      currentFile = none)
}

case class SymbolClosure[T](
    localVarValues: Map[LocalSymbol, NonEmptyList[Value[Symbol, T, SymbolClosure[T]]]])