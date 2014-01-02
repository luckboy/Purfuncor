package pl.luckboy.purfuncor.backend.interp
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend.parser
import pl.luckboy.purfuncor.frontend.kinder
import pl.luckboy.purfuncor.frontend.typer
import pl.luckboy.purfuncor.frontend.instant
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol
import pl.luckboy.purfuncor.frontend.kinder.InferredKindTable
import pl.luckboy.purfuncor.frontend.typer.InferredTypeTable
import pl.luckboy.purfuncor.frontend.typer.SymbolTypeEnvironment
import pl.luckboy.purfuncor.frontend.instant.AbstractPolyFunction
import pl.luckboy.purfuncor.frontend.instant.GlobalInstance
import pl.luckboy.purfuncor.frontend.instant.InstanceTree
import pl.luckboy.purfuncor.frontend.instant.InstanceArgTable

case class SymbolEnvironment[T, U, V](
    globalVarValues: Map[GlobalSymbol, Value[Symbol, T, U, SymbolClosure[T, U]]],
    closureStack: List[SymbolClosure[T, U]],
    currentFile: Option[java.io.File],
    typeEnv: SymbolTypeEnvironment[V],
    kindTable: InferredKindTable[GlobalSymbol],
    typeTable: InferredTypeTable[GlobalSymbol, GlobalSymbol],
    instTree: InstanceTree[instant.AbstractPolyFunction[GlobalSymbol], GlobalSymbol, GlobalInstance[GlobalSymbol]],
    instArgTable: InstanceArgTable[GlobalSymbol, GlobalSymbol])
{
  def localVarValues = closureStack.headOption.map { _.localVarValues.mapValues { _.head } }.getOrElse(Map())
  
  def currentClosure = closureStack.headOption.getOrElse(SymbolClosure(Map()))
  
  def varValue(sym: Symbol): Value[Symbol, T, U, SymbolClosure[T, U]] =
    sym match {
      case globalSym: GlobalSymbol =>
        globalVarValues.getOrElse(globalSym, NoValue.fromString("undefined global variable "))
      case localSym: LocalSymbol   =>
        closureStack.headOption.map {
          _.localVarValues.get(localSym).map { _.head }.getOrElse(NoValue.fromString("undefined local variable"))
        }.getOrElse(NoValue.fromString("closure stack is empty"))
    }
  
  def pushLocalVars(values: Map[LocalSymbol, Value[Symbol, T, U, SymbolClosure[T, U]]]): SymbolEnvironment[T, U, V] =
    copy(closureStack = closureStack.headOption.map { closure => SymbolClosure(values.mapValues { NonEmptyList(_) } |+| closure.localVarValues) :: closureStack.tail }.getOrElse(Nil))
    
  def popLocalVars(syms: Set[LocalSymbol]): SymbolEnvironment[T, U, V] =
    copy(closureStack = closureStack.headOption.map { closure => SymbolClosure(closure.localVarValues.flatMap { case (s, vs) => if(syms.contains(s)) vs.tail.toNel.map { (s, _) } else some(s, vs) }.toMap) :: closureStack.tail }.getOrElse(Nil))
  
  def pushClosure(closure: SymbolClosure[T, U]): SymbolEnvironment[T, U, V] =
    copy(closureStack = closure :: closureStack)
    
  def popClosure: SymbolEnvironment[T, U, V] =
    copy(closureStack = closureStack.headOption.map { _ => closureStack.tail }.getOrElse(Nil))
  
  def withLocalVars(values: Map[LocalSymbol, Value[Symbol, T, U, SymbolClosure[T, U]]])(f: SymbolEnvironment[T, U, V] => (SymbolEnvironment[T, U, V], Value[Symbol, T, U, SymbolClosure[T, U]])) = {
    val (newEnv, value) = f(pushLocalVars(values))
    (newEnv.popLocalVars(values.keySet), value)
  }
  
  def withClosure(closure: SymbolClosure[T, U])(f: SymbolEnvironment[T, U, V] => (SymbolEnvironment[T, U, V], Value[Symbol, T, U, SymbolClosure[T, U]])) = {
    val (newEnv, value) = f(pushClosure(closure))
    (newEnv.popClosure, value)
  }

  def withGlobalVar(sym: GlobalSymbol, value: Value[Symbol, T, U, SymbolClosure[T, U]]) = copy(globalVarValues = globalVarValues + (sym -> value))
  
  def withCurrentFile(file: Option[java.io.File]) = copy(currentFile = file)
}

object SymbolEnvironment
{
  def empty[T, U, V] = SymbolEnvironment[T, U, V](
      globalVarValues = Map(),
      closureStack = List(SymbolClosure(Map())),
      currentFile = none,
      typeEnv = SymbolTypeEnvironment.empty,
      kindTable = InferredKindTable.empty,
      typeTable = InferredTypeTable.empty,
      instTree = InstanceTree.empty,
      instArgTable = InstanceArgTable.empty)
}

case class SymbolClosure[T, U](
    localVarValues: Map[LocalSymbol, NonEmptyList[Value[Symbol, T, U, SymbolClosure[T, U]]]])