package pl.luckboy.purfuncor.frontend.typer
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol

case class SymbolTypeEnvironment[T](
    globalTypeVarValues: Map[GlobalSymbol, TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]],
    typeClosureStack: List[SymbolTypeClosure[T]],
    typeParamCount: Int,
    currentFile: Option[java.io.File],
    applyingTypeCombSyms: Set[GlobalSymbol],
    currentTypeParamAppIdx: Int,
    isPartial: Boolean,
    recursiveTypeCombSyms: Set[GlobalSymbol])
{
  def currentTypeClosure = typeClosureStack.headOption.getOrElse(SymbolTypeClosure(Map()))
  
  def typeVarValue(sym: Symbol) =
    sym match {
      case globalSym: GlobalSymbol =>
        globalTypeVarValues.getOrElse(globalSym, NoTypeValue.fromError(FatalError("undefined global type variable", none, NoPosition)))
      case localSym: LocalSymbol   =>
        typeClosureStack.headOption.map {
          _.localTypeVarValues.get(localSym).map { _.head }.getOrElse(NoTypeValue.fromError(FatalError("undefined local type variable", none, NoPosition)))
        }.getOrElse(NoTypeValue.fromError(FatalError("empty type closure stack", none, NoPosition)))
    }
  
  def pushLocalTypeVars(values: Map[LocalSymbol, TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]]): SymbolTypeEnvironment[T] =
    copy(typeClosureStack = typeClosureStack.headOption.map { closure => SymbolTypeClosure(values.mapValues {  NonEmptyList(_) }.toMap |+| closure.localTypeVarValues) :: typeClosureStack }.getOrElse(Nil))
    
  def popLocalTypeVars(syms: Set[LocalSymbol]): SymbolTypeEnvironment[T] =
    copy(typeClosureStack = typeClosureStack.headOption.map { closure => SymbolTypeClosure(closure.localTypeVarValues.flatMap { case (s, vs) => if(syms.contains(s)) vs.tail.toNel.map { (s, _) } else some(s, vs) }) :: typeClosureStack }.getOrElse(Nil))
    
  def pushTypeClosure(closure: SymbolTypeClosure[T]): SymbolTypeEnvironment[T] =
    copy(typeClosureStack = closure :: typeClosureStack)
    
  def popTypeClosure: SymbolTypeEnvironment[T] =
    copy(typeClosureStack = typeClosureStack.headOption.map { _ => typeClosureStack.tail }.getOrElse(Nil))
    
  def withLocalTypeVars(values: Map[LocalSymbol, TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]])(f: SymbolTypeEnvironment[T] => (SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]])) = {
    val (newEnv, value) = f(pushLocalTypeVars(values))
    (newEnv.popLocalTypeVars(values.keySet), value)
  }
  
  def withTypeClosure(closure: SymbolTypeClosure[T])(f: SymbolTypeEnvironment[T] => (SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]])) = {
    val (newEnv, value) = f(pushTypeClosure(closure))
    (newEnv.popTypeClosure, value)
  }
  
  def withTypeParamCount(paramCount: Int): SymbolTypeEnvironment[T] = copy(typeParamCount = paramCount)
  
  def withTypeParams[U](paramCount: Int)(f: (Int, Int, SymbolTypeEnvironment[T]) => (SymbolTypeEnvironment[T], U)) = {
    val oldParamCount = typeParamCount
    val newParamCount = typeParamCount + paramCount
    val (newEnv, res) = f(oldParamCount, newParamCount, withTypeParamCount(newParamCount))
    (newEnv.withTypeParamCount(oldParamCount), res)
  }
  
  def withGlobalTypeVar(sym: GlobalSymbol, value: TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]): SymbolTypeEnvironment[T] = copy(globalTypeVarValues = globalTypeVarValues + (sym -> value))
  
  def withCurrentFile(file: Option[java.io.File]) = copy(currentFile = file)
  
  def withFile(file: Option[java.io.File])(f: SymbolTypeEnvironment[T] => (SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]])) = {
    val oldFile = currentFile
    val (newEnv, value) = f(withCurrentFile(file))
    (newEnv.withCurrentFile(oldFile), value)
  }
  
  def withApplyingTypeComb(sym: GlobalSymbol): SymbolTypeEnvironment[T] = copy(applyingTypeCombSyms = applyingTypeCombSyms + sym)
  
  def withoutApplyingTypeComb(sym: GlobalSymbol): SymbolTypeEnvironment[T] = copy(applyingTypeCombSyms = applyingTypeCombSyms - sym)
  
  def withTypeCombSym(sym: GlobalSymbol)(f: SymbolTypeEnvironment[T] => (SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]])) = {
    val (newEnv, value) = f(withApplyingTypeComb(sym))
    (newEnv.withoutApplyingTypeComb(sym), value)
  }

  def withApplyingTypeCombs(syms: Set[GlobalSymbol]): SymbolTypeEnvironment[T] = copy(applyingTypeCombSyms = applyingTypeCombSyms | syms)
  
  def withoutApplyingTypeCombs(syms: Set[GlobalSymbol]): SymbolTypeEnvironment[T] = copy(applyingTypeCombSyms = applyingTypeCombSyms -- syms)

  def withTypeCombSyms(syms: Set[GlobalSymbol])(f: SymbolTypeEnvironment[T] => (SymbolTypeEnvironment[T], TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]])) = {
    val (newEnv, value) = f(withApplyingTypeCombs(syms))
    (newEnv.withoutApplyingTypeCombs(syms), value)
  }
  
  def withCurrentTypeParamAppIdx(paramAppIdx: Int) = copy(currentTypeParamAppIdx = paramAppIdx)
  
  def withTypeParamAppIdx[U](paramAppIdx: Int)(f: SymbolTypeEnvironment[T] => (SymbolTypeEnvironment[T], U)) = {
    val oldParamAppIdx = currentTypeParamAppIdx
    val (env, res) = f(withCurrentTypeParamAppIdx(paramAppIdx))
    (env.withCurrentTypeParamAppIdx(oldParamAppIdx), res)
  }
  
  def withPartial(isPartial: Boolean): SymbolTypeEnvironment[T] = copy(isPartial = isPartial)
  
  def withPartialEvaluation[U](isPartial: Boolean)(f: SymbolTypeEnvironment[T] => (SymbolTypeEnvironment[T], U)) = {
    val old = this.isPartial
    val (env, res) = f(withPartial(isPartial))
    (env.withPartial(old), res)
  }
  
  def withRecursiveTypeCombSyms(syms: Set[GlobalSymbol]): SymbolTypeEnvironment[T] = copy(recursiveTypeCombSyms = syms)
  
  def withClear[U](f: SymbolTypeEnvironment[T] => (SymbolTypeEnvironment[T], U)): (SymbolTypeEnvironment[T], U) = {
    val (env, res) = f(copy(typeClosureStack = List(SymbolTypeClosure(Map()))))
    (env.copy(typeClosureStack = List(SymbolTypeClosure(Map()))), res)
  }
}

object SymbolTypeEnvironment
{
  def empty[T] = SymbolTypeEnvironment[T](
      globalTypeVarValues = Map(),
      typeClosureStack = List(SymbolTypeClosure(Map())),
      typeParamCount = 0,
      currentFile = none,
      applyingTypeCombSyms = Set(),
      currentTypeParamAppIdx = 0,
      isPartial = false,
      recursiveTypeCombSyms = Set())
}
    
case class SymbolTypeClosure[T](
    localTypeVarValues: Map[LocalSymbol, NonEmptyList[TypeValue[GlobalSymbol, Symbol, T, SymbolTypeClosure[T]]]])