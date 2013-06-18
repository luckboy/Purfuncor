package pl.luckboy.purfuncor.backend
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol
import pl.luckboy.purfuncor.common.Evaluator._

package object interp
{
  implicit def symbolSimpleTermEvaluator[T] = new Evaluator[SimpleTerm[Symbol, T], SymbolEnvironment[T], Value[Symbol, T, SymbolClosure[T]]] {
    private implicit val implicitThis = this
    
    override def evaluateSimpleTermS(simpleTerm: SimpleTerm[Symbol, T])(env: SymbolEnvironment[T]) =
      simpleTerm match {
        case Let(binds, body, _)       =>
          valuesFromTermsS(binds.map { _.body }.list)(env)(this) match {
            case (env2, Success(tmpBindValues)) =>
              val bindValues = binds.list.map { b => LocalSymbol(b.name) }.zip(tmpBindValues).toMap
              env2.withLocalVars(bindValues) { evaluateS(body)(_) }
            case (env2, Failure(noValue))       =>
              (env2, noValue)
          }
        case lambda: Lambda[Symbol, T] =>
          (env, LambdaValue(lambda, env.currentClosure, env.currentFile))
        case Var(loc)                  =>
          (env, env.varValue(loc))
        case Literal(value)            =>
          Value.fromLiteralValue(value) match {
            case BuiltinFunValue(_, f) if f.argCount === 0 => f.applyS(Vector())(env)
            case value2                                    => (env, value2)
          }
      }
    
    override def valueFromTermS(term: Term[SimpleTerm[Symbol, T]])(env: SymbolEnvironment[T]) = evaluateS(term)(env)
    
    override def valueArgCount(value: Value[Symbol, T, SymbolClosure[T]]) = value.argCount
    
    override def fullyAppS(funValue: Value[Symbol, T, SymbolClosure[T]], argValues: Seq[Value[Symbol, T, SymbolClosure[T]]])(env: SymbolEnvironment[T]): (SymbolEnvironment[T], Value[Symbol, T, SymbolClosure[T]]) =
      funValue match {
        case CombinatorValue(comb, sym) =>
          if(comb.args.size === argValues.size) {
            val localVarValues = comb.args.zip(argValues).flatMap { 
              case (Arg(Some(name), _), v) => some((LocalSymbol(name), v))
            }.toMap
            val (env2, retValue) = env.withClosure(SymbolClosure(Map())) {
              _.withLocalVars(localVarValues) { newEnv => evaluateS(comb.body)(env.withCurrentFile(comb.file)) }
            }
            (env2.popClosure, retValue.forFileAndCombSym(comb.file, some(sym)))
          } else
            (env, NoValue.fromString("illegal number of arguments"))
        case LambdaValue(lambda, closure, file) =>
          if(lambda.args.size === argValues.size) {
            val localVarValues = lambda.args.list.zip(argValues).flatMap { 
              case (Arg(Some(name), _), v) => some((LocalSymbol(name), v))
            }.toMap
            val (env2, retValue) = env.withClosure(closure) {
              _.withLocalVars(localVarValues) { newEnv => evaluateS(lambda.body)(newEnv.withCurrentFile(file)) }
            }
            (env2, retValue.forFileAndCombSym(file, none))
          } else
            (env, NoValue.fromString("illegal number of arguments"))
        case PartialAppValue(funValue2, argValues2) =>
          if(funValue2.argCount - argValues2.size === argValues.size)
            fullyAppS(funValue2, argValues2 ++ argValues)(env)
          else
            (env, NoValue.fromString("illegal number of arguments"))
        case tupleFunValue @ TupleFunValue(_) =>
          tupleFunValue.fullyApplyS(argValues)(env)
        case tupleFieldFunValue @ TupleFieldFunValue(_) =>
          tupleFieldFunValue.fullyApplyS(argValues)(env)
        case BuiltinFunValue(_, f) =>
          if(f.argCount === argValues.size)
            f.applyS(argValues)(env)
          else
            (env, NoValue.fromString("illegal number of arguments"))
        case _ =>
          (env, NoValue.fromString("no applicable"))
      }
    
    override def partiallyAppS(funValue: Value[Symbol, T, SymbolClosure[T]], argValues: Seq[Value[Symbol, T, SymbolClosure[T]]])(env: SymbolEnvironment[T]) =
      (env, PartialAppValue(funValue, argValues))

    override def isNoValue(value: Value[Symbol, T, SymbolClosure[T]]) =
      value.isNoValue
      
    override def withPos(res: (SymbolEnvironment[T], Value[Symbol, T, SymbolClosure[T]]))(pos: Position) =
      (res._1, res._2.withPos(pos))
  }
  
  implicit def symbolCombinatorInitializer[T] = new Initializer[NoValue[Symbol, T, SymbolClosure[T]], GlobalSymbol, Combinator[Symbol, T], SymbolEnvironment[T]] {
    override def globalVarsFromEnvironment(env: SymbolEnvironment[T]) = env.globalVarValues.keySet
        
    private def usedGlobalVarsFromTerm(term: Term[SimpleTerm[Symbol, T]]): Set[GlobalSymbol] =
      term match {
        case App(fun, args, _)                 => usedGlobalVarsFromTerm(fun) | args.list.flatMap(usedGlobalVarsFromTerm).toSet
        case Simple(Var(sym: GlobalSymbol), _) => Set(sym)
        case Simple(_, _)                      => Set()
      }
    
    override def usedGlobalVarsFromCombinator(comb: Combinator[Symbol, T]) = usedGlobalVarsFromTerm(comb.body)
      
    override def prepareGlobalVarS(loc: GlobalSymbol)(env: SymbolEnvironment[T]) = 
      (env.withGlobalVar(loc, NoValue.fromString("initialization cycle")), ())
    
    override def initializeGlobalVarS(loc: GlobalSymbol, comb: Combinator[Symbol, T])(env: SymbolEnvironment[T]) = {
      val (env2, value: Value[Symbol, T, SymbolClosure[T]]) = if(comb.args.isEmpty)
        evaluateS(comb.body)(env.withCurrentFile(comb.file))
      else
        (env, CombinatorValue(comb, loc))
      value match {
        case noValue: NoValue[Symbol, T, SymbolClosure[T]] => (env2, noValue.failure)
        case _                                             => (env2.withGlobalVar(loc, value), ().success)
      }
    }

    override def undefinedGlobalVarError: NoValue[Symbol, T, SymbolClosure[T]] =
      NoValue.fromString("undefined global variable")
  }
}