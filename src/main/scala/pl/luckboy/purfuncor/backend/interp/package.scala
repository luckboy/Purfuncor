package pl.luckboy.purfuncor.backend
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol
import pl.luckboy.purfuncor.frontend.resolver.NameTree
import pl.luckboy.purfuncor.common.Evaluator._
import pl.luckboy.purfuncor.frontend.resolver.TermUtils._

package object interp
{
  implicit def symbolSimpleTermEvaluator[T, U]: Evaluator[SimpleTerm[Symbol, T, U], SymbolEnvironment[T, U], Value[Symbol, T, U, SymbolClosure[T, U]]] = new Evaluator[SimpleTerm[Symbol, T, U], SymbolEnvironment[T, U], Value[Symbol, T, U, SymbolClosure[T, U]]] {
    private implicit val implicitThis = this
    
    override def evaluateSimpleTermS(simpleTerm: SimpleTerm[Symbol, T, U])(env: SymbolEnvironment[T, U]) =
      simpleTerm match {
        case Let(binds, body, _)          =>
          valuesFromTermsS(binds.map { _.body }.list)(env)(this) match {
            case (env2, Success(tmpBindValues)) =>
              val bindValues = binds.list.map { b => LocalSymbol(b.name) }.zip(tmpBindValues).toMap
              env2.withLocalVars(bindValues) { evaluateS(body)(_) }
            case (env2, Failure(noValue))       =>
              (env2, noValue)
          }
        case lambda: Lambda[Symbol, T, U] =>
          (env, LambdaValue(lambda, env.currentClosure, env.currentFile))
        case Var(loc)                     =>
          (env, env.varValue(loc))
        case Literal(value)               =>
          Value.fromLiteralValue(value) match {
            case BuiltinFunValue(_, f) if f.argCount === 0 => f.applyS(Vector())(env)
            case value2                                    => (env, value2)
          }
        case TypedTerm(term, _)            =>
          evaluateS(term)(env)
      }
    
    override def valueFromTermS(term: Term[SimpleTerm[Symbol, T, U]])(env: SymbolEnvironment[T, U]) = evaluateS(term)(env)
    
    override def valueArgCount(value: Value[Symbol, T, U, SymbolClosure[T, U]]) = value.argCount
    
    override def fullyAppS(funValue: Value[Symbol, T, U, SymbolClosure[T, U]], argValues: Seq[Value[Symbol, T, U, SymbolClosure[T, U]]])(env: SymbolEnvironment[T, U]): (SymbolEnvironment[T, U], Value[Symbol, T, U, SymbolClosure[T, U]]) =
      funValue match {
        case CombinatorValue(comb: Combinator[Symbol, T, U], sym) =>
          if(comb.args.size === argValues.size) {
            val localVarValues = comb.args.zip(argValues).flatMap { 
              case (Arg(Some(name), _, _), v) => some((LocalSymbol(name), v))
            }.toMap
            val (env2, retValue) = env.withClosure(SymbolClosure(Map())) {
              _.withLocalVars(localVarValues) { newEnv => evaluateS(comb.body)(newEnv.withCurrentFile(comb.file)) }
            }
            (env2, retValue.forFileAndCombSym(comb.file, some(sym)))
          } else
            (env, NoValue.fromString("illegal number of arguments"))
        case LambdaValue(lambda, closure, file) =>
          if(lambda.args.size === argValues.size) {
            val localVarValues = lambda.args.list.zip(argValues).flatMap { 
              case (Arg(Some(name), _, _), v) => List((LocalSymbol(name), v))
              case (_, _)                  => Nil
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
    
    override def partiallyAppS(funValue: Value[Symbol, T, U, SymbolClosure[T, U]], argValues: Seq[Value[Symbol, T, U, SymbolClosure[T, U]]])(env: SymbolEnvironment[T, U]) =
      (env, PartialAppValue(funValue, argValues))

    override def isNoValue(value: Value[Symbol, T, U, SymbolClosure[T, U]]) =
      value.isNoValue
      
    override def withPos(res: (SymbolEnvironment[T, U], Value[Symbol, T, U, SymbolClosure[T, U]]))(pos: Position) =
      (res._1, res._2.withPos(pos))
  }
  
  implicit def symbolCombinatorInitializer[T, U] = new Initializer[NoValue[Symbol, T, U, SymbolClosure[T, U]], GlobalSymbol, AbstractCombinator[Symbol, T, U], SymbolEnvironment[T, U]] {
    override def globalVarsFromEnvironmentS(env: SymbolEnvironment[T, U]) = (env, env.globalVarValues.keySet)
        
    override def usedGlobalVarsFromCombinator(comb: AbstractCombinator[Symbol, T, U]) =
      comb match {
        case Combinator(_, _, body, _, _) => usedGlobalVarsFromTerm(body)
      }
      
    override def prepareGlobalVarS(loc: GlobalSymbol)(env: SymbolEnvironment[T, U]) = 
      (env.withGlobalVar(loc, NoValue.fromString("initialization cycle")), ())
    
    override def initializeGlobalVarS(loc: GlobalSymbol, comb: AbstractCombinator[Symbol, T, U])(env: SymbolEnvironment[T, U]) = {
      val (env2, value: Value[Symbol, T, U, SymbolClosure[T, U]]) = if(comb.argCount === 0) {
        comb match {
          case Combinator(_, _, body, _, file) =>
            val (newEnv, value) = evaluateS(body)(env.withCurrentFile(file))
            (newEnv, value.forFileAndCombSym(file, some(loc)))
        }
      } else
        (env, CombinatorValue(comb, loc))
      value match {
        case noValue: NoValue[Symbol, T, U, SymbolClosure[T, U]] => (env2, noValue.failure)
        case _                                                   => (env2.withGlobalVar(loc, value), ().success)
      }
    }

    override def checkEnvironmentS(env: SymbolEnvironment[T, U]) =
      (env, ().success[NoValue[Symbol, T, U, SymbolClosure[T, U]]])
    
    override def undefinedGlobalVarError: NoValue[Symbol, T, U, SymbolClosure[T, U]] =
      NoValue.fromString("undefined global variable")
    
    override def withSaveS[V, W](f: SymbolEnvironment[T, U] => (SymbolEnvironment[T, U], Validation[V, W]))(env: SymbolEnvironment[T, U]) = {
      val (env2, res) = f(env)
      res.map { x => (env2, x.success) }.valueOr { e => (env, e.failure ) }
    }
  }

  implicit def symbolEnvironmentState[T, U] = new EnvironmentState[SymbolEnvironment[T, U]] {
    override def nameTreeFromEnvironment(env: SymbolEnvironment[T, U]) =
      (env, NameTree.fromGlobalSymbols(env.globalVarValues.keys) |+| NameTree.fromTypeGlobalSymbols(env.typeCombSyms))
  }
  
  implicit def symbolEnvironmental[T, U] = new Environmental[SymbolEnvironment[T, U], Value[Symbol, T, U, SymbolClosure[T, U]]] {
    override def globalVarValueFromEnvironment(env: SymbolEnvironment[T, U])(sym: GlobalSymbol) =
      env.varValue(sym)
  }
}