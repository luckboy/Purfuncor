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
  implicit def instantSymbolSimpleTermExtendedEvaluator[T, U, V]: ExtendedEvaluator[instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], SymbolEnvironment[instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], U, V], Value[Symbol, instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], U, SymbolClosure[instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], U]]] = new ExtendedEvaluator[instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], SymbolEnvironment[instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], U, V], Value[Symbol, instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], U, SymbolClosure[instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], U]]] {
    override def variableS(value: Value[Symbol, instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], U, SymbolClosure[instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], U]], lambdaInfo: instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol])(env: SymbolEnvironment[instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], U, V]): (SymbolEnvironment[instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], U, V], Value[Symbol, instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], U, SymbolClosure[instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], U]]) =
      throw new UnsupportedOperationException
    
    override def constructS(n: Int, lambdaInfo: instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol])(env: SymbolEnvironment[instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], U, V]): (SymbolEnvironment[instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], U, V], Value[Symbol, instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], U, SymbolClosure[instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], U]]) =
      throw new UnsupportedOperationException
    
    override def selectS[W, X](value: Value[Symbol, instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], U, SymbolClosure[instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], U]], cases: Seq[Case[W, instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], X]], lambdaInfo: instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol])(env: SymbolEnvironment[instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], U, V]): (SymbolEnvironment[instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], U, V], Validation[Value[Symbol, instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], U, SymbolClosure[instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], U]], Case[W, instant.LambdaInfo[T, LocalSymbol, GlobalSymbol, GlobalSymbol], X]]) =
      throw new UnsupportedOperationException
  }
  
  implicit def symbolSimpleTermEvaluator[T, U, V](implicit extendedEval: ExtendedEvaluator[T, SymbolEnvironment[T, U, V], Value[Symbol, T, U, SymbolClosure[T, U]]]): Evaluator[SimpleTerm[Symbol, T, U], SymbolEnvironment[T, U, V], Value[Symbol, T, U, SymbolClosure[T, U]]] = new Evaluator[SimpleTerm[Symbol, T, U], SymbolEnvironment[T, U, V], Value[Symbol, T, U, SymbolClosure[T, U]]] {
    override def evaluateSimpleTermS(simpleTerm: SimpleTerm[Symbol, T, U])(env: SymbolEnvironment[T, U, V]) =
      simpleTerm match {
        case Let(binds, body, _)             =>
          valuesFromTermsS(binds.map { _.body }.list)(env)(this) match {
            case (env2, Success(tmpBindValues)) =>
              val bindValues = binds.list.map { b => LocalSymbol(b.name) }.zip(tmpBindValues).toMap
              env2.withLocalVars(bindValues) { evaluateS(body)(_) }
            case (env2, Failure(noValue))       =>
              (env2, noValue)
          }
        case lambda: Lambda[Symbol, T, U]    =>
          (env, LambdaValue(lambda, env.currentClosure, env.currentFile))
        case Var(loc, lambdaInfo)         =>
          extendedEval.variableS(env.varValue(loc), lambdaInfo)(env)
        case Literal(value)               =>
          Value.fromLiteralValue(value) match {
            case BuiltinFunValue(_, f) if f.argCount === 0 => f.applyS(Vector())(env)
            case value2                                    => (env, value2)
          }
        case TypedTerm(term, _)              =>
          evaluateS(term)(env)
        case Construct(n, lambdaInfo)        =>
          extendedEval.constructS(n, lambdaInfo)(env)
        case Select(term, cases, lambdaInfo) =>
          val (env2, value) = evaluateS(term)(env)
          val (env3, res) = extendedEval.selectS(value, cases.list, lambdaInfo)(env2)
          res match {
            case Success(Case(name, _, body, _)) =>
              env3.withLocalVars(name.map { s => Map(LocalSymbol(s) -> value) }.getOrElse(Map())) { evaluateS(body)(_) }
            case Failure(noValue)                =>
              (env3, noValue)
          }
        case Extract(term, args, body, _)    =>
          val (env2, value) = evaluateS(term)(env)
          value match {
            case productValue: ProductValue[Symbol, T, U, SymbolClosure[T, U]] =>
              if(productValue.values.size === args.size) {
                val localVarValues = args.list.zip(productValue.values).flatMap {
                  case (Arg(Some(name), _, _), v) => List((LocalSymbol(name), v))
                  case _                          => Nil
                }.toMap
                env2.withLocalVars(localVarValues) { evaluateS(body)(_) }
              } else
                (env2, NoValue.fromString("illegal number of arguments"))
            case noValue: NoValue[Symbol, T, U, SymbolClosure[T, U]]           =>
              (env2, value)
            case _                                                             =>
              (env2, NoValue.fromString("no product value"))
          }
      }
    
    override def valueFromTermS(term: Term[SimpleTerm[Symbol, T, U]])(env: SymbolEnvironment[T, U, V]) = evaluateS(term)(env)
    
    override def valueArgCount(value: Value[Symbol, T, U, SymbolClosure[T, U]]) = value.argCount
    
    override def fullyAppS(funValue: Value[Symbol, T, U, SymbolClosure[T, U]], argValues: Seq[Value[Symbol, T, U, SymbolClosure[T, U]]])(env: SymbolEnvironment[T, U, V]): (SymbolEnvironment[T, U, V], Value[Symbol, T, U, SymbolClosure[T, U]]) =
      funValue match {
        case CombinatorValue(comb: Combinator[Symbol, T, U], sym) =>
          if(comb.args.size === argValues.size) {
            val localVarValues = comb.args.zip(argValues).flatMap { 
              case (Arg(Some(name), _, _), v) => List((LocalSymbol(name), v))
              case (_, _)                     => Nil
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
              case (_, _)                     => Nil
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
        case tupleFieldFunValue @ TupleFieldFunValue(_, _) =>
          tupleFieldFunValue.fullyApplyS(argValues)(env)
        case BuiltinFunValue(_, f) =>
          if(f.argCount === argValues.size)
            f.applyS(argValues)(env)
          else
            (env, NoValue.fromString("illegal number of arguments"))
        case _ =>
          (env, NoValue.fromString("no applicable"))
      }
    
    override def partiallyAppS(funValue: Value[Symbol, T, U, SymbolClosure[T, U]], argValues: Seq[Value[Symbol, T, U, SymbolClosure[T, U]]])(env: SymbolEnvironment[T, U, V]) =
      (env, PartialAppValue(funValue, argValues))

    override def isNoValue(value: Value[Symbol, T, U, SymbolClosure[T, U]]) =
      value.isNoValue
      
    override def forceS(value: Value[Symbol, T, U, SymbolClosure[T, U]])(env: SymbolEnvironment[T, U, V]) =
      (env, value)
      
    override def withPos(res: (SymbolEnvironment[T, U, V], Value[Symbol, T, U, SymbolClosure[T, U]]))(pos: Position) =
      (res._1, res._2.withPos(pos))
  }
  
  implicit def instantLambdaInfoSymbolSimpleTermEvaluator[T, U, V] = symbolSimpleTermEvaluator(instantSymbolSimpleTermExtendedEvaluator[T, U, V])
  
  def symbolCombinatorInitializer[T, U, V](implicit extendedEval: ExtendedEvaluator[T, SymbolEnvironment[T, U, V], Value[Symbol, T, U, SymbolClosure[T, U]]]): Initializer[NoValue[Symbol, T, U, SymbolClosure[T, U]], GlobalSymbol, AbstractCombinator[Symbol, T, U], SymbolEnvironment[T, U, V]] = new Initializer[NoValue[Symbol, T, U, SymbolClosure[T, U]], GlobalSymbol, AbstractCombinator[Symbol, T, U], SymbolEnvironment[T, U, V]] {
    override def globalVarsFromEnvironmentS(env: SymbolEnvironment[T, U, V]) = (env, env.globalVarValues.keySet)
        
    override def usedGlobalVarsFromCombinator(comb: AbstractCombinator[Symbol, T, U]) =
      comb match {
        case Combinator(_, _, body, _, _) => usedGlobalVarsFromTerm(body)
        case PolyCombinator(_, _)         => Set()
      }
      
    override def prepareGlobalVarS(loc: GlobalSymbol)(env: SymbolEnvironment[T, U, V]) = 
      (env.withGlobalVar(loc, NoValue.fromString("initialization cycle")), ())
    
    override def initializeGlobalVarS(loc: GlobalSymbol, comb: AbstractCombinator[Symbol, T, U])(env: SymbolEnvironment[T, U, V]) = {
      val (env2, value: Value[Symbol, T, U, SymbolClosure[T, U]]) = comb match {
        case comb2 @ Combinator(_, _, body, _, file) =>
          if(comb2.argCount === 0) {
            val (newEnv, value) = evaluateS(body)(env.withCurrentFile(file))
            (newEnv, value.forFileAndCombSym(file, some(loc)))
          } else
            (env, CombinatorValue(comb2, loc))
        case PolyCombinator(_, _)            =>
          (env, PolyFunValue)
      }
      value match {
        case noValue: NoValue[Symbol, T, U, SymbolClosure[T, U]] => (env2, noValue.failure)
        case _                                                   => (env2.withGlobalVar(loc, value), ().success)
      }
    }

    override def checkEnvironmentS(env: SymbolEnvironment[T, U, V]) =
      (env, ().success[NoValue[Symbol, T, U, SymbolClosure[T, U]]])
    
    override def undefinedGlobalVarError: NoValue[Symbol, T, U, SymbolClosure[T, U]] =
      NoValue.fromString("undefined global variable")
    
    override def withSaveS[W, X](f: SymbolEnvironment[T, U, V] => (SymbolEnvironment[T, U, V], Validation[W, X]))(env: SymbolEnvironment[T, U, V]) = {
      val (env2, res) = f(env)
      res.map { x => (env2, x.success) }.valueOr { e => (env, e.failure ) }
    }
  }
  
  implicit def instantLambdaInfoSymbolCombinatorInitializer[T, U, V] = symbolCombinatorInitializer(instantSymbolSimpleTermExtendedEvaluator[T, U, V])

  implicit def symbolEnvironmentState[T, U, V] = new EnvironmentState[SymbolEnvironment[T, U, V]] {
    override def nameTreeFromEnvironmentS(env: SymbolEnvironment[T, U, V]) =
      (env, NameTree.fromGlobalSymbols(env.globalVarValues.keys) |+| NameTree.fromTypeGlobalSymbols(env.typeEnv.globalTypeVarValues.keySet))
  }
  
  implicit def symbolEnvironmental[T, U, V] = new Environmental[SymbolEnvironment[T, U, V], Value[Symbol, T, U, SymbolClosure[T, U]]] {
    override def globalVarValueFromEnvironment(env: SymbolEnvironment[T, U, V])(sym: GlobalSymbol) =
      env.varValue(sym)
  }
}