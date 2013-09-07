package pl.luckboy.purfuncor.common
import scala.annotation.tailrec
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._

trait Evaluator[-T, E, V]
{
  def evaluateSimpleTermS(simpleTerm: T)(env: E): (E, V)
  
  def valueFromTermS(term: Term[T])(env: E): (E, V)
  
  def valueArgCount(value: V): Int
  
  def fullyAppS(funValue: V, argValues: Seq[V])(env: E): (E, V)
  
  def partiallyAppS(funValue: V, argValues: Seq[V])(env: E): (E, V)
  
  def isNoValue(value: V): Boolean
  
  def forceS(value: V)(env: E): (E, V)
  
  def withPos(res: (E, V))(pos: Position): (E, V)
}

object Evaluator
{
  def evaluateS[T, E, V](term: Term[T])(env: E)(implicit eval: Evaluator[T, E, V]): (E, V) = {
    val res = term match {
      case App(fun, args, _)     =>
        val (env2, funValue) = evaluateS(fun)(env)
        valuesFromTermsS(args.list)(env2) match {
          case (env3, Success(argValues)) => appS(funValue, argValues)(env3)
          case (env3, Failure(noValue))   => (env3, noValue)
        }
      case Simple(simpleTerm, _) =>
        eval.evaluateSimpleTermS(simpleTerm)(env)
    }
    eval.withPos(res)(term.pos)
  }
    
  def evaluate[T, E, V](term: Term[T])(implicit eval: Evaluator[T, E, V]) =
    State(evaluateS[T, E, V](term))
    
  @tailrec
  def appS[T, E, V](funValue: V, argValues: Seq[V])(env: E)(implicit eval: Evaluator[T, E, V]): (E, V) = {
    val argCount = eval.valueArgCount(funValue)
    if(!eval.isNoValue(funValue))
      if(argCount === argValues.size) {
        eval.fullyAppS(funValue, argValues)(env)
      } else if(argCount > argValues.size) {
        eval.partiallyAppS(funValue, argValues)(env)
      } else {
        val (passedArgValues, otherArgValues) = argValues.splitAt(argCount)
        val (env2, retValue) = eval.fullyAppS(funValue, passedArgValues)(env)
        appS[T, E, V](retValue, otherArgValues)(env2)
      }
    else
      (env, funValue)
  }
  
  def app[T, E, V](funValue: V, argValues: Seq[V])(implicit eval: Evaluator[T, E, V]) =
    State(appS[T, E, V](funValue, argValues))
    
  def valuesFromTermsS[T, E, V](terms: List[Term[T]])(env: E)(implicit eval: Evaluator[T, E, V]) =
    terms.foldLeft((env, Seq[V]().success[V])) {
      case ((newEnv, Success(values)), term) =>
        val (newEnv2, value) = eval.valueFromTermS(term)(newEnv)
        (newEnv2, if(!eval.isNoValue(value)) (values :+ value).success else value.failure)
      case ((newEnv, Failure(noValue)), _)   =>
        (newEnv, Failure(noValue))
    }
  
  def valuesFromTerms[T, U, V](terms: List[Term[T]])(implicit eval: Evaluator[T, U, V]) =
    State(valuesFromTermsS[T, U, V](terms))
}