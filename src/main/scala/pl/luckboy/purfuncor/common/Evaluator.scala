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
  
  def withPos(res: (E, V))(pos: Position): (E, V)
}

object Evaluator
{
  def evaluateS[T, E, V](term: Term[T])(env: E)(implicit eval: Evaluator[T, E, V]): (E, V) = {
    val res = term match {
      case App(fun, args, _)     =>
        val (env2, funValue) = evaluateS(fun)(env)
        val (env3, argValues) = valuesFromTermNelS(args)(env2)
        appS(funValue, argValues)(env3)
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
    if(argCount == argValues.size) {
      eval.fullyAppS(funValue, argValues)(env)
    } else if(argCount > argValues.size) {
      eval.partiallyAppS(funValue, argValues)(env)
    } else {
      val (passedArgValues, otherArgValues) = argValues.splitAt(argCount)
      val (env2, retValue) = eval.fullyAppS(funValue, passedArgValues)(env)
      appS[T, E, V](retValue, otherArgValues)(env2)
    }
  }
  
  def app[T, E, V](funValue: V, argValues: Seq[V])(implicit eval: Evaluator[T, E, V]) =
    State(appS[T, E, V](funValue, argValues))
    
  def valuesFromTermNelS[T, E, V](terms: NonEmptyList[Term[T]])(env: E)(implicit eval: Evaluator[T, E, V]) =
    terms.list.foldLeft((env, Seq[V]())) {
      case ((newEnv, values), term) =>
        val (newEnv2, value) = eval.valueFromTermS(terms.head)(newEnv)
        (newEnv2, values :+ value)
    }
  
  def valuesFromTermNel[T, U, V](terms: NonEmptyList[Term[T]])(implicit eval: Evaluator[T, U, V]) =
    State(valuesFromTermNelS[T, U, V](terms))
}