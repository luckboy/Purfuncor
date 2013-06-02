package pl.luckboy.purfuncor.common
import scala.annotation.tailrec
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._

trait Evaluator[-T, U, V]
{
  def evaluateSimpleTermS(simpleTerm: T)(env: U): (U, V)
  
  def valueFromTermS(term: Term[T])(env: U): (U, V)
  
  def valueArgCount(value: V): Int
  
  def fullyAppS(funValue: V, argValues: Seq[V])(env: U): (U, V)
  
  def partiallyAppS(funValue: V, argValues: Seq[V])(env: U): (U, V)
  
  def withPos(res: (U, V))(pos: Position): (U, V)
}

object Evaluator
{
  def evaluateS[T, U, V](term: Term[T])(env: U)(implicit eval: Evaluator[T, U, V]): (U, V) = {
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
    
  def evaluate[T, U, V](term: Term[T])(implicit eval: Evaluator[T, U, V]) =
    State(evaluateS[T, U, V](term))
    
  @tailrec
  def appS[T, U, V](funValue: V, argValues: Seq[V])(env: U)(implicit eval: Evaluator[T, U, V]): (U, V) = {
    val argCount = eval.valueArgCount(funValue)
    if(argCount == argValues.size) {
      eval.fullyAppS(funValue, argValues)(env)
    } else if(argCount > argValues.size) {
      eval.partiallyAppS(funValue, argValues)(env)
    } else {
      val (passedArgValues, otherArgValues) = argValues.splitAt(argCount)
      val (env2, retValue) = eval.fullyAppS(funValue, passedArgValues)(env)
      appS[T, U, V](retValue, otherArgValues)(env2)
    }
  }
  
  def app[T, U, V](funValue: V, argValues: Seq[V])(implicit eval: Evaluator[T, U, V]) =
    State(appS[T, U, V](funValue, argValues))
    
  def valuesFromTermNelS[T, U, V](terms: NonEmptyList[Term[T]])(env: U)(implicit eval: Evaluator[T, U, V]) =
    terms.list.foldLeft((env, Seq[V]())) {
      case ((newEnv, values), term) =>
        val (newEnv2, value) = eval.valueFromTermS(terms.head)(newEnv)
        (newEnv2, values :+ value)
    }
  
  def valuesFromTermNel[T, U, V](terms: NonEmptyList[Term[T]])(implicit eval: Evaluator[T, U, V]) =
    State(valuesFromTermNelS[T, U, V](terms))
}