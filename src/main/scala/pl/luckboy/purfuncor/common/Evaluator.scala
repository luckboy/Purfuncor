package pl.luckboy.purfuncor.common
import scala.annotation.tailrec
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._

trait Evaluator[-T, U, V]
{
  def evaluateSimpleTerm(simpleTerm: T)(env: U): (U, V)
  
  def valueFromTerm(term: Term[T])(env: U): (U, V)
  
  def valueArgCount(value: V): Int
  
  def fullyApp(funValue: V, argValues: Seq[V])(env: U): (U, V)
  
  def partiallyApp(funValue: V, argValues: Seq[V])(env: U): (U, V)
  
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
        eval.evaluateSimpleTerm(simpleTerm)(env)
    }
    eval.withPos(res)(term.pos)
  }
    
  def evaluate[T, U, V](term: Term[T])(implicit eval: Evaluator[T, U, V]) =
    State[U, V](evaluateS[T, U, V](term))
    
  @tailrec
  def appS[T, U, V](funValue: V, argValues: Seq[V])(env: U)(implicit eval: Evaluator[T, U, V]): (U, V) = {
    val argCount = eval.valueArgCount(funValue)
    if(argCount == argValues.size) {
      eval.fullyApp(funValue, argValues)(env)
    } else if(argCount > argValues.size) {
      eval.partiallyApp(funValue, argValues)(env)
    } else {
      val (passedArgValues, otherArgValues) = argValues.splitAt(argCount)
      val (env2, retValue) = eval.fullyApp(funValue, passedArgValues)(env)
      appS[T, U, V](retValue, otherArgValues)(env2)
    }
  }
    
  def valuesFromTermNelS[T, U, V](terms: NonEmptyList[Term[T]])(env: U)(implicit eval: Evaluator[T, U, V]) =
    terms.list.foldLeft((env, Seq[V]())) {
      case ((newEnv, values), term) =>
        val (newEnv2, value) = eval.valueFromTerm(terms.head)(newEnv)
        (newEnv2, values :+ value)
    }
}