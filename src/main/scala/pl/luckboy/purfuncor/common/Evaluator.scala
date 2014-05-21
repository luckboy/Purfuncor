/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.common
import scala.annotation.tailrec
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.util.CollectionUtils._

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
    val (env2, funValue2) = eval.forceS(funValue)(env)
    val argCount = eval.valueArgCount(funValue2)
    if(!eval.isNoValue(funValue2))
      if(argCount === argValues.size) {
        eval.fullyAppS(funValue2, argValues)(env2)
      } else if(argCount > argValues.size) {
        eval.partiallyAppS(funValue2, argValues)(env2)
      } else {
        val (passedArgValues, otherArgValues) = argValues.splitAt(argCount)
        val (env3, retValue) = eval.fullyAppS(funValue2, passedArgValues)(env2)
        appS[T, E, V](retValue, otherArgValues)(env3)
      }
    else
      (env2, funValue2)
  }
  
  def app[T, E, V](funValue: V, argValues: Seq[V])(implicit eval: Evaluator[T, E, V]) =
    State(appS[T, E, V](funValue, argValues))
    
  def valuesFromTermsS[T, E, V](terms: List[Term[T]])(env: E)(implicit eval: Evaluator[T, E, V]) =
    stMapToVectorValidationS(terms) {
      (term, newEnv: E) =>
        val (newEnv2, value) = eval.valueFromTermS(term)(newEnv)
        (newEnv2, if(!eval.isNoValue(value)) value.success else value.failure)
    } (env)
  
  def valuesFromTerms[T, E, V](terms: List[Term[T]])(implicit eval: Evaluator[T, E, V]) =
    State(valuesFromTermsS[T, E, V](terms))
}
