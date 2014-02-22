/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend
import scala.collection.immutable.IntMap
import scala.util.parsing.input.Position
import scala.annotation.tailrec
import scalaz._
import scalaz.Scalaz._

case class ParamForest[+T](nodes: IntMap[ParamNode], terms: IntMap[T], next: Int)
{
  def containsTerm(param: Int) = terms.contains(param)
  
  def getTerm(param: Int) = terms.get(param)
  
  @tailrec
  final def findRootParam(param: Int): Option[Int] =
    nodes.get(param) match {
      case Some(ParamNode(Some(prev))) => findRootParam(prev)
      case Some(ParamNode(None))       => some(param)
      case None                        => none
    }
  
  def replaceParam[T2 >: T](param: Int, term: T2) = findRootParam(param).map { rp => copy(terms = terms.updated(rp, term)) }
  
  def unionParams(param1: Int, param2: Int) =
    for {
      rootParam1 <- findRootParam(param1)
      rootParam2 <- findRootParam(param2)
    } yield {
      if(rootParam1 =/= rootParam2) (copy(nodes.updated(rootParam1, ParamNode(some(rootParam2)))), true) else (this, false)
    }
  
  def allocateParam = {
    if(next < Integer.MAX_VALUE) {
      val param = next
      some((copy(nodes = nodes + (param -> ParamNode(None)), next = next + 1), param))
    } else
      none
  }
  
  def allocatedParams = nodes.keySet
  
  def reverseParamMap(paramMap: Map[Int, Int]) = {
    nodes.keySet.foldLeft(some((paramMap.map { _.swap }.toMap, Set[Int]()))) {
      case (Some((reversedParamMap, markedParams)), param) =>        
        findParams(param)(Set[Int](), markedParams).map {
          params =>
            val tmpReversedParamMap = reversedParamMap.find { p => params.contains(p._1) }.toList.flatMap { p => params.map { (_, p._2) } }.toMap
            (reversedParamMap ++ tmpReversedParamMap, markedParams | params)
        }
      case (None, _)                                       =>
        none
    }.map { _._1 }
  }
  
  @tailrec
  private def findParams(param: Int)(params: Set[Int], markedParams: Set[Int]): Option[Set[Int]] =
    nodes.get(param) match {
      case Some(ParamNode(Some(prev))) => 
        if(!markedParams.contains(param))
          findParams(prev)(params + param, markedParams)
        else
          some(params + param)
      case Some(ParamNode(None))       =>
        some(params + param)
      case None                        =>
        none
    }
  
  def findParamUnions(params: Set[Int]) =
    params.foldLeft(some(IntMap[Set[Int]]())) {
      case (Some(unions), param) =>
        findRootParam(param).map { rp => unions.updated(rp, unions.getOrElse(rp, Set()) + param) }
      case (None, _)             =>
        none
    }
  
  def findParamUnionsWithTerms(params: Set[Int]) =
    findParamUnions(params).map { _.map { case (rp, ps) => (rp, (ps, getTerm(rp))) } }
}

object ParamForest
{
  val empty = ParamForest(IntMap(), IntMap(), 0)
}

case class ParamNode(prev: Option[Int])
