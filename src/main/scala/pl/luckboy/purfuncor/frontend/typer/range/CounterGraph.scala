/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.typer.range
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scalaz._
import scalaz.Scalaz._

case class CounterGraph[T](vertices: Map[T, CounterGraphVertice[T]])
{
  @tailrec
  private def decreaseCountersForQueue(q: Queue[T]): Option[CounterGraph[T]] =
    if(!q.isEmpty) {
      val (vLoc, q2) = q.dequeue
      vertices.get(vLoc) match {
        case Some(v) =>
          val optPair = v.neighborLocs.foldLeft(some((this: CounterGraph[T], q2))) {
            case (Some((g, q3)), uLoc) =>
              g.vertices.get(uLoc).map { 
                u =>
                  if(u.count > 0) 
                    (CounterGraph(g.vertices.updated(uLoc, u.copy(count = u.count - 1))), if(u.count === 1) q3.enqueue(uLoc) else q3)
                  else
                    (g, q3)
              }
            case (None, _)             =>
              none
          }
          optPair match {
            case Some((g2, q4)) => g2.decreaseCountersForQueue(q4)
            case None           => none
          }
        case None    =>
          none
      }
    } else
      some(this)
      
  def decreaseCounters(implicit equal: Equal[T]) = {
    val q = Queue[T]() ++ vertices.flatMap { case (vLoc, v) => if(v.count === 0) Set(vLoc)  else Set[T]() }
    decreaseCountersForQueue(q)
  }
      
  def withEdge(vLoc: T, uLoc: T)(implicit equal: Equal[T]) = 
    if(vLoc =/= uLoc) {
      val v = vertices.get(vLoc).getOrElse(CounterGraphVertice(Vector(), 0))
      CounterGraph(vertices = vertices + (vLoc -> v.copy(neighborLocs = v.neighborLocs :+ uLoc)))
    } else
      this
  
  def withTwoEdges(vLoc: T, uLoc: T)(implicit equal: Equal[T]) = withEdge(vLoc, uLoc).withEdge(uLoc, vLoc)
  
  def withCount(vLoc: T, count: Int) = 
    CounterGraph[T](vertices = vertices + (vLoc -> vertices.get(vLoc).getOrElse(CounterGraphVertice(Vector(), 0)).copy(count = count)))
}

object CounterGraph
{
  def empty[T] = CounterGraph(Map[T, CounterGraphVertice[T]]())
}

case class CounterGraphVertice[T](
    neighborLocs: Vector[T],
    count: Int)
