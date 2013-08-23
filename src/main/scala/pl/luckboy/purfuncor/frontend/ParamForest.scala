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
}

object ParamForest
{
  val empty = ParamForest(IntMap(), IntMap(), 0)
}

case class ParamNode(prev: Option[Int])