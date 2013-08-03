package pl.luckboy.purfuncor.frontend
import scala.collection.immutable.BitSet
import scala.collection.immutable.IntMap
import scala.util.parsing.input.Position
import scala.annotation.tailrec
import scalaz._
import scalaz.Scalaz._

case class ParamForest[+T](nodes: IntMap[ParamNode], terms: IntMap[T], next: Int, freeParams: List[Int])
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
    } yield copy(nodes.updated(rootParam1, ParamNode(some(rootParam2))))
  
  def allocateParam =
    freeParams.headOption.map {
      p => if(!nodes.contains(p)) some((copy(freeParams = freeParams.tail), p)) else none
    }.getOrElse {
        if(next < Integer.MAX_VALUE) {
          val param = next
          some((copy(nodes = nodes + (param -> ParamNode(None)), next = next + 1), param))
        } else
          none
    }
  
  @tailrec
  private def paramDependencesWithParams(param: Int)(params: BitSet): Option[BitSet] =
    nodes.get(param) match {
      case Some(ParamNode(Some(prev))) => paramDependencesWithParams(prev)(params + prev)
      case Some(ParamNode(None))       => some(params)
      case None                        => none
    }
  
  private def paramDependences(param: Int) = paramDependencesWithParams(param)(BitSet(param))
    
  def freeUnusedParams(usedParams: Set[Int]) =
    usedParams.foldLeft(some(BitSet())) {
      case (Some(ps), p) => paramDependences(p).map { ps | _ }
      case (None, _)     => none
    }.map {
      ps =>
        val unusedParams = nodes.keySet &~ ps
        val newNodes = nodes -- unusedParams
        val newNext = newNodes.keySet.minimum.getOrElse(0)
        copy(nodes = newNodes, terms = terms -- unusedParams, next = newNext, freeParams = (freeParams ++ unusedParams).filter(newNext >))
    }
}

object ParamForest
{
  val empty = ParamForest(IntMap(), IntMap(), 0, Nil)
}

case class ParamNode(prev: Option[Int])