package pl.luckboy.purfuncor.frontend.resolver
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.common.Tree

case class TreeInfo[+T, +U](typeTree: Tree[GlobalSymbol, AbstractTypeCombinator[Symbol, T], U])
{
  override def toString = typeTree.toString
}