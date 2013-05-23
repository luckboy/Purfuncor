package pl.luckboy.purfuncor.common
import pl.luckboy.purfuncor.util._

case class Tree[T, +U, +V](combs: Map[T, U], treeInfo: V)(implicit treeShowing: Showing[Tree[T, U, V]])
{
  override def toString = treeShowing.stringFrom(this)
}