package pl.luckboy.purfuncor.common

case class Tree[T, +U, +V](combs: Map[T, U], treeInfo: V)