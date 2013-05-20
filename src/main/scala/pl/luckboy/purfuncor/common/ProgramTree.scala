package pl.luckboy.purfuncor.common

case class ProgramTree[T, +U, +V](combs: Map[T, U], treeInfo: V)