package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._

case class TreeInfo[+T, U, V](treeInfo: T, typeTable: InferredTypeTable[U, V])