package pl.luckboy.purfuncor.frontend.instant
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.frontend.typer.InferredTypeTable

case class TreeInfo[+T, U, V](
    treeInfo: T,
    typeTable: InferredTypeTable[U, V],
    instTree: InstanceTree[AbstractPolyFunction[U], V, GlobalInstance[U]],
    instArgTable: InstanceArgTable[U, V])