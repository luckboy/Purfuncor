package pl.luckboy.purfuncor.frontend
import scalaz._
import scalaz.Scalaz._

package object resolver
{
  implicit val nameTableMonoid = new Monoid[NameTable] {
    override def zero = NameTable.empty
    
    override def append(f1: NameTable, f2: => NameTable) = NameTable(f1.combNames | f2.combNames, f1.typeCombNames | f2.typeCombNames, f1.moduleNames | f2.moduleNames)
  }

  implicit val nameTreeMonoid = new Monoid[NameTree] {
    override def zero = NameTree.empty
    
    override def append(f1: NameTree, f2: => NameTree) = NameTree(f1.nameTables |+| f2.nameTables)
  }
}