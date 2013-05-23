package pl.luckboy.purfuncor.frontend
import scalaz._
import scalaz.Scalaz._

package object resolver
{
  implicit val nameTableMonoid = new Monoid[NameTable] {
    override def zero = NameTable.empty
    
    override def append(f1: NameTable, f2: => NameTable) = NameTable(f1.combNames | f2.combNames, f1.moduleNames | f2.moduleNames)
  }

  implicit val nameTreeMonoid = new Monoid[NameTree] {
    override def zero = NameTree.empty
    
    override def append(f1: NameTree, f2: => NameTree) = NameTree(f1.nameTables |+| f2.nameTables)
  }
  
  implicit def symbolEqual[T <: Symbol] = new Equal[T] {
    override def equal(f1: T, f2: T) =
      (f1, f2) match {
        case (GlobalSymbol(names1), GlobalSymbol(names2)) => names1 === names2
        case (LocalSymbol(name1), LocalSymbol(name2))     => name1 === name2
        case _                                            => false
      }
  }
}