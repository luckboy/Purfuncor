package pl.luckboy.purfuncor.frontend
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.common.Tree

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
  
  implicit def treeInfoGlobalSymbolTabular[T, U] = new GlobalSymbolTabular[TreeInfo[T, U], GlobalSymbol] {
    override def getGlobalLocationFromTable(table: TreeInfo[T, U])(sym: GlobalSymbol) = some(sym)

    override def getGlobalSymbolFromTable(table: TreeInfo[T, U])(sym: GlobalSymbol) = some(sym)
  }

  implicit val typeTreeInfoGlobalSymbolTabular = new GlobalSymbolTabular[TypeTreeInfo, GlobalSymbol] {
    override def getGlobalLocationFromTable(table: TypeTreeInfo)(sym: GlobalSymbol) = some(sym)

    override def getGlobalSymbolFromTable(table: TypeTreeInfo)(sym: GlobalSymbol) = some(sym)
  }
  
  implicit val parserLambdaInfoLocalSymbolTabular = new LocalSymbolTabular[parser.LambdaInfo, LocalSymbol] {
    override def getLocalLocationFromTable(table: parser.LambdaInfo)(sym: LocalSymbol) = some(sym)
    
    override def getLocalSymbolFromTable(table: parser.LambdaInfo)(sym: LocalSymbol) = some(sym)
  }
  
  implicit val parserTypeLambdaInfoLocalSymbolTabular = new LocalSymbolTabular[parser.TypeLambdaInfo, LocalSymbol] {
    override def getLocalLocationFromTable(table: parser.TypeLambdaInfo)(sym: LocalSymbol) = some(sym)
    
    override def getLocalSymbolFromTable(table: parser.TypeLambdaInfo)(sym: LocalSymbol) = some(sym)    
  }
  
  implicit def symbolEqual[T <: Symbol] = new Equal[T] {
    override def equal(a1: T, a2: T) =
      (a1, a2) match {
        case (GlobalSymbol(names1), GlobalSymbol(names2)) => names1 === names2
        case (LocalSymbol(name1), LocalSymbol(name2))     => name1 === name2
        case (_, _)                                       => false
      }
  }
}