package pl.luckboy.purfuncor.frontend.resolver
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.common.Tree

case class TreeInfo[+T, +U](
    typeTree: Tree[GlobalSymbol, AbstractTypeCombinator[Symbol, T], U],
    insts: Map[GlobalSymbol, List[Instance[GlobalSymbol]]],
    selectConstructInsts: List[SelectConstructInstance[Symbol, T]])
{
  override def toString =
    "//// typeTree\n" +
    typeTree.toString + "\n" +
    "//// insts\n" +
    insts.flatMap { case (s, is) => is.map { (s, _) } }.groupBy { case (_, i) => i.file }.map {
      case (file, insts2) =>
        "// " + file.map { _.getPath() }.getOrElse("<no file>") + "\n\n"
        insts2.map { case (s, i) => i.toStringForName(s.toString) + "\n" }.mkString("\n")
    }.mkString("\n") + "\n" +
    "//// selectConstructInsts\n" +
    selectConstructInsts.groupBy { _.file }.map {
      case (file, selectConstructInsts2) =>
        "// " + file.map { _.getPath() }.getOrElse("<no file>") + "\n\n" +
        selectConstructInsts2.map { _ + "\n" }.mkString("\n")
    }.mkString("\n")
}