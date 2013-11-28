package pl.luckboy.purfuncor.frontend.resolver
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.common.Tree

case class TreeInfo[+T, +U](
    typeTree: Tree[GlobalSymbol, AbstractTypeCombinator[Symbol, T], U],
    instances: Map[GlobalSymbol, List[Instance[GlobalSymbol]]],
    selectConstructInstances: List[SelectConstructInstance[Symbol, T]])
{
  override def toString =
    "//// typeTree\n" +
    typeTree.toString + "\n\n" +
    "//// instances\n" +
    instances.flatMap { case (s, is) => is.map { (s, _) } }.groupBy { case (_, i) => i.file }.map {
      case (file, instances) =>
        "// " + file.map { _.getPath() }.getOrElse("<no file>") + "\n\n"
        instances.map { case (s, i) => i.toStringForName(s.toString) + "\n" }.mkString("\n")
    }.mkString("\n") + "\n" +
    "//// selectConstructInstances\n" +
    selectConstructInstances.groupBy { _.file }.map {
      case (file, selectConstructInstances) =>
        "// " + file.map { _.getPath() }.getOrElse("<no file>") + "\n\n" +
        selectConstructInstances.map { _ + "\n" }.mkString("\n")
    }.mkString("\n") + "\n"
}