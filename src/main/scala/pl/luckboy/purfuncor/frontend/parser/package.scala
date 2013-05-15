package pl.luckboy.purfuncor.frontend
import pl.luckboy.purfuncor.util._

package object parser
{
  implicit val defIndenting = new Indenting[Def] {
    override def indentedStringFrom(x: Def)(n: Int) =
      x match {
        case ImportDef(sym)                 =>
          "import " + sym
        case CombinatorDef(sym, args, body) =>
          sym + " " + args.map { _ + " " }.mkString("") + "= "+ termIndenting.indentedStringFrom(body)(n + 2)
        case ModuleDef(sym, defs)           =>
          "module " + sym + " {\n" + defs.map { (" " * (n + 2)) + _ }.mkString("\n\n") + (" " * n) + "}"
      }
  }
}