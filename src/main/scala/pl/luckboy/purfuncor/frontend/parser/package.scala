package pl.luckboy.purfuncor.frontend
import pl.luckboy.purfuncor.util._

package object parser
{
  implicit val defIndenting: Indenting[Def] = new Indenting[Def] {
    override def indentedStringFrom(x: Def)(n: Int) =
      x match {
        case ImportDef(sym)                     =>
          "import " + sym
        case CombinatorDef(sym, args, body)     =>
          sym + " " + args.map { a => a.typ.map { _ => "(" + argShowing[TypeSimpleTerm[Symbol, TypeLambdaInfo]].stringFrom(a) + ")" }.getOrElse(argShowing[TypeSimpleTerm[Symbol, TypeLambdaInfo]].stringFrom(a)) + " " }.mkString("") + "= "+ termIndenting[Symbol, LambdaInfo, TypeSimpleTerm[Symbol, TypeLambdaInfo]].indentedStringFrom(body)(n + 2)
        case TypeCombinatorDef(sym, args, body) =>
          "type " + sym + " " + args.map { a => a.kind.map { _ => "(" + a + ")" }.getOrElse(a.toString) + " " }.mkString("") + "= "+ typeTermShowing.stringFrom(body)
        case UnittypeCombinatorDef(n, sym)      =>
          "unittype " + n + " " + sym 
        case ModuleDef(sym, defs)               =>
          "module " + sym + "\n" + (" " * n) + "{\n" + defs.map { d => (" " * (n + 2)) + defIndenting.indentedStringFrom(d)(n + 2) }.mkString("\n\n") + "\n" + (" " * n) + "}"
      }
  }
}