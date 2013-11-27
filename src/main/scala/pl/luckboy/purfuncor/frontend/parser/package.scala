package pl.luckboy.purfuncor.frontend
import pl.luckboy.purfuncor.util._

package object parser
{
  implicit val defIndenting: Indenting[Def] = new Indenting[Def] {
    override def indentedStringFrom(x: Def)(n: Int) =
      x match {
        case ImportDef(sym)                               =>
          "import " + sym
        case CombinatorDef(sym, typ, args, body)          =>
          typ.map { t => "(" + sym + ": " + typeTermShowing.stringFrom(t) + ")" }.getOrElse(sym) + " " +
          args.map { a => a.typ.map { _ => "(" + argShowing[TypeSimpleTerm[Symbol, TypeLambdaInfo]].stringFrom(a) + ")" }.getOrElse(argShowing[TypeSimpleTerm[Symbol, TypeLambdaInfo]].stringFrom(a)) + " " }.mkString("") + "= "+ termIndenting[Symbol, LambdaInfo, TypeSimpleTerm[Symbol, TypeLambdaInfo]].indentedStringFrom(body)(n + 2)
        case PolyCombinatorDef(sym, typ)                  =>
          "poly " + sym + typ.map { t => ": " + typeTermShowing.stringFrom(t) }.getOrElse("")  
        case TypeCombinatorDef(sym, kind, args, body)     =>
          "type " + kind.map { k => "(" + sym + ": " + stringKindTermShowing.stringFrom(k) + ")" }.getOrElse(sym) + " " +
          args.map { a => a.kind.map { _ => "(" + a + ")" }.getOrElse(a.toString) + " " }.mkString("") + "= "+ typeTermShowing.stringFrom(body)
        case UnittypeCombinatorDef(n, sym, kind)          =>
          "unittype " + n + " " + kind.map { k => "(" + sym + ": " + stringKindTermShowing.stringFrom(k) + ")" }.getOrElse(sym)
        case ModuleDef(sym, defs)                         =>
          "module " + sym + "\n" + (" " * n) + "{\n" + defs.map { d => (" " * (n + 2)) + defIndenting.indentedStringFrom(d)(n + 2) }.mkString("\n\n") + "\n" + (" " * n) + "}"
        case InstanceDef(sym, instanceCombSym)            =>
          "instance " + sym + " => " + instanceCombSym
        case SelectConstructInstanceDef(supertype, types) =>
          "instance select " + typeTermShowing.stringFrom(supertype) + " construct {\n" + types.map { t => (" " * (n + 2)) + typeTermShowing.stringFrom(t) }.list.mkString("\n") + "\n" + (" " * n) + "}"
      }
  }
}