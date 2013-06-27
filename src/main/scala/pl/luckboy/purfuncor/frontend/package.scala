package pl.luckboy.purfuncor
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.util._
import pl.luckboy.purfuncor.common.Arrow
import pl.luckboy.purfuncor.common.Tree

package object frontend
{
  implicit def termIndenting[T, U, V, W]: Indenting[Term[SimpleTerm[T, U, TypeSimpleTerm[V, W]]]] = new Indenting[Term[SimpleTerm[T, U, TypeSimpleTerm[V, W]]]] {
    override def indentedStringFrom(x: Term[SimpleTerm[T, U, TypeSimpleTerm[V, W]]])(n: Int) =
      x match {
        case App(fun, args, _)     =>
          (fun :: args.list).map {
            case term @ Simple((Literal(_) | Var(_)), _) => indentedStringFrom(term)(n + 2)
            case term                                    => "(" + indentedStringFrom(term)(n + 2) + ")"
          }.mkString(" ")
        case Simple(simpleTerm, _) =>
          simpleTermIndenting.indentedStringFrom(simpleTerm)(n)
      }
  }
  
  implicit def simpleTermIndenting[T, U, V, W]: Indenting[SimpleTerm[T, U, TypeSimpleTerm[V, W]]] = new Indenting[SimpleTerm[T, U, TypeSimpleTerm[V, W]]] {
    override def indentedStringFrom(x: SimpleTerm[T, U, TypeSimpleTerm[V, W]])(n: Int) =
      x match {
        case Let(binds, body, lambdaInfo)   =>
          "let\n" + binds.map { (" " * (n + 2)) + bindIndenting.indentedStringFrom(_)(n + 2) }.list.mkString("\n") + "\n" +
          (" " * (n + 2)) + (if(lambdaInfo.toString =/= "")  "/*" + lambdaInfo.toString + "*/ " else "") +
          (" " * n) + "in\n" + (" " * (n + 2)) + termIndenting.indentedStringFrom(body)(n + 2)
        case Lambda(args, body, lambdaInfo) =>
          "\\" + args.map { a => a.typ.map { _ => "(" + a + ")" }.getOrElse(a.toString) + " " }.list.mkString("") +
          (if(lambdaInfo.toString =/= "")  "/*" + lambdaInfo.toString + "*/ " else "") +
          "=> " + termIndenting.indentedStringFrom(body)(n + 2)
        case Var(loc)                       =>
          loc.toString
        case Literal(value)                 =>
          value.toString
        case TypedTerm(term, typ)           =>
          termIndenting.indentedStringFrom(term)(n) + ": " + typeTermShowing
      }
  }

  implicit def bindIndenting[T, U, V, W]: Indenting[Bind[T, U, TypeSimpleTerm[V, W]]] = new Indenting[Bind[T, U, TypeSimpleTerm[V, W]]] {
    override def indentedStringFrom(x: Bind[T, U, TypeSimpleTerm[V, W]])(n: Int) =
      x match {
        case Bind(name, body, _) => name + " = " + termIndenting.indentedStringFrom(body)(n + 2)
      }
  }

  implicit def treeShowing[T, U, V, W, X, Z] = new Showing[Tree[T, AbstractCombinator[U, V, TypeSimpleTerm[W, X]], Z]] {
    override def stringFrom(x: Tree[T, AbstractCombinator[U, V, TypeSimpleTerm[W, X]], Z]) =
      x match {
        case Tree(combs, treeInfo) =>
          combs.groupBy { case (_, comb) => comb.file }.map {
            case (file, combs2) =>
              "// " + file.map { _.getPath() }.getOrElse("<no file>") + "\n\n" +
              combs2.map { case (loc, comb) => comb.toStringForName(loc.toString) + "\n" }.mkString("\n")
          }.mkString("\n") +
          (if(treeInfo.toString =/= "") "//// treeInfo\n" + treeInfo else "")
      }
  }

  implicit def typeTermShowing[T, U]: Showing[Term[TypeSimpleTerm[T, U]]] = new Showing[Term[TypeSimpleTerm[T, U]]] {
    override def stringFrom(x: Term[TypeSimpleTerm[T, U]]): String =
      x match {
        case App(fun, args, _)     =>
          (fun :: args.list).map {
            case term @ Simple((TypeLiteral(_) | TypeVar(_)), _) => stringFrom(term)
            case term                                            => "(" + stringFrom(term) + ")"
          }.mkString(" ")
        case Simple(simpleTerm, _) =>
          simpleTerm.toString
      }
  }
  
  implicit def typeTreeShowing[T, U, V, W] = new Showing[Tree[T, AbstractTypeCombinator[U, V], W]] {
    override def stringFrom(x: Tree[T, AbstractTypeCombinator[U, V], W]) =
      x match {
        case Tree(combs, treeInfo) =>
          combs.groupBy { case (_, comb) => comb.file }.map {
            case (file, combs2) =>
              "// " + file.map { _.getPath() }.getOrElse("<no file>") + "\n\n" +
              combs2.map { case (loc, comb) => comb.toStringForName(loc.toString) + "\n" }.mkString("\n")
          }.mkString("\n") +
          (if(treeInfo.toString =/= "") "//// treeInfo\n" + treeInfo else "")
      }
  }
  
  implicit val kindShowing: Showing[Kind[StarKind[String]]] = new Showing[Kind[StarKind[String]]] {
    override def stringFrom(x: Kind[StarKind[String]]) =
      x match {
        case Arrow(arg, ret, _)         =>
          arg match {
            case Arrow(_, _, _) => "(" + kindShowing.stringFrom(arg) + ") -> " + kindShowing.stringFrom(ret)
            case Star(_, _)     => kindShowing.stringFrom(arg) + " ->" + kindShowing.stringFrom(ret)
          }
        case Star(KindParam(param), _)  =>
          param.toString
        case Star(KindType, _)          =>
          "*"
      }
  }
}