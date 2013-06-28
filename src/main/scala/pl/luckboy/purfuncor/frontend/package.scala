package pl.luckboy.purfuncor
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.util._
import pl.luckboy.purfuncor.common.Arrow
import pl.luckboy.purfuncor.common.Tree

package object frontend
{
  implicit def termIndenting[T, U, V](implicit showing: Showing[Term[V]]): Indenting[Term[SimpleTerm[T, U, V]]] = new Indenting[Term[SimpleTerm[T, U, V]]] {
    override def indentedStringFrom(x: Term[SimpleTerm[T, U, V]])(n: Int) =
      x match {
        case App(fun, args, _)     =>
          (fun :: args.list).map {
            case term @ Simple((Literal(_) | Var(_)), _) => indentedStringFrom(term)(n + 2)
            case term                                    => "(" + indentedStringFrom(term)(n + 2) + ")"
          }.mkString(" ")
        case Simple(simpleTerm, _) =>
          simpleTermIndenting(showing).indentedStringFrom(simpleTerm)(n)
      }
  }
  
  implicit def simpleTermIndenting[T, U, V](implicit showing: Showing[Term[V]]): Indenting[SimpleTerm[T, U, V]] = new Indenting[SimpleTerm[T, U, V]] {
    override def indentedStringFrom(x: SimpleTerm[T, U, V])(n: Int) =
      x match {
        case Let(binds, body, lambdaInfo)   =>
          "let\n" + binds.map { (" " * (n + 2)) + bindIndenting(showing).indentedStringFrom(_)(n + 2) }.list.mkString("\n") + "\n" +
          (" " * (n + 2)) + (if(lambdaInfo.toString =/= "")  "/*" + lambdaInfo.toString + "*/ " else "") +
          (" " * n) + "in\n" + (" " * (n + 2)) + termIndenting(showing).indentedStringFrom(body)(n + 2)
        case Lambda(args, body, lambdaInfo) =>
          "\\" + args.map { a => a.typ.map { _ => "(" + argShowing(showing).stringFrom(a) + ")" }.getOrElse(argShowing(showing).stringFrom(a)) + " " }.list.mkString("") +
          (if(lambdaInfo.toString =/= "")  "/*" + lambdaInfo.toString + "*/ " else "") +
          "=> " + termIndenting(showing).indentedStringFrom(body)(n + 2)
        case Var(loc)                       =>
          loc.toString
        case Literal(value)                 =>
          value.toString
        case TypedTerm(term, typ)           =>
          termIndenting(showing).indentedStringFrom(term)(n) + ": " + typeTermShowing
      }
  }

  implicit def bindIndenting[T, U, V](implicit showing: Showing[Term[V]]): Indenting[Bind[T, U, V]] = new Indenting[Bind[T, U, V]] {
    override def indentedStringFrom(x: Bind[T, U, V])(n: Int) =
      x match {
        case Bind(name, body, _) => name + " = " + termIndenting(showing).indentedStringFrom(body)(n + 2)
      }
  }
  
  implicit def argShowing[V](implicit showing: Showing[Term[V]]): Showing[Arg[V]] = new Showing[Arg[V]] {
    override def stringFrom(x: Arg[V]) =
      x match {
        case Arg(name, typ, _) => name.map { _.toString }.getOrElse("_") + typ.map { t => ": " + showing.stringFrom(t) }.getOrElse("")
      }
  }

  implicit def treeShowing[T, U, V, W, X](implicit showing: Showing[Term[W]]) = new Showing[Tree[T, AbstractCombinator[U, V, W], X]] {
    override def stringFrom(x: Tree[T, AbstractCombinator[U, V, W], X]) =
      x match {
        case Tree(combs, treeInfo) =>
          combs.groupBy { case (_, comb) => comb.file }.map {
            case (file, combs2) =>
              "// " + file.map { _.getPath() }.getOrElse("<no file>") + "\n\n" +
              combs2.map { case (loc, comb) => comb.toStringForName(loc.toString)(showing) + "\n" }.mkString("\n")
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
            case Star(_, _)     => kindShowing.stringFrom(arg) + " -> " + kindShowing.stringFrom(ret)
          }
        case Star(KindParam(param), _)  =>
          param.toString
        case Star(KindType, _)          =>
          "*"
      }
  }
}