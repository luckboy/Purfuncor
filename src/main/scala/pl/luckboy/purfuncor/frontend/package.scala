package pl.luckboy.purfuncor
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.util._
import pl.luckboy.purfuncor.common.Tree

package object frontend
{
  implicit def termIndenting[T, U, V]: Indenting[Term[SimpleTerm[T, U, V]]] = new Indenting[Term[SimpleTerm[T, U, V]]] {
    override def indentedStringFrom(x: Term[SimpleTerm[T, U, V]])(n: Int) =
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
  
  implicit def simpleTermIndenting[T, U, V]: Indenting[SimpleTerm[T, U, V]] = new Indenting[SimpleTerm[T, U, V]] {
    override def indentedStringFrom(x: SimpleTerm[T, U, V])(n: Int) =
      x match {
        case Let(binds, body, letInfo)   =>
          "let\n" + binds.map { (" " * (n + 2)) + bindIndenting.indentedStringFrom(_)(n + 2) }.list.mkString("\n") + "\n" +
          (" " * n) + "in\n" + (" " * (n + 2)) + termIndenting.indentedStringFrom(body)(n + 2)
        case Lambda(args, body, letInfo) =>
          "\\" + args.map { _ + " " }.list.mkString("") +
          (if(letInfo.toString =/= "")  "/*" + letInfo.toString + "*/ " else "") +
          "=> " + termIndenting.indentedStringFrom(body)(n + 2)
        case Var(loc)                    =>
          loc.toString
        case Literal(value)              =>
          value.toString
      }
  }

  implicit def bindIndenting[T, U, V]: Indenting[Bind[T, U, V]] = new Indenting[Bind[T, U, V]] {
    override def indentedStringFrom(x: Bind[T, U, V])(n: Int) =
      x match {
        case Bind(name, body, _) => name + " = " + termIndenting.indentedStringFrom(body)(n + 2)
      }
  }
  
  implicit def treeShowing[T, U, V, W, X] = new Showing[Tree[T, Combinator[U, V, W], X]] {
    override def stringFrom(x: Tree[T, Combinator[U, V, W], X]) =
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
}