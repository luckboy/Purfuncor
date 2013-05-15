package pl.luckboy.purfuncor
import scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.util._

package object frontend
{
  implicit def termIndenting[T, U]: Indenting[Term[SimpleTerm[T, U]]] = new Indenting[Term[SimpleTerm[T, U]]] {
    override def indentedStringFrom(x: Term[SimpleTerm[T, U]])(n: Int) =
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
  
  implicit def simpleTermIndenting[T, U]: Indenting[SimpleTerm[T, U]] = new Indenting[SimpleTerm[T, U]] {
    override def indentedStringFrom(x: SimpleTerm[T, U])(n: Int) =
      x match {
        case Let(binds, body, letInfo)   =>
          "let\n" + binds.map { (" " * (n + 2)) + localBindIndenting.indentedStringFrom(_)(n + 2) }.list.mkString("\n") + "\n" +
          (" " * n) + "in\n" + (" " * (n + 2)) + termIndenting.indentedStringFrom(body)(n + 2)
        case Lambda(args, body, letInfo) =>
          "\\" + args.map { _ + " " }.list.mkString("") + "=> " + termIndenting.indentedStringFrom(body)(n + 2)
        case Var(loc)                    =>
          loc.toString
        case Literal(value)              =>
          value.toString
      }
  }

  implicit def localBindIndenting[T, U]: Indenting[LocalBind[T, U]] = new Indenting[LocalBind[T, U]] {
    override def indentedStringFrom(x: LocalBind[T, U])(n: Int) =
      x match {
        case LocalBind(name, body, _) => name + " = " + termIndenting.indentedStringFrom(body)(n + 2)
      }
  }
}