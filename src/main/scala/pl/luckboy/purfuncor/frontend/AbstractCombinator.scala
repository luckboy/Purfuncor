package pl.luckboy.purfuncor.frontend
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.util._
import pl.luckboy.purfuncor.common._

sealed trait AbstractCombinator[+T, +U, +V]
{
  def argCount: Int
  
  def file: Option[java.io.File]
  
  def withFile(file: Option[java.io.File]): AbstractCombinator[T, U, V]
  
  def toStringForName[T2 >: T, U2 >: U, V2 >: V](name: String)(implicit indenting: Indenting[Term[SimpleTerm[T2, U2, V2]]]) = 
    this match {
      case Combinator(args, body, lambdaInfo, _) =>
        name + " " + args.map { a => a.typ.map { _ => "(" + a + ")" }.getOrElse(a.toString) + " " }.mkString("") +
        (if(lambdaInfo.toString =/= "")  "/*" + lambdaInfo.toString + "*/ " else "") +
        "= " + indenting.indentedStringFrom(body)(2)
    }
}

case class Combinator[+T, +U, +V](args: List[Arg[V]], body: Term[SimpleTerm[T, U, V]], lambdaInfo: U, file: Option[java.io.File]) extends AbstractCombinator[T, U, V]
{
  override def argCount = args.size
  
  override def withFile(file: Option[java.io.File]) = copy(file = file)
}