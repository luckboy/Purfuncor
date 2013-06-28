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
  
  def toStringForName[V2 >: V](name: String)(implicit showing: Showing[Term[V2]]): String = 
    this match {
      case Combinator(args, body, lambdaInfo, _) =>
        name + " " + args.map { a => a.typ.map { _ => "(" + argShowing(showing).stringFrom(a) + ")" }.getOrElse(argShowing(showing).stringFrom(a)) + " " }.mkString("") +
        (if(lambdaInfo.toString =/= "")  "/*" + lambdaInfo.toString + "*/ " else "") +
        "= " + termIndenting(showing).indentedStringFrom(body)(2)
    }
}

case class Combinator[+T, +U, +V](args: List[Arg[V]], body: Term[SimpleTerm[T, U, V]], lambdaInfo: U, file: Option[java.io.File]) extends AbstractCombinator[T, U, V]
{
  override def argCount = args.size
  
  override def withFile(file: Option[java.io.File]) = copy(file = file)
}