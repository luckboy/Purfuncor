package pl.luckboy.purfuncor.frontend
import pl.luckboy.purfuncor.common._

case class Combinator[+T, +U](args: List[Arg], body: Term[SimpleTerm[T, U]], lambdaInfo: U, file: Option[java.io.File])
{
  def toStringForName(name: String) = name + " " + args.map { _ + " " }.mkString("") + "= " + termIndenting.indentedStringFrom(body)(2)
  
  override def toString = toStringForName("<unnamed>")
}