package pl.luckboy.purfuncor.frontend
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._

case class SelectConstructInstance[+T, +U](
    supertype: Term[TypeSimpleTerm[T, U]],
    types: NonEmptyList[Term[TypeSimpleTerm[T, U]]],
    file: Option[java.io.File])
{
  override def toString = 
    "instance select " + typeTermShowing.stringFrom(supertype) + " construct {\n" +
    types.map { t => (" " * 2) + typeTermShowing.stringFrom(t) }.list.mkString("\n") +
    "\n}"
}