package pl.luckboy.purfuncor.frontend
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._

sealed trait AbstractTypeCombinator[+T, +U]
{
  def argCount: Int
  
  def file: Option[java.io.File]

  def withFile(file: Option[java.io.File]): AbstractTypeCombinator[T, U]
  
  def toStringForName(name: String) =
    this match {
      case TypeCombinator(args, body, lambdaInfo, _) =>
        "type " + name + " " + args.map { a => a.kind.map { _ => "(" + a + ")" }.getOrElse(a.toString) + " " }.mkString("") + 
        (if(lambdaInfo.toString =/= "")  "/*" + lambdaInfo.toString + "*/ " else "") +
        "= " + body
      case UnittypeCombinator(argCount, _)           =>
        "unittype " + argCount + " " + name
    }
}
case class TypeCombinator[+T, +U](args: List[TypeArg], body: Term[TypeSimpleTerm[T, U]], lambdaInfo: U, file: Option[java.io.File]) extends AbstractTypeCombinator[T, U]
{
  override def argCount = args.size
  
  override def withFile(file: Option[java.io.File]) = copy(file = file)
}
case class UnittypeCombinator[+T, +U](argCount: Int, file: Option[java.io.File]) extends AbstractTypeCombinator[T, U]
{
  override def withFile(file: Option[java.io.File]) = copy(file = file)
}