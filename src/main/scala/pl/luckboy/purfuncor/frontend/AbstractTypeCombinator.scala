package pl.luckboy.purfuncor.frontend
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._

sealed trait AbstractTypeCombinator[+T, +U]
{
  def argCount =
    this match {
      case TypeCombinator(args, _, _, _, _) => args.size
      case UnittypeCombinator(n, _, _)      => n
    }
  
  def file: Option[java.io.File]

  def withFile(file: Option[java.io.File]): AbstractTypeCombinator[T, U]
  
  def toStringForName(name: String) =
    this match {
      case TypeCombinator(kind, args, body, lambdaInfo, _) =>
        "type " + kind.map { k => "(" + name + ": " + kindShowing.stringFrom(k) + ")" }.getOrElse(name) + " " + 
        args.map { a => a.kind.map { _ => "(" + a + ")" }.getOrElse(a.toString) + " " }.mkString("") + 
        (if(lambdaInfo.toString =/= "")  "/*" + lambdaInfo.toString + "*/ " else "") +
        "= " + typeTermShowing.stringFrom(body)
      case UnittypeCombinator(n, kind, _)           =>
        "unittype " + n + " " + kind.map { k => "(" + name + ": " + kindShowing.stringFrom(k) + ")" }.getOrElse(name)
    }
}
case class TypeCombinator[+T, +U](kind: Option[Kind[StarKind[String]]], args: List[TypeArg], body: Term[TypeSimpleTerm[T, U]], lambdaInfo: U, file: Option[java.io.File]) extends AbstractTypeCombinator[T, U]
{ 
  override def withFile(file: Option[java.io.File]) = copy(file = file)
}
case class UnittypeCombinator[+T, +U](n: Int,  kind: Option[Kind[StarKind[String]]], file: Option[java.io.File]) extends AbstractTypeCombinator[T, U]
{
  override def withFile(file: Option[java.io.File]) = copy(file = file)
}