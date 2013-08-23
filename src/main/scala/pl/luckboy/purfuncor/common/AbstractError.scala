package pl.luckboy.purfuncor.common
import scala.util.parsing.input.Position

sealed trait AbstractError
{
  def msg: String
  
  def withFile(file: Option[java.io.File]): AbstractError
  
  def withPos(pos: Position): AbstractError
  
  override def toString =
    this match {
      case Error(msg, file, pos)            =>
        file.map { _.getPath() }.getOrElse("<no file>") + ": " + pos + ": " + msg
      case err @ FatalError(msg, file, pos, _) =>
        file.map { _.getPath() }.getOrElse("<no file>") + ": " + pos + ": fatal: " + msg + "\n" +
        err.stackTrace.map { (" " * 8) + "at " + _ }.mkString("\n") + "\n"
      case IOError(msg, file)               =>
        file.map { _.getPath() }.getOrElse("<no file>") + ": io error: " + msg
    }
}
case class Error(msg: String, file: Option[java.io.File], pos: Position) extends AbstractError
{
  override def withFile(file: Option[java.io.File]) = Error(msg, file, pos)

  override def withPos(pos: Position) = Error(msg, file, pos)
}
case class FatalError(msg: String, file: Option[java.io.File], pos: Position, stackTrace: List[java.lang.StackTraceElement] = Thread.currentThread().getStackTrace().toList) extends AbstractError
{
  override def withFile(file: Option[java.io.File]) = copy(file = file)

  override def withPos(pos: Position) = copy(pos = pos)
}
case class IOError(msg: String, file: Option[java.io.File]) extends AbstractError
{
  override def withFile(file: Option[java.io.File]) = IOError(msg, file)

  override def withPos(pos: Position) = this
}