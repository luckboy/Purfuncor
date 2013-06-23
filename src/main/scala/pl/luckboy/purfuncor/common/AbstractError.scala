package pl.luckboy.purfuncor.common
import scala.util.parsing.input.Position

sealed trait AbstractError
{
  def msg: String
  
  def withFile(file: Option[java.io.File]): AbstractError
  
  override def toString =
    this match {
      case Error(msg, file, pos)            =>
        file.map { _.getPath() }.getOrElse("<no file>") + ": " + pos + ": " + msg
      case err @ FatalError(msg, file, pos) =>
        file.map { _.getPath() }.getOrElse("<no file>") + ": " + pos + ": fatal: " + msg + "\n" +
        err.stackTrace.map { "\tat " + _ }.mkString("\n") + "\n"
      case IOError(msg, file)               =>
        file.map { _.getPath() }.getOrElse("<no file>") + ": io error: " + msg
    }
}
case class Error(msg: String, file: Option[java.io.File], pos: Position) extends AbstractError
{
  override def withFile(file: Option[java.io.File]) = Error(msg, file, pos)
}
case class FatalError(msg: String, file: Option[java.io.File], pos: Position) extends AbstractError
{
  val stackTrace = Thread.currentThread().getStackTrace().toList

  override def withFile(file: Option[java.io.File]) = FatalError(msg, file, pos)
}
case class IOError(msg: String, file: Option[java.io.File]) extends AbstractError
{
  override def withFile(file: Option[java.io.File]) = IOError(msg, file)
}