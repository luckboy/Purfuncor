package pl.luckboy.purfuncor.common
import scala.util.parsing.input.Position

sealed trait AbstractError
{
  override def toString =
    this match {
      case Error(msg, file, pos)            =>
        file.map { _.getPath() }.getOrElse("<no file>") + ": " + pos + ": " + msg
      case err @ FatalError(msg, file, pos) =>
        file.map { _.getPath() }.getOrElse("<no file>") + ": " + pos + ": fatal: " + msg + "\n" +
        err.stackTrace.map { "\tat " + _ }.mkString("\n") + "\n"
    }
}
case class Error(msg: String, file: Option[java.io.File], pos: Position) extends AbstractError
case class FatalError(msg: String, file: Option[java.io.File], pos: Position) extends AbstractError
{
  val stackTrace = Thread.currentThread().getStackTrace().toList
}