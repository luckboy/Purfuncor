package pl.luckboy.purfuncor.frontend
import scala.util.parsing.input.Position

case class Error(file: Option[java.io.File], pos: Position, msg: String)
{
  override def toString = file.map { _.getPath() }.getOrElse("<no file>") + ": " + pos + ": " + msg
}