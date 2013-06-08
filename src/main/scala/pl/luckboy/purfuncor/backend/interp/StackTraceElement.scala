package pl.luckboy.purfuncor.backend.interp
import scala.util.parsing.input.Position
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol

case class StackTraceElement(file: Option[java.io.File], combSym: Option[GlobalSymbol], pos: Position)
{
  override def toString = file.map { _.getPath() }.getOrElse("<no file>") + ": " + combSym.map { _.toString }.getOrElse("<lambda>") + ": " + pos
}