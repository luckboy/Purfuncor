package pl.luckboy.purfuncor.common
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._

object Result
{
  def resultForFile[T](res: ValidationNel[AbstractError, T], file: Option[java.io.File]) =
    res.swapped { _.map { _.map { _.withFile(file) } } }
 
  def resultWithPos[T](res: ValidationNel[AbstractError, T], pos: Position) =
    res.swapped { _.map { _.map { _.withPos(pos) } } }
}