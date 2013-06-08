package pl.luckboy.purfuncor.backend.interp
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.common.Tree
import Initializer._

object Interpreter
{
  def interpretTree[E, L, C, I, F](tree: Tree[L, C, I])(env: F)(implicit init: Initializer[E, L, C, F]) = {
    val (newEnv, res) = initialize(tree).run(env)
    res.map { _ => env }
  }
}