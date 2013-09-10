package pl.luckboy.purfuncor.typer
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.common.Evaluator._
import pl.luckboy.purfuncor.common.Initializer._

object Typer
{
  def interpretTypeTermS[T, E, V](term: Term[T])(env: E)(implicit eval: Evaluator[T, E, V]) =
    evaluateS(term)(env)
  
  def interpretTypeTerm[T, E, V](term: Term[T])(implicit eval: Evaluator[T, E, V]) =
    State(interpretTypeTermS[T, E, V](term))
  
  def interpretTypeTreeS[E, L, C, I, F](tree: Tree[L, C, I])(env: F)(implicit init: Initializer[E, L, C, F]) =
    initializeS(tree)(env)
  
  def interpretTypeTree[E, L, C, I, F](tree: Tree[L, C, I])(implicit init: Initializer[E, L, C, F]) =
    State(interpretTypeTreeS[E, L, C, I, F](tree))
}