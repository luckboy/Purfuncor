package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.kinder.Kind
import pl.luckboy.purfuncor.frontend.kinder.NoKind

object TypeResult
{
  def typeResultFromKind[T](kind: Kind) =
    kind match {
      case noKind: NoKind => NoType.fromNoKind[T](noKind).failure
      case _              => kind.success
    }
  
  def typeResultFromKindResult[T, U](res: Validation[NoKind, T]) = res.swap.map(NoType.fromNoKind[U]).swap
}