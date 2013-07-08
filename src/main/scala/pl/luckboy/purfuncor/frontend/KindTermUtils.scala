package pl.luckboy.purfuncor.frontend
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.common.Arrow

object KindTermUtils
{
  def kindParamsFromKindTerm[T](term: KindTerm[StarKindTerm[T]]): Set[T] =
    term match {
      case Arrow(arg, ret, _)        => kindParamsFromKindTerm(arg) | kindParamsFromKindTerm(ret)
      case Star(KindParam(param), _) => Set(param)
      case _                         => Set()
    }
}