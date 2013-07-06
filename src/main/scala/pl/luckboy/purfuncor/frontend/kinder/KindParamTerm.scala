package pl.luckboy.purfuncor.frontend.kinder
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

sealed trait KindParamTerm
{
  def kindTerm: KindTerm[StarKindTerm[Int]]
}
case class InferredParamTerm(kindTerm: KindTerm[StarKindTerm[Int]]) extends KindParamTerm
case class DefinedKindParamTerm(kindTerm: KindTerm[StarKindTerm[Int]]) extends KindParamTerm