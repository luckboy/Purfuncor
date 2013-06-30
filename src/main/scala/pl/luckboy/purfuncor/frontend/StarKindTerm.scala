package pl.luckboy.purfuncor.frontend

sealed trait StarKindTerm[+T]
case class KindParam[+T](param: T) extends StarKindTerm[T]
case object KindType extends StarKindTerm[Nothing]