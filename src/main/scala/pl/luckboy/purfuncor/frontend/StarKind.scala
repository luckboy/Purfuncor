package pl.luckboy.purfuncor.frontend

sealed trait StarKind[+T]
case class KindParam[+T](param: T) extends StarKind[T]
case object KindType extends StarKind[Nothing]