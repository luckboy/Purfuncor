package pl.luckboy.purfuncor.frontend
import pl.luckboy.purfuncor.common._

case class Combinator[+T, +U, +V](loc: T, args: List[Arg], body: Term[SimpleTerm[U, V]], letInfo: V, file: Option[java.io.File])