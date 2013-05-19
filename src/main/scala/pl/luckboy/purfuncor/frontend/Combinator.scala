package pl.luckboy.purfuncor.frontend
import pl.luckboy.purfuncor.common._

case class Combinator[+T, +U](loc: T, args: List[Arg], body: Term[SimpleTerm[T, U]], letInfo: U)