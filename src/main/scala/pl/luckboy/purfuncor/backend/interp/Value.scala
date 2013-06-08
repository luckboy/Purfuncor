package pl.luckboy.purfuncor.backend.interp
import scala.util.parsing.input.Position
import pl.luckboy.purfuncor.frontend
import pl.luckboy.purfuncor.frontend.resolver

sealed trait Value[+T, +U, +V, W]

case class NoValue[+T, +U, +V, W](msg: String, stackTrace: List[StackTraceElement]) extends Value[T, U, V, W]
{
  def systemStackTrace = Thread.currentThread().getStackTrace().toList
}

case class BooleanValue[+T, +U, +V, W](x: Boolean) extends Value[T, U, V, W]
case class ByteValue[+T, +U, +V, W](x: Byte) extends Value[T, U, V, W]
case class ShortValue[+T, +U, +V, W](x: Short) extends Value[T, U, V, W]
case class IntValue[+T, +U, +V, W](x: Int) extends Value[T, U, V, W]
case class LongValue[+T, +U, +V, W](x: Long) extends Value[T, U, V, W]
case class FloatValue[+T, +U, +V, W](x: Float) extends Value[T, U, V, W]
case class DoubleValue[+T, +U, +V, W](x: Double) extends Value[T, U, V, W]
case class TupleFunValue[+T, +U, +V, W](n: Int) extends Value[T, U, V, W]
case class TupleFieldFunValue[+T, +U, +V, W](i: Int) extends Value[T, U, V, W]
case class BuiltinFunValue[+T, +U, +V, W](f: frontend.BuiltinFunction.Value) extends Value[T, U, V, W]
case class TupleValue[+T, +U, +V, W](xs: Vector[Value[T, U, V, W]]) extends Value[T, U, V, W]
case class ArrayValue[+T, +U, +V, W](xs: Vector[Value[T, U, V, W]]) extends Value[T, U, V, W]
case class CombinatorValue[+T, +U, +V, W](comb: frontend.Combinator[T, U], loc: V) extends Value[T, U, V, W]
case class LambdaValue[+T, +U, +V, W](lambda: frontend.Lambda[T, U], closureVarValues: Map[W, Value[T, U, V, W]], pos: Position) extends Value[T, U, V, W]
case class PartialAppValue[+T, +U, +V, W](funValue: Value[T, U, V, W], argValues: Vector[Value[T, U, V, W]]) extends Value[T, U, V, W]