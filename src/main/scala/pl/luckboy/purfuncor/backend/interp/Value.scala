package pl.luckboy.purfuncor.backend.interp
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend
import pl.luckboy.purfuncor.frontend.Combinator
import pl.luckboy.purfuncor.frontend.SimpleTerm
import pl.luckboy.purfuncor.frontend.Lambda
import pl.luckboy.purfuncor.frontend.BuiltinFunction
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.common.Evaluator._

sealed trait Value[+T, +U, +V]
{
  def argCount: Int =
    this match {
      case CombinatorValue(comb, _)             => comb.args.size
      case LambdaValue(lambda, _, _)            => lambda.args.size
      case PartialAppValue(funValue, argValues) => funValue.argCount - argValues.size
      case TupleFunValue(n)                     => n
      case TupleFieldFunValue(_)                => 1
      case BuiltinFunValue(bf, f)               => f.argCount
      case _                                    => 1
    }
  
  def isNoValue = isInstanceOf[NoValue[T, U, V]]

  def apply[T2 >: T, U2 >: U, V2 >: V, E](argValues: Seq[Value[T2, U2, V2]])(implicit eval: Evaluator[SimpleTerm[T2, U2], E, Value[T2, U2, V2]]) =
    app(this, argValues)
    
  def withPos(pos: Position): Value[T, U, V] =
    this match {
      case noValue: NoValue[T, U, V] =>
        noValue.copy(currentStackTraceElem = noValue.currentStackTraceElem.orElse(some(StackTraceElement(none, none, pos))))
      case _                         =>
        this
    }
  
  def forFileAndCombSym(file: Option[java.io.File], combSym: Option[GlobalSymbol]): Value[T, U, V] =
    this match {
      case noValue: NoValue[T, U, V] =>
        noValue.copy(prevStackTraceElems = noValue.prevStackTraceElems ++ noValue.currentStackTraceElem.map { _.copy(file = file, combSym = combSym) }, currentStackTraceElem = none)
      case _                         =>
        this
    }
  
  override def toString =
    this match {
      case noValue @ NoValue(msg, _, _)         =>
        "<no value: " + msg + ">\n" +
        "system stack trace:\n" +
        noValue.systemStackTrace.map { (" " * 8) + "at " + _ }.mkString("\n") + "\n" +
        "stack trace:\n" +
        noValue.stackTrace.map { (" " * 8) + _ }.mkString("\n") + "\n"
      case BooleanValue(x)                      => if(x) "true" else "false"
      case CharValue(x)                         => "'" + (if(x === '\'') "\\'" else x) + "'"
      case ByteValue(x)                         => x + "b"
      case ShortValue(x)                        => x + "s"
      case IntValue(x)                          => x.toString
      case LongValue(x)                         => x + "L"
      case FloatValue(x)                        => x + "f"
      case DoubleValue(x)                       => x.toString
      case TupleFunValue(n)                     => "tuple " + n
      case TupleFieldFunValue(i)                => "#" + (i + 1)
      case BuiltinFunValue(f, _)                => "#" + f
      case TupleValue(values)                   => "tuple " + values.size + " " + values.mkString(" ")
      case ArrayValue(values)                   => "#[" + values.mkString(", ") + "]"
      case CombinatorValue(_, sym)              => sym.toString
      case LambdaValue(_, _, _)                 => "<lambda value>"
      case PartialAppValue(funValue, argValues) =>
        (List(funValue) ++ argValues).map {
          value =>
            value match {
              case _: PartialAppValue[T, U, V] => "(" + value + ")"
              case _                           => value.toString
            }
        }.mkString(" ")
    }
}

object Value
{
  def fromLiteralValue[T, U, V](value: frontend.LiteralValue): Value[T, U, V] =
    value match {
      case frontend.BooleanValue(x)       => BooleanValue(x)
      case frontend.CharValue(x)          => CharValue(x)
      case frontend.ByteValue(x)          => ByteValue(x)
      case frontend.ShortValue(x)         => ShortValue(x)
      case frontend.IntValue(x)           => IntValue(x)
      case frontend.LongValue(x)          => LongValue(x)
      case frontend.FloatValue(x)         => FloatValue(x)
      case frontend.DoubleValue(x)        => DoubleValue(x)
      case frontend.TupleFunValue(n)      => TupleFunValue(n)
      case frontend.TupleFieldFunValue(i) => TupleFieldFunValue(i)
      case frontend.BuiltinFunValue(bf)   => BuiltinFunValue.fromBuiltinFunction(bf)
    }
}

case class NoValue[+T, +U, +V](msg: String, prevStackTraceElems: List[StackTraceElement], currentStackTraceElem: Option[StackTraceElement]) extends Value[T, U, V]
{
  val stackTrace = prevStackTraceElems ++ currentStackTraceElem
  
  val systemStackTrace = Thread.currentThread().getStackTrace().toList  
}

object NoValue
{
  def fromString[T, U, V](s: String) = NoValue[T, U, V](s, Nil, none)
}

case class BooleanValue[+T, +U, +V](x: Boolean) extends Value[T, U, V]
case class CharValue[+T, +U, +V](x: Char) extends Value[T, U, V]
case class ByteValue[+T, +U, +V](x: Byte) extends Value[T, U, V]
case class ShortValue[+T, +U, +V](x: Short) extends Value[T, U, V]
case class IntValue[+T, +U, +V](x: Int) extends Value[T, U, V]
case class LongValue[+T, +U, +V](x: Long) extends Value[T, U, V]
case class FloatValue[+T, +U, +V](x: Float) extends Value[T, U, V]
case class DoubleValue[+T, +U, +V](x: Double) extends Value[T, U, V]

case class TupleFunValue[+T, +U, +V](n: Int) extends Value[T, U, V]
{
  def fullyApplyS[T2 >: T, U2 >: U, V2 >: V, E](argValues: Seq[Value[T2, U2, V2]])(env: E)(implicit eval: Evaluator[SimpleTerm[T2, U2], E, Value[T2, U2, V2]]) =
    if(argValues.size === n)
      (env, TupleValue(argValues.toVector))
    else
      (env, NoValue.fromString("illegal application"))
}

case class TupleFieldFunValue[+T, +U, +V](i: Int) extends Value[T, U, V]
{
  def fullyApplyS[T2 >: T, U2 >: U, V2 >: V, E](argValues: Seq[Value[T2, U2, V2]])(env: E)(implicit eval: Evaluator[SimpleTerm[T2, U2], E, Value[T2, U2, V2]]) =
    argValues match {
      case Seq(TupleValue(values)) => (env, values.lift(i).getOrElse(NoValue.fromString("no tuple field")))
      case _                       => (env, NoValue.fromString("illegal application"))
    }
}

case class BuiltinFunValue[+T, +U, +V](val bf: BuiltinFunction.Value, f: Function) extends Value[T, U, V]

object BuiltinFunValue
{
  def fromBuiltinFunction(bf: BuiltinFunction.Value) =
    BuiltinFunctions.builtinFunctions.get(bf).map { BuiltinFunValue(bf, _) }.getOrElse(NoValue.fromString("unsupported built-in function"))
}

case class TupleValue[+T, +U, +V](values: Vector[Value[T, U, V]]) extends Value[T, U, V]
case class ArrayValue[+T, +U, +V](values: Vector[Value[T, U, V]]) extends Value[T, U, V]
case class CombinatorValue[+T, +U, +V](comb: Combinator[T, U], sym: GlobalSymbol) extends Value[T, U, V]
case class LambdaValue[+T, +U, +V](lambda: Lambda[T, U], closure: V, file: Option[java.io.File]) extends Value[T, U, V]
case class PartialAppValue[+T, +U, +V](funValue: Value[T, U, V], argValues: Seq[Value[T, U, V]]) extends Value[T, U, V]