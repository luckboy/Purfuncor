package pl.luckboy.purfuncor.typer
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.common.Evaluator._

object TypeBuiltinFunctions
{
  def illegalAppNoTypeValue[T, U, V, W] = NoTypeValue.fromError[T, U, V, W](FatalError("illegal type application", none, NoPosition))
  
  val typeBuiltinFunctions = Map[TypeBuiltinFunction.Value, TypeFunction](
      TypeBuiltinFunction.Any -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Any, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      TypeBuiltinFunction.Nothing -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Nothing, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      TypeBuiltinFunction.Zero -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Zero, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      TypeBuiltinFunction.NonZero -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.NonZero, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      TypeBuiltinFunction.Boolean -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Boolean, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      TypeBuiltinFunction.Char -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Char, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      TypeBuiltinFunction.Byte -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Byte, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      TypeBuiltinFunction.Short -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Short, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      TypeBuiltinFunction.Int -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Int, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      TypeBuiltinFunction.Long -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Long, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      TypeBuiltinFunction.Float -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Float, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      TypeBuiltinFunction.Double -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Double, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      TypeBuiltinFunction.Empty -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Empty, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      TypeBuiltinFunction.NonEmpty -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.NonEmpty, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      TypeBuiltinFunction.Array -> new TypeFunction(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq(value) =>
              val (env2, evaluatedValue) = eval.forceS(value)(env)
              evaluatedValue match {
                case EvaluatedTypeValue(term)         =>
                  (env2, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Array, Seq(term))))
                case noValue: NoTypeValue[T, U, V, W] =>
                  (env2, noValue)
                case _                                =>
                  (env2, NoTypeValue.fromError(FatalError("unevaluated type value", none, NoPosition)))
              }
            case _     =>
              (env, illegalAppNoTypeValue)
          }
      },
      TypeBuiltinFunction.Fun -> new TypeFunction(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq(value1, value2) =>
              val (env2, evaluatedValue1) = eval.forceS(value1)(env)
              val (env3, evaluatedValue2) = eval.forceS(value2)(env2)
              (evaluatedValue1, evaluatedValue2) match {
                case (EvaluatedTypeValue(term1), EvaluatedTypeValue(term2)) =>
                  (env3, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Fun, Seq(term1, term2))))
                case (noValue: NoTypeValue[T, U, V, W], _)                  =>
                  (env3, noValue)
                case (_, noValue: NoTypeValue[T, U, V, W])                  =>
                  (env3, noValue)
                case _                                                      =>
                  (env3, NoTypeValue.fromError(FatalError("unevaluated type values", none, NoPosition)))
              }
          }
      },
      TypeBuiltinFunction.Conj -> new TypeFunction(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]): (E, TypeValue[T, U, V, W]) =
          throw new UnsupportedOperationException
      },
      TypeBuiltinFunction.Disj -> new TypeFunction(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]): (E, TypeValue[T, U, V, W]) =
          throw new UnsupportedOperationException
      })
}