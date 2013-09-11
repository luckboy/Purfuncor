package pl.luckboy.purfuncor.typer
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend
import pl.luckboy.purfuncor.common.Evaluator._

object TypeBuiltinFunctions
{
  def illegalAppNoTypeValue[T, U, V, W] = NoTypeValue.fromError[T, U, V, W](FatalError("illegal type application", none, NoPosition))
  
  val typeBuiltinFunctions = Map[frontend.TypeBuiltinFunction.Value, TypeFunction](
      frontend.TypeBuiltinFunction.Any -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Any, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      frontend.TypeBuiltinFunction.Nothing -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Nothing, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      frontend.TypeBuiltinFunction.Zero -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Zero, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      frontend.TypeBuiltinFunction.NonZero -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.NonZero, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      frontend.TypeBuiltinFunction.Boolean -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Boolean, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      frontend.TypeBuiltinFunction.Char -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Char, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      frontend.TypeBuiltinFunction.Byte -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Byte, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      frontend.TypeBuiltinFunction.Short -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Short, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      frontend.TypeBuiltinFunction.Int -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Int, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      frontend.TypeBuiltinFunction.Long -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Long, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      frontend.TypeBuiltinFunction.Float -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Float, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      frontend.TypeBuiltinFunction.Double -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Double, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      frontend.TypeBuiltinFunction.Empty -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Empty, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      frontend.TypeBuiltinFunction.NonEmpty -> new TypeFunction(0) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq() => (env, EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.NonEmpty, Nil)))
            case _     => (env, illegalAppNoTypeValue)
          }
      },
      frontend.TypeBuiltinFunction.Array -> new TypeFunction(1) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq(value) =>
              val (env2, res) = value.typeValueTermS(env)
              (env2, res.map { tvt => EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Array, Seq(tvt))) }.valueOr(identity))
            case _     =>
              (env, illegalAppNoTypeValue)
          }
      },
      frontend.TypeBuiltinFunction.Fun -> new TypeFunction(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq(value1, value2) =>
              val (env2, res1) = value1.typeValueTermS(env)
              val (env3, res2) = value2.typeValueTermS(env2)
              val retValue = (for {
                t1 <- res1
                t2 <- res2
              } yield EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Fun, Seq(t1, t2)))).valueOr(identity)
              (env3, retValue)
            case _     =>
              (env, illegalAppNoTypeValue)
          }
      },
      frontend.TypeBuiltinFunction.Conj -> new TypeFunction(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq(value1, value2) =>
              val (env2, res1) = value1.typeValueTermS(env)
              val (env3, res2) = value2.typeValueTermS(env2)
              val retValue = (for {
                t1 <- res1
                t2 <- res2
              } yield EvaluatedTypeValue(t1 & t2)).valueOr(identity)
              (env3, retValue)
            case _     =>
              (env, illegalAppNoTypeValue)
          }
      },
      frontend.TypeBuiltinFunction.Disj -> new TypeFunction(2) {
        override def applyS[T, U, V, W, E](argValues: Seq[TypeValue[T, U, V, W]])(env: E)(implicit eval: Evaluator[TypeSimpleTerm[U, V], E, TypeValue[T, U, V, W]]) =
          argValues match {
            case Seq(value1, value2) =>
              val (env2, res1) = value1.typeValueTermS(env)
              val (env3, res2) = value2.typeValueTermS(env2)
              val retValue = (for {
                t1 <- res1
                t2 <- res2
              } yield EvaluatedTypeValue(t1 | t2)).valueOr(identity)
              (env3, retValue)
            case _     =>
              (env, illegalAppNoTypeValue)
          }
      })
}