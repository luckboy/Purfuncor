package pl.luckboy.purfuncor.frontend.typer
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.kinder.InferredKind
import TypeValueTerms._

class BuiltinFunTypes[T]
{
  val builtinFunTypes = BuiltinFunTypes.builtinFunTypes[T]
}

object BuiltinFunTypes
{
  def builtinFunTypes[T] = Map[BuiltinFunction.Value, Type[T]](
      // Boolean
      BuiltinFunction.ZNot -> (
          // #Boolean #-> #Boolean
          InferredType[T](funType(booleanType, booleanType), Nil)),
      BuiltinFunction.ZAnd -> (
          // #Boolean #-> #Boolean #-> #Boolean
          InferredType[T](funType(booleanType, funType(booleanType, booleanType)), Nil)),
      BuiltinFunction.ZOr -> (
          // #Boolean #-> #Boolean #-> #Boolean
          InferredType[T](funType(booleanType, funType(booleanType, booleanType)), Nil)),
      BuiltinFunction.ZXor -> (
          // #Boolean #-> #Boolean #-> #Boolean
          InferredType[T](funType(booleanType, funType(booleanType, booleanType)), Nil)),
      BuiltinFunction.ZEq -> (
          // #Boolean #-> #Boolean #-> #Boolean
          InferredType[T](funType(booleanType, funType(booleanType, booleanType)), Nil)),
      BuiltinFunction.ZNe -> (
          // #Boolean #-> #Boolean #-> #Boolean
          InferredType[T](funType(booleanType, funType(booleanType, booleanType)), Nil)),
      BuiltinFunction.Cond -> (
          // \t1 => (() #-> t1) #-> (() #-> t1) #-> #Boolean #-> t1
          InferredType[T](funType(funType(TupleType(Nil), typeParam(0, 0)), funType(funType(TupleType(Nil), typeParam(0, 0)), funType(booleanType, typeParam(0, 0)))),
              Seq(InferredKind(Star(KindType, NoPosition))))),
      // Char
      BuiltinFunction.CEq -> (
          // #Char #-> #Char #-> #Boolean
          InferredType[T](funType(charType, funType(charType, booleanType)), Nil)),
      BuiltinFunction.CNe -> (
          // #Char #-> #Char #-> #Boolean
          InferredType[T](funType(charType, funType(charType, booleanType)), Nil)),
      BuiltinFunction.CharFromInt -> (
          // ((#Zero #| #NonZero) #& #Int) #-> #Char
          InferredType[T](funType((zeroType | nonZeroType) & intType, charType), Nil)),
      // Byte
      BuiltinFunction.BNeg -> (
          // ((#Zero #| #NonZero) #& #Byte) #-> ((#Zero #| #NonZero) #& #Byte)
          InferredType[T](funType((zeroType | nonZeroType) & byteType, (zeroType | nonZeroType) & byteType), Nil)),
      BuiltinFunction.BNot -> (
          // ((#Zero #| #NonZero) #& #Byte) #-> ((#Zero #| #NonZero) #& #Byte)
          InferredType[T](funType((zeroType | nonZeroType) & byteType, (zeroType | nonZeroType) & byteType), Nil)),
      BuiltinFunction.BAdd -> (
          // ((#Zero #| #NonZero) #& #Byte) #-> #((#Zero #| #NonZero) #& #Byte) #-> ((#Zero #| #NonZero) #& #Byte)
          InferredType[T](funType((zeroType | nonZeroType) & byteType, funType((zeroType | nonZeroType) & byteType, (zeroType | nonZeroType) & byteType)), Nil)),
      BuiltinFunction.BSub -> (
          // ((#Zero #| #NonZero) #& #Byte) #-> #((#Zero #| #NonZero) #& #Byte) #-> ((#Zero #| #NonZero) #& #Byte)
          InferredType[T](funType((zeroType | nonZeroType) & byteType, funType((zeroType | nonZeroType) & byteType, (zeroType | nonZeroType) & byteType)), Nil)),
      BuiltinFunction.BMul -> (
          // ((#Zero #| #NonZero) #& #Byte) #-> #((#Zero #| #NonZero) #& #Byte) #-> ((#Zero #| #NonZero) #& #Byte)
          InferredType[T](funType((zeroType | nonZeroType) & byteType, funType((zeroType | nonZeroType) & byteType, (zeroType | nonZeroType) & byteType)), Nil)),
      BuiltinFunction.BDiv -> (
          // ((#Zero #| #NonZero) #& #Byte) #-> (#NonZero #& #Byte) #-> ((#Zero #| #NonZero) #& #Byte)
          InferredType[T](funType((zeroType | nonZeroType) & byteType, funType(nonZeroType & byteType, (zeroType | nonZeroType) & byteType)), Nil)),
      BuiltinFunction.BMod -> (
          // ((#Zero #| #NonZero) #& #Byte) #-> (#NonZero #& #Byte) #-> ((#Zero #| #NonZero) #& #Byte)
          InferredType[T](funType((zeroType | nonZeroType) & byteType, funType(nonZeroType & byteType, (zeroType | nonZeroType) & byteType)), Nil)),
      BuiltinFunction.BLsh -> (
          // ((#Zero #| #NonZero) #& #Byte) #-> #((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Byte)
          InferredType[T](funType((zeroType | nonZeroType) & byteType, funType((zeroType | nonZeroType) & intType, (zeroType | nonZeroType) & byteType)), Nil)),
      BuiltinFunction.BRsh -> (
          // ((#Zero #| #NonZero) #& #Byte) #-> #((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Byte)
          InferredType[T](funType((zeroType | nonZeroType) & byteType, funType((zeroType | nonZeroType) & intType, (zeroType | nonZeroType) & byteType)), Nil)),
      BuiltinFunction.BAnd -> (
          // ((#Zero #| #NonZero) #& #Byte) #-> #((#Zero #| #NonZero) #& #Byte) #-> ((#Zero #| #NonZero) #& #Byte)
          InferredType[T](funType((zeroType | nonZeroType) & byteType, funType((zeroType | nonZeroType) & byteType, (zeroType | nonZeroType) & byteType)), Nil)),
      BuiltinFunction.BOr -> (
          // ((#Zero #| #NonZero) #& #Byte) #-> #((#Zero #| #NonZero) #& #Byte) #-> ((#Zero #| #NonZero) #& #Byte)
          InferredType[T](funType((zeroType | nonZeroType) & byteType, funType((zeroType | nonZeroType) & byteType, (zeroType | nonZeroType) & byteType)), Nil)),
      BuiltinFunction.BXor -> (
          // ((#Zero #| #NonZero) #& #Byte) #-> #((#Zero #| #NonZero) #& #Byte) #-> ((#Zero #| #NonZero) #& #Byte)
          InferredType[T](funType((zeroType | nonZeroType) & byteType, funType((zeroType | nonZeroType) & byteType, (zeroType | nonZeroType) & byteType)), Nil)),
      BuiltinFunction.BEq -> (
          // ((#Zero #| #NonZero) #& #Byte) #-> #((#Zero #| #NonZero) #& #Byte) #-> #Boolean
          InferredType[T](funType((zeroType | nonZeroType) & byteType, funType((zeroType | nonZeroType) & byteType, booleanType)), Nil)),
      BuiltinFunction.BNe -> (
          // ((#Zero #| #NonZero) #& #Byte) #-> #((#Zero #| #NonZero) #& #Byte) #-> #Boolean
          InferredType[T](funType((zeroType | nonZeroType) & byteType, funType((zeroType | nonZeroType) & byteType, booleanType)), Nil)),
      BuiltinFunction.BLt -> (
          // ((#Zero #| #NonZero) #& #Byte) #-> #((#Zero #| #NonZero) #& #Byte) #-> #Boolean
          InferredType[T](funType((zeroType | nonZeroType) & byteType, funType((zeroType | nonZeroType) & byteType, booleanType)), Nil)),
      BuiltinFunction.BLe -> (
          // ((#Zero #| #NonZero) #& #Byte) #-> #((#Zero #| #NonZero) #& #Byte) #-> #Boolean
          InferredType[T](funType((zeroType | nonZeroType) & byteType, funType((zeroType | nonZeroType) & byteType, booleanType)), Nil)),
      BuiltinFunction.BGt -> (
          // ((#Zero #| #NonZero) #& #Byte) #-> #((#Zero #| #NonZero) #& #Byte) #-> #Boolean
          InferredType[T](funType((zeroType | nonZeroType) & byteType, funType((zeroType | nonZeroType) & byteType, booleanType)), Nil)),
      BuiltinFunction.BGe -> (
          // ((#Zero #| #NonZero) #& #Byte) #-> #((#Zero #| #NonZero) #& #Byte) #-> #Boolean
          InferredType[T](funType((zeroType | nonZeroType) & byteType, funType((zeroType | nonZeroType) & byteType, booleanType)), Nil)),
      BuiltinFunction.ByteFromShort -> (
          // ((zeroType | nonZeroType) & #Short) #-> ((#Zero #| #NonZero) #& #Byte)
          InferredType[T](funType((zeroType | nonZeroType) & shortType, (zeroType | nonZeroType) & byteType), Nil)),
      BuiltinFunction.ByteFromInt -> (
          // ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Byte)
          InferredType[T](funType((zeroType | nonZeroType) & intType, (zeroType | nonZeroType) & byteType), Nil)),
      BuiltinFunction.ByteFromLong -> (
          // #Long #-> ((#Zero #| #NonZero) #& #Byte)
          InferredType[T](funType((zeroType | nonZeroType) & longType, (zeroType | nonZeroType) & byteType), Nil)),
      BuiltinFunction.ByteFromFloat -> (
          // #Float #-> ((#Zero #| #NonZero) #& #Byte)
          InferredType[T](funType(floatType, (zeroType | nonZeroType) & byteType), Nil)),
      BuiltinFunction.ByteFromDouble -> (
          // #Double #-> #Byte
          InferredType[T](funType(doubleType, byteType), Nil)),
      // Short
      BuiltinFunction.SNeg -> (
          // ((#Zero #| #NonZero) #& #Short) #-> ((#Zero #| #NonZero) #& #Short)
          InferredType[T](funType((zeroType | nonZeroType) & shortType, (zeroType | nonZeroType) & shortType), Nil)),
      BuiltinFunction.SNot -> (
          // ((#Zero #| #NonZero) #& #Short) #-> ((#Zero #| #NonZero) #& #Short)
          InferredType[T](funType((zeroType | nonZeroType) & shortType, (zeroType | nonZeroType) & shortType), Nil)),
      BuiltinFunction.SAdd -> (
          // ((#Zero #| #NonZero) #& #Short) #-> ((#Zero #| #NonZero) #& #Short) #-> ((#Zero #| #NonZero) #& #Short)
          InferredType[T](funType((zeroType | nonZeroType) & shortType, funType((zeroType | nonZeroType) & shortType, (zeroType | nonZeroType) & shortType)), Nil)),
      BuiltinFunction.SSub -> (
          // ((#Zero #| #NonZero) #& #Short) #-> ((#Zero #| #NonZero) #& #Short) #-> ((#Zero #| #NonZero) #& #Short)
          InferredType[T](funType((zeroType | nonZeroType) & shortType, funType((zeroType | nonZeroType) & shortType, (zeroType | nonZeroType) & shortType)), Nil)),
      BuiltinFunction.SMul -> (
          // ((#Zero #| #NonZero) #& #Short) #-> ((#Zero #| #NonZero) #& #Short) #-> ((#Zero #| #NonZero) #& #Short)
          InferredType[T](funType((zeroType | nonZeroType) & shortType, funType((zeroType | nonZeroType) & shortType, (zeroType | nonZeroType) & shortType)), Nil)),
      BuiltinFunction.SDiv -> (
          // ((#Zero #| #NonZero) #& #Short) #-> (#NonZero #& #Short) #-> ((#Zero #| #NonZero) #& #Short)
          InferredType[T](funType((zeroType | nonZeroType) & shortType, funType(nonZeroType & shortType, (zeroType | nonZeroType) & shortType)), Nil)),
      BuiltinFunction.SMod -> (
          // ((#Zero #| #NonZero) #& #Short) #-> (#NonZero #& #Short) #-> ((#Zero #| #NonZero) #& #Short)
          InferredType[T](funType((zeroType | nonZeroType) & shortType, funType(nonZeroType & shortType, (zeroType | nonZeroType) & shortType)), Nil)),
      BuiltinFunction.SLsh -> (
          // ((#Zero #| #NonZero) #& #Short) #-> ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Short)
          InferredType[T](funType((zeroType | nonZeroType) & shortType, funType((zeroType | nonZeroType) & intType, (zeroType | nonZeroType) & shortType)), Nil)),
      BuiltinFunction.SRsh -> (
          // ((#Zero #| #NonZero) #& #Short) #-> ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Short)
          InferredType[T](funType((zeroType | nonZeroType) & shortType, funType((zeroType | nonZeroType) & intType, (zeroType | nonZeroType) & shortType)), Nil)),
      BuiltinFunction.SAnd -> (
          // ((#Zero #| #NonZero) #& #Short) #-> ((#Zero #| #NonZero) #& #Short) #-> ((#Zero #| #NonZero) #& #Short)
          InferredType[T](funType((zeroType | nonZeroType) & shortType, funType((zeroType | nonZeroType) & shortType, (zeroType | nonZeroType) & shortType)), Nil)),
      BuiltinFunction.SOr -> (
          // ((#Zero #| #NonZero) #& #Short) #-> ((#Zero #| #NonZero) #& #Short) #-> ((#Zero #| #NonZero) #& #Short)
          InferredType[T](funType((zeroType | nonZeroType) & shortType, funType((zeroType | nonZeroType) & shortType, (zeroType | nonZeroType) & shortType)), Nil)),
      BuiltinFunction.SXor -> (
          // ((#Zero #| #NonZero) #& #Short) #-> ((#Zero #| #NonZero) #& #Short) #-> ((#Zero #| #NonZero) #& #Short)
          InferredType[T](funType((zeroType | nonZeroType) & shortType, funType((zeroType | nonZeroType) & shortType, (zeroType | nonZeroType) & shortType)), Nil)),
      BuiltinFunction.SEq -> (
          // ((#Zero #| #NonZero) #& #Short) #-> ((#Zero #| #NonZero) #& #Short) #-> #Boolean
          InferredType[T](funType((zeroType | nonZeroType) & shortType, funType((zeroType | nonZeroType) & shortType, booleanType)), Nil)),
      BuiltinFunction.SNe -> (
          // ((#Zero #| #NonZero) #& #Short) #-> ((#Zero #| #NonZero) #& #Short) #-> #Boolean
          InferredType[T](funType((zeroType | nonZeroType) & shortType, funType((zeroType | nonZeroType) & shortType, booleanType)), Nil)),
      BuiltinFunction.SLt -> (
          // ((#Zero #| #NonZero) #& #Short) #-> ((#Zero #| #NonZero) #& #Short) #-> #Boolean
          InferredType[T](funType((zeroType | nonZeroType) & shortType, funType((zeroType | nonZeroType) & shortType, booleanType)), Nil)),
      BuiltinFunction.SLe -> (
          // ((#Zero #| #NonZero) #& #Short) #-> ((#Zero #| #NonZero) #& #Short) #-> #Boolean
          InferredType[T](funType((zeroType | nonZeroType) & shortType, funType((zeroType | nonZeroType) & shortType, booleanType)), Nil)),
      BuiltinFunction.SGt -> (
          // ((#Zero #| #NonZero) #& #Short) #-> ((#Zero #| #NonZero) #& #Short) #-> #Boolean
          InferredType[T](funType((zeroType | nonZeroType) & shortType, funType((zeroType | nonZeroType) & shortType, booleanType)), Nil)),
      BuiltinFunction.SGe -> (
          // ((#Zero #| #NonZero) #& #Short) #-> ((#Zero #| #NonZero) #& #Short) #-> #Boolean
          InferredType[T](funType((zeroType | nonZeroType) & shortType, funType((zeroType | nonZeroType) & shortType, booleanType)), Nil)),
      BuiltinFunction.ShortFromByte -> (
          // ((#Zero #| #NonZero) #& #Byte) #-> ((#Zero #| #NonZero) #& #Short)
          InferredType[T](funType((zeroType | nonZeroType) & byteType, (zeroType | nonZeroType) & shortType), Nil)),
      BuiltinFunction.ShortFromInt -> (
          // ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Short)
          InferredType[T](funType((zeroType | nonZeroType) & intType, (zeroType | nonZeroType) & shortType), Nil)),
      BuiltinFunction.ShortFromLong -> (
          // ((#Zero #| #NonZero) #& #Long) #-> ((#Zero #| #NonZero) #& #Short)
          InferredType[T](funType((zeroType | nonZeroType) & longType, (zeroType | nonZeroType) & shortType), Nil)),
      BuiltinFunction.ShortFromFloat -> (
          // #Float #-> ((#Zero #| #NonZero) #& #Short)
          InferredType[T](funType(floatType, (zeroType | nonZeroType) & shortType), Nil)),
      BuiltinFunction.ShortFromDouble -> (
          // #Double #-> ((#Zero #| #NonZero) #& #Short)
          InferredType[T](funType(doubleType, (zeroType | nonZeroType) & shortType), Nil)),
      // Int
      BuiltinFunction.INeg -> (
          // ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int)
          InferredType[T](funType((zeroType | nonZeroType) & intType,  (zeroType | nonZeroType) & intType), Nil)),
      BuiltinFunction.INot -> (
          // ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int)
          InferredType[T](funType((zeroType | nonZeroType) & intType,  (zeroType | nonZeroType) & intType), Nil)),
      BuiltinFunction.IAdd -> (
          // ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int)
          InferredType[T](funType((zeroType | nonZeroType) & intType, funType((zeroType | nonZeroType) & intType, (zeroType | nonZeroType) & intType)), Nil)),
      BuiltinFunction.ISub -> (
          // ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int)
          InferredType[T](funType((zeroType | nonZeroType) & intType, funType((zeroType | nonZeroType) & intType, (zeroType | nonZeroType) & intType)), Nil)),
      BuiltinFunction.IMul -> (
          // ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int)
          InferredType[T](funType((zeroType | nonZeroType) & intType, funType((zeroType | nonZeroType) & intType, (zeroType | nonZeroType) & intType)), Nil)),
      BuiltinFunction.IDiv -> (
          // ((#Zero #| #NonZero) #& #Int) #-> (#NonZero #& #Int) #-> ((#Zero #| #NonZero) #& #Int)
          InferredType[T](funType((zeroType | nonZeroType) & intType, funType(nonZeroType & intType, (zeroType | nonZeroType) & intType)), Nil)),
      BuiltinFunction.IMod -> (
          // ((#Zero #| #NonZero) #& #Int) #-> (#NonZero #& #Int) #-> ((#Zero #| #NonZero) #& #Int)
          InferredType[T](funType((zeroType | nonZeroType) & intType, funType(nonZeroType & intType, (zeroType | nonZeroType) & intType)), Nil)),
      BuiltinFunction.ILsh -> (
          // ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int)
          InferredType[T](funType((zeroType | nonZeroType) & intType, funType((zeroType | nonZeroType) & intType, (zeroType | nonZeroType) & intType)), Nil)),
      BuiltinFunction.IRsh -> (
          // ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int)
          InferredType[T](funType((zeroType | nonZeroType) & intType, funType((zeroType | nonZeroType) & intType, (zeroType | nonZeroType) & intType)), Nil)),
      BuiltinFunction.IAnd -> (
          // ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int)
          InferredType[T](funType((zeroType | nonZeroType) & intType, funType((zeroType | nonZeroType) & intType, (zeroType | nonZeroType) & intType)), Nil)),
      BuiltinFunction.IOr -> (
          // ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int)
          InferredType[T](funType((zeroType | nonZeroType) & intType, funType((zeroType | nonZeroType) & intType, (zeroType | nonZeroType) & intType)), Nil)),
      BuiltinFunction.IXor -> (
          // ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int)
          InferredType[T](funType((zeroType | nonZeroType) & intType, funType((zeroType | nonZeroType) & intType, (zeroType | nonZeroType) & intType)), Nil)),
      BuiltinFunction.IEq -> (
          // ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int) #-> #Boolean
          InferredType[T](funType((zeroType | nonZeroType) & intType, funType((zeroType | nonZeroType) & intType, booleanType)), Nil)),
      BuiltinFunction.INe -> (
          // ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int) #-> #Boolean
          InferredType[T](funType((zeroType | nonZeroType) & intType, funType((zeroType | nonZeroType) & intType, booleanType)), Nil)),
      BuiltinFunction.ILt -> (
          // ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int) #-> #Boolean
          InferredType[T](funType((zeroType | nonZeroType) & intType, funType((zeroType | nonZeroType) & intType, booleanType)), Nil)),
      BuiltinFunction.ILe -> (
          // ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int) #-> #Boolean
          InferredType[T](funType((zeroType | nonZeroType) & intType, funType((zeroType | nonZeroType) & intType, booleanType)), Nil)),
      BuiltinFunction.IGt -> (
          // ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int) #-> #Boolean
          InferredType[T](funType((zeroType | nonZeroType) & intType, funType((zeroType | nonZeroType) & intType, booleanType)), Nil)),
      BuiltinFunction.IGe -> (
          // ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int) #-> #Boolean
          InferredType[T](funType((zeroType | nonZeroType) & intType, funType((zeroType | nonZeroType) & intType, booleanType)), Nil)),
      BuiltinFunction.IntFromChar -> (
          // #Char #-> ((#Zero #| #NonZero) #& #Int)
          InferredType[T](funType(charType, (zeroType | nonZeroType) & intType), Nil)),
      BuiltinFunction.IntFromByte -> (
          // ((#Zero #| #NonZero) #& #Byte) #-> ((#Zero #| #NonZero) #& #Int)
          InferredType[T](funType((zeroType | nonZeroType) & byteType, (zeroType | nonZeroType) & intType), Nil)),
      BuiltinFunction.IntFromShort -> (
          // ((#Zero #| #NonZero) #& #Short) #-> ((#Zero #| #NonZero) #& #Int)
          InferredType[T](funType((zeroType | nonZeroType) & shortType, (zeroType | nonZeroType) & intType), Nil)),
      BuiltinFunction.IntFromLong -> (
          // ((#Zero #| #NonZero) #& #Long) #-> ((#Zero #| #NonZero) #& #Int)
          InferredType[T](funType((zeroType | nonZeroType) & longType, (zeroType | nonZeroType) & intType), Nil)),
      BuiltinFunction.IntFromFloat -> (
          // #Float #-> ((#Zero #| #NonZero) #& #Int)
          InferredType[T](funType(floatType, (zeroType | nonZeroType) & intType), Nil)),
      BuiltinFunction.IntFromDouble -> (
          // #Double #-> ((#Zero #| #NonZero) #& #Int)
          InferredType[T](funType(doubleType, (zeroType | nonZeroType) & intType), Nil)),
      // Long
      BuiltinFunction.LNeg -> (
          // ((#Zero #| #NonZero) #& #Long) #-> ((#Zero #| #NonZero) #& #Long)
          InferredType[T](funType((zeroType | nonZeroType) & longType, (zeroType | nonZeroType) & longType), Nil)),
      BuiltinFunction.LNot -> (
          // ((#Zero #| #NonZero) #& #Long) #-> ((#Zero #| #NonZero) #& #Long)
          InferredType[T](funType((zeroType | nonZeroType) & longType, (zeroType | nonZeroType) & longType), Nil)),
      BuiltinFunction.LAdd -> (
          // ((#Zero #| #NonZero) #& #Long) #-> ((#Zero #| #NonZero) #& #Long) #-> ((#Zero #| #NonZero) #& #Long)
          InferredType[T](funType((zeroType | nonZeroType) & longType, funType((zeroType | nonZeroType) & longType, (zeroType | nonZeroType) & longType)), Nil)),
      BuiltinFunction.LSub -> (
          // ((#Zero #| #NonZero) #& #Long) #-> ((#Zero #| #NonZero) #& #Long) #-> ((#Zero #| #NonZero) #& #Long)
          InferredType[T](funType((zeroType | nonZeroType) & longType, funType((zeroType | nonZeroType) & longType, (zeroType | nonZeroType) & longType)), Nil)),
      BuiltinFunction.LMul -> (
          // ((#Zero #| #NonZero) #& #Long) #-> ((#Zero #| #NonZero) #& #Long) #-> ((#Zero #| #NonZero) #& #Long)
          InferredType[T](funType((zeroType | nonZeroType) & longType, funType((zeroType | nonZeroType) & longType, (zeroType | nonZeroType) & longType)), Nil)),
      BuiltinFunction.LDiv -> (
          // ((#Zero #| #NonZero) #& #Long) #-> (#NonZero #& #Long) #-> ((#Zero #| #NonZero) #& #Long)
          InferredType[T](funType((zeroType | nonZeroType) & longType, funType(nonZeroType & longType, (zeroType | nonZeroType) & longType)), Nil)),
      BuiltinFunction.LMod -> (
          // ((#Zero #| #NonZero) #& #Long) #-> (#NonZero #& #Long) #-> ((#Zero #| #NonZero) #& #Long)
          InferredType[T](funType((zeroType | nonZeroType) & longType, funType(nonZeroType & longType, (zeroType | nonZeroType) & longType)), Nil)),
      BuiltinFunction.LLsh -> (
          // ((#Zero #| #NonZero) #& #Long) #-> ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Long)
          InferredType[T](funType((zeroType | nonZeroType) & longType, funType((zeroType | nonZeroType) & intType, (zeroType | nonZeroType) & longType)), Nil)),
      BuiltinFunction.LRsh -> (
          // ((#Zero #| #NonZero) #& #Long) #-> ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Long)
          InferredType[T](funType((zeroType | nonZeroType) & longType, funType((zeroType | nonZeroType) & intType, (zeroType | nonZeroType) & longType)), Nil)),
      BuiltinFunction.LAnd -> (
          // ((#Zero #| #NonZero) #& #Long) #-> ((#Zero #| #NonZero) #& #Long) #-> ((#Zero #| #NonZero) #& #Long)
          InferredType[T](funType((zeroType | nonZeroType) & longType, funType((zeroType | nonZeroType) & longType, (zeroType | nonZeroType) & longType)), Nil)),
      BuiltinFunction.LOr -> (
          // ((#Zero #| #NonZero) #& #Long) #-> ((#Zero #| #NonZero) #& #Long) #-> ((#Zero #| #NonZero) #& #Long)
          InferredType[T](funType((zeroType | nonZeroType) & longType, funType((zeroType | nonZeroType) & longType, (zeroType | nonZeroType) & longType)), Nil)),
      BuiltinFunction.LXor -> (
          // ((#Zero #| #NonZero) #& #Long) #-> ((#Zero #| #NonZero) #& #Long) #-> ((#Zero #| #NonZero) #& #Long)
          InferredType[T](funType((zeroType | nonZeroType) & longType, funType((zeroType | nonZeroType) & longType, (zeroType | nonZeroType) & longType)), Nil)),
      BuiltinFunction.LEq -> (
          // ((#Zero #| #NonZero) #& #Long) #-> ((#Zero #| #NonZero) #& #Long) #-> #Boolean
          InferredType[T](funType((zeroType | nonZeroType) & longType, funType((zeroType | nonZeroType) & longType, booleanType)), Nil)),
      BuiltinFunction.LNe -> (
          // ((#Zero #| #NonZero) #& #Long) #-> ((#Zero #| #NonZero) #& #Long) #-> #Boolean
          InferredType[T](funType((zeroType | nonZeroType) & longType, funType((zeroType | nonZeroType) & longType, booleanType)), Nil)),
      BuiltinFunction.LLt -> (
          // ((#Zero #| #NonZero) #& #Long) #-> ((#Zero #| #NonZero) #& #Long) #-> #Boolean
          InferredType[T](funType((zeroType | nonZeroType) & longType, funType((zeroType | nonZeroType) & longType, booleanType)), Nil)),
      BuiltinFunction.LLe -> (
          // ((#Zero #| #NonZero) #& #Long) #-> ((#Zero #| #NonZero) #& #Long) #-> #Boolean
          InferredType[T](funType((zeroType | nonZeroType) & longType, funType((zeroType | nonZeroType) & longType, booleanType)), Nil)),
      BuiltinFunction.LGt -> (
          // ((#Zero #| #NonZero) #& #Long) #-> ((#Zero #| #NonZero) #& #Long) #-> #Boolean
          InferredType[T](funType((zeroType | nonZeroType) & longType, funType((zeroType | nonZeroType) & longType, booleanType)), Nil)),
      BuiltinFunction.LGe -> (
          // ((#Zero #| #NonZero) #& #Long) #-> ((#Zero #| #NonZero) #& #Long) #-> #Boolean
          InferredType[T](funType((zeroType | nonZeroType) & longType, funType((zeroType | nonZeroType) & longType, booleanType)), Nil)),
      BuiltinFunction.LongFromByte -> (
          // ((#Zero #| #NonZero) #& #Byte) #-> ((#Zero #| #NonZero) #& #Long)
          InferredType[T](funType((zeroType | nonZeroType) & byteType, (zeroType | nonZeroType) & longType), Nil)),
      BuiltinFunction.LongFromShort -> (
          // ((#Zero #| #NonZero) #& #Short) #-> ((#Zero #| #NonZero) #& #Long)
          InferredType[T](funType((zeroType | nonZeroType) & shortType, (zeroType | nonZeroType) & longType), Nil)),
      BuiltinFunction.LongFromInt -> (
          // ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Long)
          InferredType[T](funType((zeroType | nonZeroType) & intType, (zeroType | nonZeroType) & longType), Nil)),
      BuiltinFunction.LongFromFloat -> (
          // #Float #-> ((#Zero #| #NonZero) #& #Long)
          InferredType[T](funType(floatType, (zeroType | nonZeroType) & longType), Nil)),
      BuiltinFunction.LongFromDouble -> (
          // #Double #-> ((#Zero #| #NonZero) #& #Long)
          InferredType[T](funType(doubleType, (zeroType | nonZeroType) & longType), Nil)),
      // Float
      BuiltinFunction.FNeg -> (
          // #Float #-> #Float
          InferredType[T](funType(floatType, floatType), Nil)),
      BuiltinFunction.FAdd -> (
          // #Float #-> #Float #-> #Float
          InferredType[T](funType(floatType, funType(floatType, floatType)), Nil)),
      BuiltinFunction.FSub -> (
          // #Float #-> #Float #-> #Float
          InferredType[T](funType(floatType, funType(floatType, floatType)), Nil)),
      BuiltinFunction.FMul -> (
          // #Float #-> #Float #-> #Float
          InferredType[T](funType(floatType, funType(floatType, floatType)), Nil)),
      BuiltinFunction.FDiv -> (
          // #Float #-> #Float #-> #Float
          InferredType[T](funType(floatType, funType(floatType, floatType)), Nil)),
      BuiltinFunction.FEq -> (
          // #Float #-> #Float #-> #Boolean
          InferredType[T](funType(floatType, funType(floatType, booleanType)), Nil)),
      BuiltinFunction.FNe -> (
          // #Float #-> #Float #-> #Boolean
          InferredType[T](funType(floatType, funType(floatType, booleanType)), Nil)),
      BuiltinFunction.FLt -> (
          // #Float #-> #Float #-> #Boolean
          InferredType[T](funType(floatType, funType(floatType, booleanType)), Nil)),
      BuiltinFunction.FLe -> (
          // #Float #-> #Float #-> #Boolean
          InferredType[T](funType(floatType, funType(floatType, booleanType)), Nil)),
      BuiltinFunction.FGt -> (
          // #Float #-> #Float #-> #Boolean
          InferredType[T](funType(floatType, funType(floatType, booleanType)), Nil)),
      BuiltinFunction.FGe -> (
          // #Float #-> #Float #-> #Boolean
          InferredType[T](funType(floatType, funType(floatType, booleanType)), Nil)),
      BuiltinFunction.FloatFromByte -> (
          // ((#Zero #| #NonZero) #& #Byte) #-> #Float
          InferredType[T](funType((zeroType | nonZeroType) & byteType, floatType), Nil)),
      BuiltinFunction.FloatFromShort -> (
          // ((#Zero #| #NonZero) #& #Short) #-> #Float
          InferredType[T](funType((zeroType | nonZeroType) & shortType, floatType), Nil)),
      BuiltinFunction.FloatFromInt -> (
          // ((#Zero #| #NonZero) #& #Int) #-> #Float
          InferredType[T](funType((zeroType | nonZeroType) & intType, floatType), Nil)),
      BuiltinFunction.FloatFromLong -> (
          // ((#Zero #| #NonZero) #& #Long) #-> #Float
          InferredType[T](funType((zeroType | nonZeroType) & longType, floatType), Nil)),
      BuiltinFunction.FloatFromDouble -> (
          // #Double #-> #Float
          InferredType[T](funType(doubleType, floatType), Nil)),
      // Double
      BuiltinFunction.DNeg -> (
          // #Double #-> #Double
          InferredType[T](funType(doubleType, doubleType), Nil)),
      BuiltinFunction.DAdd -> (
          // #Double #-> #Double #-> #Double
          InferredType[T](funType(doubleType, funType(doubleType, doubleType)), Nil)),
      BuiltinFunction.DSub -> (
          // #Double #-> #Double #-> #Double
          InferredType[T](funType(doubleType, funType(doubleType, doubleType)), Nil)),
      BuiltinFunction.DMul -> (
          // #Double #-> #Double #-> #Double
          InferredType[T](funType(doubleType, funType(doubleType, doubleType)), Nil)),
      BuiltinFunction.DDiv -> (
          // #Double #-> #Double #-> #Double
          InferredType[T](funType(doubleType, funType(doubleType, doubleType)), Nil)),
      BuiltinFunction.DEq -> (
          // #Double #-> #Double #-> #Boolean
          InferredType[T](funType(doubleType, funType(doubleType, booleanType)), Nil)),
      BuiltinFunction.DNe -> (
          // #Double #-> #Double #-> #Boolean
          InferredType[T](funType(doubleType, funType(doubleType, booleanType)), Nil)),
      BuiltinFunction.DLt -> (
          // #Double #-> #Double #-> #Boolean
          InferredType[T](funType(doubleType, funType(doubleType, booleanType)), Nil)),
      BuiltinFunction.DLe -> (
          // #Double #-> #Double #-> #Boolean
          InferredType[T](funType(doubleType, funType(doubleType, booleanType)), Nil)),
      BuiltinFunction.DGt -> (
          // #Double #-> #Double #-> #Boolean
          InferredType[T](funType(doubleType, funType(doubleType, booleanType)), Nil)),
      BuiltinFunction.DGe -> (
          // #Double #-> #Double #-> #Boolean
          InferredType[T](funType(doubleType, funType(doubleType, booleanType)), Nil)),
      BuiltinFunction.DoubleFromByte -> (
          // ((#Zero #| #NonZero) #& #Byte) #-> #Double
          InferredType[T](funType((zeroType | nonZeroType) & byteType, doubleType), Nil)),
      BuiltinFunction.DoubleFromShort -> (
          // ((#Zero #| #NonZero) #& #Short) #-> #Double
          InferredType[T](funType((zeroType | nonZeroType) & shortType, doubleType), Nil)),
      BuiltinFunction.DoubleFromInt -> (
          // ((#Zero #| #NonZero) #& #Int) #-> #Double
          InferredType[T](funType((zeroType | nonZeroType) & intType, doubleType), Nil)),
      BuiltinFunction.DoubleFromLong -> (
          // ((#Zero #| #NonZero) #& #Long) #-> #Double
          InferredType[T](funType((zeroType | nonZeroType) & longType, doubleType), Nil)),
      BuiltinFunction.DoubleFromFloat -> (
          // #Float #-> #Double
          InferredType[T](funType(floatType, doubleType), Nil)),
      BuiltinFunction.EmptyArray -> (
          // \t1 => #Empty #& (#Array t1)
          InferredType[T](emptyType & (arrayType(typeParam(0, 0))), Seq(InferredKind(Star(KindType, NoPosition))))),
      BuiltinFunction.Array -> (
          // \t1 => ((#Zero #| #NonZero) #& #Long) #-> t1 #-> ((#Empty #| #NonEmpty) #& (#Array t1))
          InferredType[T](funType((zeroType | nonZeroType) & longType, funType(typeParam(0, 0), (emptyType | nonEmptyType) & (arrayType(typeParam(0, 0))))),
              Seq(InferredKind(Star(KindType, NoPosition))))),
      BuiltinFunction.Length -> (
          // \t1 => ((#Empty #| #NonEmpty) #& (#Array t1)) #-> ((#Zero #| #NonZero) #& #Long)
          InferredType[T](funType((emptyType | nonEmptyType) & arrayType(typeParam(0, 0)), (zeroType | nonZeroType) & longType),
              Seq(InferredKind(Star(KindType, NoPosition))))),
      BuiltinFunction.MapNthOrElse -> (
          // \t1 t2 => (t1 #-> #t2) #-> #t2 #-> ((#Zero #| #NonZero) #& #Long) #-> ((#Empty #| #NonEmpty) #& (#Array t1)) #-> t2
          InferredType[T](funType(funType(typeParam(0, 0), typeParam(1, 0)), funType(typeParam(1, 0), funType((zeroType | nonZeroType) & longType, funType((emptyType | nonEmptyType) & arrayType(typeParam(0, 0)), typeParam(1, 0))))),
              Seq.fill(2)(InferredKind(Star(KindType, NoPosition))))),
      BuiltinFunction.Updated -> (
          // \t1 => ((#Zero #| #NonZero) #& #Long) #-> t1 #-> ((#Empty #| #NonEmpty) #& (#Array t1)) #-> ((#Empty #| #NonEmpty) #& (#Array t1))
          InferredType[T](funType((zeroType | nonZeroType) & longType, funType(typeParam(0, 0), funType((emptyType | nonEmptyType) & arrayType(typeParam(0, 0)), (emptyType | nonEmptyType) & arrayType(typeParam(0, 0))))),
              Seq(InferredKind(Star(KindType, NoPosition))))))
}