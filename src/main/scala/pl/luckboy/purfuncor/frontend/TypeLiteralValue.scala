package pl.luckboy.purfuncor.frontend

sealed trait TypeLiteralValue
case class TupleTypeFunValue(n: Int) extends TypeLiteralValue
case class TypeBuiltinFunValue(bf: TypeBuiltinFunction.Value) extends TypeLiteralValue