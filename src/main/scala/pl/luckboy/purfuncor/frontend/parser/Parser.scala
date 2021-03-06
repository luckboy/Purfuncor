/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.parser
import scala.io.Source
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.NoPosition
import scala.util.parsing.input.Positional
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.common.Arrow
import pl.luckboy.purfuncor.frontend.Bind

object Parser extends StandardTokenParsers with PackratParsers
{
  override val lexical = Lexer()
  
  object NlMode extends Enumeration
  {
    val Nl, NoNl = Value
  }
  
  case class RichParser[+T](parser: Parser[T])
  {
    def ~~ [U](parser2: Parser[U])(implicit nlMode: NlMode.Value) =
      nlMode match {
        case NlMode.Nl   => this ~- parser2
        case NlMode.NoNl => parser ~ parser2
      }
    
    def ~~> [U](parser2: Parser[U])(implicit nlMode: NlMode.Value) =
      nlMode match {
        case NlMode.Nl   => this ~-> parser2
        case NlMode.NoNl => parser ~> parser2
      }

    def <~~ [U](parser2: Parser[U])(implicit nlMode: NlMode.Value) =
      nlMode match {
        case NlMode.Nl   => this <~- parser2
        case NlMode.NoNl => parser <~ parser2
      }
    
    def ~+ (implicit nlMode: NlMode.Value) =
      nlMode match {
        case NlMode.Nl   => this -+
        case NlMode.NoNl => parser +
      }

    def ~:+ (implicit nlMode: NlMode.Value) =
      nlMode match {
        case NlMode.Nl   => this -:+
        case NlMode.NoNl => this :+
      }
    
    def ~* (implicit nlMode: NlMode.Value) =
      nlMode match {
        case NlMode.Nl   => this -*
        case NlMode.NoNl => parser *
      }
    
    def ~- [U](parser2: Parser[U]) = parser ~ (rep("\n") ~> parser2)
    
    def ~-> [U](parser2: Parser[U]) = parser ~ rep("\n") ~> parser2
    
    def <~- [U](parser2: Parser[U]) = parser <~ rep("\n") ~ parser2 
    
    def -+ = parser ~ ((rep("\n") ~> parser) *) ^^ { case x ~ xs => x :: xs }
    
    def -:+ = parser ~ ((rep("\n") ~> parser) *) ^^ { case x ~ xs => NonEmptyList.nel(x, xs) }
    
    def -* = ((this -+) ?) ^^ { _.getOrElse(Nil) }
    
    def :+ = parser ~ (parser *) ^^ { case x ~ xs => NonEmptyList.nel(x, xs) }
  }
  
  implicit def parserToRichParser[T](parser: Parser[T]) = RichParser(parser)
  implicit def elemToRighParser(e: Elem) = RichParser(elem(e))
  implicit def stringToRichParser(s: String) = RichParser(s)
  
  case class TermWrapper(term: Term[SimpleTerm[Symbol, LambdaInfo, TypeSimpleTerm[Symbol, TypeLambdaInfo]]]) extends Positional
  case class TypeTermWrapper(typeTerm: Term[TypeSimpleTerm[Symbol, TypeLambdaInfo]]) extends Positional
  case class SymbolWrapper(sym: Symbol) extends Positional
  case class ModuleSymbolWrapper(sym: ModuleSymbol) extends Positional
  case class BindWrapper(bind: Bind[Symbol, LambdaInfo, TypeSimpleTerm[Symbol, TypeLambdaInfo]]) extends Positional
  case class ArgWrapper(arg: Arg[TypeSimpleTerm[Symbol, TypeLambdaInfo]]) extends Positional
  case class TypeArgWrapper(typeArg: TypeArg) extends Positional
  case class KindTermWrapper(kindTerm: KindTerm[StarKindTerm[String]]) extends Positional

  implicit def termWrapperToTerm(wrapper: TermWrapper) =
    wrapper.term match {
      case term @ App(_, _, _) => term.copy(pos = wrapper.pos) 
      case term @ Simple(_, _) => term.copy(pos = wrapper.pos)
    }
  implicit def termWrapperNelToTermNel(wrappers: NonEmptyList[TermWrapper]) = wrappers.map { termWrapperToTerm(_) }
  implicit def typeTermWrapperToTypeTerm(wrapper: TypeTermWrapper) =
    wrapper.typeTerm match {
      case term @ App(_, _, _) => term.copy(pos = wrapper.pos)
      case term @ Simple(_, _) => term.copy(pos = wrapper.pos)
    }
  implicit def typeWrapperOptionToTypeTermOption(wrapper: Option[TypeTermWrapper]) = wrapper.map(typeTermWrapperToTypeTerm)
  implicit def typeTermWrapperNelToTypeTermNel(wrappers: NonEmptyList[TypeTermWrapper]) = wrappers.map { typeTermWrapperToTypeTerm(_) }
  implicit def typeTermWrappersToTypeTerms(wrappers: List[TypeTermWrapper]) = wrappers.map { typeTermWrapperToTypeTerm(_) }
  implicit def symbolWrapperToSymbol(wrapper: SymbolWrapper) = 
    wrapper.sym match {
      case sym @ GlobalSymbol(_, _) => sym.copy(pos = wrapper.pos)
      case sym @ NormalSymbol(_, _) => sym.copy(pos = wrapper.pos)
    }
  implicit def moduleSymbolWrapperToModuleSymbol(wrapper: ModuleSymbolWrapper) = 
    wrapper.sym match {
      case sym @ GlobalModuleSymbol(_, _) => sym.copy(pos = wrapper.pos)
      case sym @ NormalModuleSymbol(_, _) => sym.copy(pos = wrapper.pos)
    }
  implicit def bindWrapperToBind(wrapper: BindWrapper) = wrapper.bind.copy(pos = wrapper.pos)
  implicit def bindWrapperNelToBindNel(wrappers: NonEmptyList[BindWrapper]) = wrappers.map { bindWrapperToBind(_) }
  implicit def argWrapperToArg(wrapper: ArgWrapper) = wrapper.arg.copy(pos = wrapper.pos)
  implicit def argWrapperNelToArgNel(wrappers: NonEmptyList[ArgWrapper]) = wrappers.map { argWrapperToArg(_) }
  implicit def argWrappersToArgs(wrappers: List[ArgWrapper]) = wrappers.map { argWrapperToArg(_) }
  implicit def typeArgWrapperToTypeArg(wrapper: TypeArgWrapper) = wrapper.typeArg.copy(pos = wrapper.pos)
  implicit def typeArgWrapperNelToTypeArgNel(wrappers: NonEmptyList[TypeArgWrapper]) = wrappers.map { typeArgWrapperToTypeArg(_) }
  implicit def typeArgWrappersToTypeArgs(wrappers: List[TypeArgWrapper]) = wrappers.map { typeArgWrapperToTypeArg(_) }
  implicit def kindTermWrapperToKindTerm(wrapper: KindTermWrapper): KindTerm[StarKindTerm[String]] =
    wrapper.kindTerm match {
      case kindTerm @ Arrow(_, _, _) => kindTerm.copy(pos = wrapper.pos)
      case kindTerm @ Star(_, _)     => kindTerm.copy(pos = wrapper.pos)
    }
  implicit def kindTermWrapperOptionToKindTermOption(wrapper: Option[KindTermWrapper]) = wrapper.map(kindTermWrapperToKindTerm)
  
  implicit def termToTermWrapper(term: Term[SimpleTerm[Symbol, LambdaInfo, TypeSimpleTerm[Symbol, TypeLambdaInfo]]]) = TermWrapper(term)
  implicit def typeTermToTypeTermWrapper(typeTerm: Term[TypeSimpleTerm[Symbol, TypeLambdaInfo]]) = TypeTermWrapper(typeTerm)
  implicit def symbolToSymbolWrapper(sym: Symbol) = SymbolWrapper(sym)
  implicit def moduleSymbolToModuleSymbolWrapper(sym: ModuleSymbol) = ModuleSymbolWrapper(sym)
  implicit def bindToBindWrapper(bind: Bind[Symbol, LambdaInfo, TypeSimpleTerm[Symbol, TypeLambdaInfo]]) = BindWrapper(bind)
  implicit def argToArgWrapper(arg: Arg[TypeSimpleTerm[Symbol, TypeLambdaInfo]]) = ArgWrapper(arg)
  implicit def typeArgToTypeArgWrapper(typeArg: TypeArg) = TypeArgWrapper(typeArg)
  implicit def kindTermToKindTermWrapper(kindTerm: KindTerm[StarKindTerm[String]]) = KindTermWrapper(kindTerm)
  
  def p[T, U <: Positional](parser: Parser[T])(implicit f: T => U) = positioned(parser ^^ f)

  lazy val semi = rep1("\n") | (rep("\n") ~ ";" ~ rep("\n"))

  def parseInteger[T](s: String)(f: (String, Int) => T) =
    if(s.startsWith("0x") || s.startsWith("0X"))
      f(s.substring(2), 16)
    else if(s.startsWith("0") && s.length >= 2)
      f(s.substring(1), 8)
    else
      f(s, 10)

  lazy val literalValue = (
      booleanValue
      | charValue
      | byteValue
      | shortValue
      | intValue
      | longValue
      | floatValue
      | doubleValue
      | tupleFunValue
      | tupleFieldFunValue
      | builtinFunValue)
  lazy val booleanValue = falseValue | trueValue
  lazy val falseValue = "false"											^^^ BooleanValue(false)
  lazy val trueValue = "true"											^^^ BooleanValue(true)
  lazy val charValue = elem("char", _.isInstanceOf[lexical.CharLit])	^^ { t => CharValue(t.chars.head) }
  lazy val byteValue = elem("byte", _.isInstanceOf[lexical.ByteLit])	^^ { t => ByteValue(parseInteger(t.chars)(java.lang.Byte.parseByte)) }
  lazy val shortValue = elem("short", _.isInstanceOf[lexical.ShortLit]) ^^ { t => ShortValue(parseInteger(t.chars)(java.lang.Short.parseShort)) }
  lazy val intValue = elem("int", _.isInstanceOf[lexical.IntLit])		^^ { t => IntValue(parseInteger(t.chars)(Integer.parseInt)) }
  lazy val longValue = elem("long", _.isInstanceOf[lexical.LongLit])	^^ { t => LongValue(parseInteger(t.chars)(java.lang.Long.parseLong)) }  
  lazy val floatValue = elem("float", _.isInstanceOf[lexical.FloatLit]) ^^ { t => FloatValue(java.lang.Float.parseFloat(t.chars)) }
  lazy val doubleValue = elem("double", _.isInstanceOf[lexical.DoubleLit]) ^^ { t => DoubleValue(java.lang.Double.parseDouble(t.chars)) }
  lazy val tupleFunValue = "tuple" ~-> integer 							^^ TupleFunValue
  lazy val tupleFieldFunValue = "#" ~-> integer	 ~- integer				^? ({ 
    case n ~ i if n >= i && n > 0 => TupleFieldFunValue(n, i - 1) 
  }, _ => "incorrect number of fields")
  lazy val builtinFunValue = "#" ~-> ident								^? ({
    case s if BuiltinFunction.values.exists { _.toString === s } => BuiltinFunValue(BuiltinFunction.withName(s))
  }, "unknown built-in function " + _)
  
  lazy val typeLiteralValue = tupleTypeFunValue | typeBuiltinFunValue
  lazy val tupleTypeFunValue = "tuple" ~-> integer						^^ TupleTypeFunValue
  lazy val typeBuiltinFunValue = typeBuiltinFunValue1 | typeBuiltinFunValue2
  lazy val typeBuiltinFunValue1 = "#" ~-> ident							^? ({
    case s if TypeBuiltinFunction.values.exists { _.toString === s } => TypeBuiltinFunValue(TypeBuiltinFunction.withName(s))
  }, "unknown built-in type function " + _)
  lazy val typeBuiltinFunValue2 = "##" ~> ("&" | "|" | "->")			^? ({
    case s if TypeBuiltinFunction.values.exists { _.toString === s } => TypeBuiltinFunValue(TypeBuiltinFunction.withName(s))
  }, "unknown built-in type function " + _)
  
  lazy val integer = elem("integer", _.isInstanceOf[lexical.IntLit])	^^ { t => parseInteger(t.chars)(Integer.parseInt)}
  
  lazy val bind = p(ident ~ ("=" ~-> noNlParsers.expr)					^^ { case s ~ t => Bind(s, t, NoPosition) })
  lazy val binds = bind ~ ((semi ~> bind) *)							^^ { case b ~ bs => NonEmptyList.nel(b, bs) }
  lazy val arg = arg1 | arg2
  lazy val arg1 = wildcardArg1 | namedArg1
  lazy val wildcardArg1 = p("_"											^^^ Arg(none, none, NoPosition))
  lazy val namedArg1 = p(ident	 										^^ { case s => Arg(some(s), none, NoPosition) })
  lazy val arg2 = wildcardArg2 | namedArg2
  lazy val wildcardArg2 = p("(" ~- "_" ~-> ((":" ~-> nlParsers.typeExpr) ?) <~- ")" ^^ { case tt => Arg(none, tt, NoPosition) })
  lazy val namedArg2 = p("(" ~-> ident ~- ((":" ~-> nlParsers.typeExpr) ?) <~- ")" ^^ { case s ~ tt => Arg(some(s), tt, NoPosition) })
  lazy val typeArg = typeArg1 | typeArg2
  lazy val typeArg1 = wildcardTypeArg1 | namedTypeArg1
  lazy val wildcardTypeArg1 = p("_"										^^^ TypeArg(none, none, NoPosition))
  lazy val namedTypeArg1 = p(ident	 									^^ { case s => TypeArg(some(s), none, NoPosition) })
  lazy val typeArg2 = wildcardTypeArg2 | namedTypeArg2
  lazy val wildcardTypeArg2 = p("(" ~- "_" ~-> ((":" ~-> nlParsers.kindExpr) ?) <~- ")" ^^ { case kt => TypeArg(none, kt, NoPosition) })
  lazy val namedTypeArg2 = p("(" ~-> ident ~- ((":" ~-> nlParsers.kindExpr) ?) <~- ")" ^^ { case s ~ kt => TypeArg(some(s), kt, NoPosition) })
  
  case class Parsers()(implicit nlMode: NlMode.Value)
  {
    lazy val symbol = globalSymbol | normalSymbol
    lazy val normalSymbol = p(ident ~~ (("." ~-> ident) ~*)				^^ { case s ~ ss => NormalSymbol(NonEmptyList.nel(s, ss), NoPosition) })
    lazy val globalSymbol = p("#" ~~ "." ~-> ident ~~ (("." ~-> ident) ~*) ^^ { case s ~ ss => GlobalSymbol(NonEmptyList.nel(s, ss), NoPosition) })
    
    lazy val moduleSymbol = globalModuleSymbol | normalModuleSymbol
    lazy val normalModuleSymbol = p(ident ~~ (("." ~-> ident) ~*)		^^ { case s ~ ss => NormalModuleSymbol(NonEmptyList.nel(s, ss), NoPosition) })
    lazy val globalModuleSymbol = p("#" ~~> (("." ~-> ident) ~*)		^^ { case ss => GlobalModuleSymbol(ss, NoPosition) })
    
    lazy val expr: PackratParser[TermWrapper] = expr1

    lazy val expr1 = typedExpr | expr2
    lazy val typedExpr = p(expr2 ~~ (":" ~-> typeExpr)					^^ { case t ~ tt => Simple(TypedTerm(t, tt), NoPosition) })    

    lazy val expr2 = select | extract | exprN
    lazy val select = p(exprN ~ (("select" ~- "{") ~-> (cas ~ ((semi ~> cas) *)) <~- "}") ^^ {
      case t ~ (c ~ cs) => Simple(Select(t, NonEmptyList.nel(c, cs), LambdaInfo), NoPosition)
    })
    lazy val cas = namedCase | wildcardCase
    lazy val namedCase = (("(" ~-> ident ~~ (":" ~-> typeExpr)) <~- ")") ~- ("=>" ~-> expr) ^^ { case (s ~ tt) ~ t => Case(some(s), tt, t, LambdaInfo) }
    lazy val wildcardCase = (("(" ~-> ("_" ~~ ":") ~-> typeExpr) <~- ")") ~- ("=>" ~-> expr) ^^ { case tt ~ t => Case(none, tt, t, LambdaInfo) }
    lazy val extract = p(exprN ~ (("extract" ~- "{") ~-> (arg :+) ~- ("=>" ~-> expr) <~- "}") ^^ {
      case t1 ~ (as ~ t2) => Simple(Extract(t1, as, t2, LambdaInfo), NoPosition)
    })
    
    lazy val exprN = app | let | lambda | simpleExpr
    lazy val simpleExpr: PackratParser[TermWrapper] = variable | literal | construct | "(" ~-> expr <~- ")"
    
    lazy val app = p(simpleExpr ~~ (simpleExpr ~:+)						^^ { case t ~ ts => App(t, ts, NoPosition) })
    lazy val let = p("let" ~-> binds ~- ("in" ~-> expr)					^^ { case bs ~ t => Simple(Let(bs, t, LambdaInfo), NoPosition) })
    lazy val lambda = p("\\" ~> (arg :+) ~- ("=>" ~-> expr)				^^ { case as ~ t => Simple(Lambda(as, t, LambdaInfo), NoPosition) })
    lazy val variable = p(symbol										^^ { case s => Simple(Var[Symbol, LambdaInfo, TypeSimpleTerm[Symbol, TypeLambdaInfo]](s, LambdaInfo), NoPosition) })
    lazy val literal = p(literalValue									^^ { case v => Simple(Literal(v), NoPosition) })
    lazy val construct =  p("construct" ~-> integer						^^ { case n => Simple(Construct(n, LambdaInfo), NoPosition) })
    
    lazy val typeExpr: PackratParser[TypeTermWrapper] = typeExpr1
    
    lazy val typeExpr1 = kindedTypeExpr | typeExprN
    lazy val kindedTypeExpr = p(typeExprN ~~ (":" ~-> kindExpr)			^^ { case t ~ kt => Simple(KindedTypeTerm(t, kt), NoPosition) })
    lazy val typeExprN = typeApp | typeLambda | simpleTypeExpr
    lazy val simpleTypeExpr = typeVariable | typeLiteral | "(" ~-> typeExpr <~- ")"
    
    lazy val typeApp = p(simpleTypeExpr ~~ (simpleTypeExpr ~:+)			^^ { case t ~ ts => App(t, ts, NoPosition) })
    lazy val typeLambda = p("\\" ~> (typeArg :+) ~- ("=>" ~-> typeExpr)	 ^^ { case as ~ t => Simple(TypeLambda(as, t, TypeLambdaInfo), NoPosition) })
    lazy val typeVariable = p(symbol									^^ { case s => Simple(TypeVar[Symbol, TypeLambdaInfo](s), NoPosition) })
    lazy val typeLiteral = p(typeLiteralValue							^^ { case v => Simple(TypeLiteral(v), NoPosition) })
    
    lazy val kindExpr: PackratParser[KindTermWrapper] = kindExpr1

    lazy val kindExpr1: PackratParser[KindTermWrapper] = arrow | kindExprN
    lazy val arrow = p(kindExprN ~~ ("->" ~-> kindExpr1)				^^ { case t1 ~ t2 => Arrow(t1, t2, NoPosition) })
    lazy val kindExprN = kindParam | star | "(" ~-> kindExpr <~- ")"

    lazy val kindParam = p(ident										^^ { case s => Star(KindParam(s), NoPosition) })
    lazy val star = p("*"												^^^ Star(KindType, NoPosition))
  }
  
  val nlParsers = Parsers()(NlMode.Nl)
  val noNlParsers = Parsers()(NlMode.NoNl)

  lazy val definition: PackratParser[Def] = (
      importDef |
      combinatorDef |
      polyCombinatorDef |
      typeCombinatorDef |
      unittypeCombinatorDef |
      moduleDef |
      instanceDef |
      selectConstructInstanceDef)
  lazy val defs = definition ~ ((semi ~> definition) *)					^^ { case d ~ ds => d :: ds }

  lazy val importDef = "import" ~-> noNlParsers.moduleSymbol			^^ { ImportDef(_) }
  lazy val combinatorDef = combinatorDefPart ~ (arg *) ~- ("=" ~-> noNlParsers.expr) ^^ { case (s, tt) ~ as ~ t => CombinatorDef(s, tt, as, t) }
  lazy val polyCombinatorDef = "poly" ~-> combinatorDefPart			^^ { case (s, tt) => PolyCombinatorDef(s, tt) }
  lazy val typeCombinatorDef = "type" ~-> typeCombinatorDefPart ~ (typeArg *) ~- ("=" ~-> noNlParsers.typeExpr) ^^ { case (s, kt) ~ as ~ t => TypeCombinatorDef(s, kt, as, t) }
  lazy val unittypeCombinatorDef = "unittype" ~-> integer ~- typeCombinatorDefPart ^^ { case n ~ ((s, kt)) => UnittypeCombinatorDef(n, s, kt) }
  lazy val moduleDef = "module" ~-> noNlParsers.moduleSymbol ~- ("{" ~-> defs <~- "}") ^^ { case s ~ ds => ModuleDef(s, ds) }
  lazy val instanceDef = "instance" ~-> noNlParsers.symbol ~- ("=>" ~-> noNlParsers.symbol) ^^ { case s ~ s2 => InstanceDef(s, s2) }
  lazy val selectConstructInstanceDef = ("instance" ~- "select") ~-> noNlParsers.typeExpr ~- (("construct" ~- "{") ~-> typeExprs <~- "}") ^^ { case tt ~ (tt2 ~ tts) => SelectConstructInstanceDef(tt, NonEmptyList.nel(tt2, tts)) }
  lazy val typeExprs = noNlParsers.typeExpr ~ ((semi ~-> noNlParsers.typeExpr) *)

  lazy val combinatorDefPart = combinatorDefPart1 | combinatorDefPart2
  lazy val combinatorDefPart1 = noNlParsers.symbol						^^ { case s => (s, None) }
  lazy val combinatorDefPart2 = "(" ~-> noNlParsers.symbol ~- ((":" ~-> noNlParsers.typeExpr) ?) <~- ")" ^^ { case s ~ tt => (s, tt) }
    
  lazy val typeCombinatorDefPart = typeCombinatorDefPart1 | typeCombinatorDefPart2
  lazy val typeCombinatorDefPart1 = noNlParsers.symbol					^^ { case s => (s, None) }
  lazy val typeCombinatorDefPart2 = "(" ~-> noNlParsers.symbol ~- ((":" ~-> noNlParsers.kindExpr) ?) <~- ")" ^^ { case s ~ kt => (s, kt) }

  
  lazy val parseTree = rep("\n") ~> defs <~ rep("\n")					^^ ParseTree
  
  lazy val parseTerm = rep("\n") ~> noNlParsers.expr <~ rep("\n")
  
  lazy val parseTypeTerm = rep("\n") ~> noNlParsers.typeExpr <~ rep("\n")
  
  def parseString(s: String) =
    phrase(parseTree)(new lexical.Scanner(s)) match {
      case Success(parseTree, _) => parseTree.success
      case Failure(msg, next)    => common.Error(msg, none, next.pos).failureNel
      case Error(msg, next)      => common.FatalError(msg, none, next.pos).failureNel
    }
  
  def parseInputStream(in: java.io.InputStream) =
    parseString(Source.fromInputStream(in).mkString(""))
    
  def parseFile(file: java.io.File) = {
    val res = try {
      val in = new java.io.FileInputStream(file)
      try {
        parseInputStream(in)
      } finally {
        in.close()
      }
    } catch {
      case e: java.io.IOException => common.IOError(e.getMessage(), none).failureNel
    }
    common.Result.resultForFile(res, some(file))
  }
  
  def parseTermString(s: String) =
    phrase(parseTerm)(new lexical.Scanner(s)) match {
      case Success(termWrapper, _) => termWrapperToTerm(termWrapper).successNel[AbstractError]
      case Failure(msg, next)      => common.Error(msg, none, next.pos).failureNel
      case Error(msg, next)        => common.FatalError(msg, none, next.pos).failureNel
    }
  
  def parseTypeTermString(s: String) =
    phrase(parseTypeTerm)(new lexical.Scanner(s)) match {
      case Success(typeTermWrapper, _) => typeTermWrapperToTypeTerm(typeTermWrapper).successNel[AbstractError]
      case Failure(msg, next)          => common.Error(msg, none, next.pos).failureNel
      case Error(msg, next)            => common.FatalError(msg, none, next.pos).failureNel
    }
}
