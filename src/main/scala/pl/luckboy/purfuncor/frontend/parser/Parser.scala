package pl.luckboy.purfuncor.frontend.parser
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.NoPosition
import scala.util.parsing.input.Positional
import scalaz._
import Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._

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
  
  case class TermWrapper(term: Term[SimpleTerm]) extends Positional
  case class SymbolWrapper(sym: Symbol) extends Positional
  case class BindWrapper(bind: Bind) extends Positional
  case class ArgWrapper(arg: Arg) extends Positional

  implicit def termWrapperToTerm(wrapper: TermWrapper) =
    wrapper.term match {
      case term @ App(_, _, info) => term.copy(pos = wrapper.pos) 
      case term @ Simple(_, info) => term.copy(pos = wrapper.pos)
    }
  implicit def termWrapperNelToTermNel(wrappers: NonEmptyList[TermWrapper]) = wrappers.map { termWrapperToTerm(_) }
  implicit def symbolWrapperToSymbol(wrapper: SymbolWrapper) = wrapper.sym.copy(pos = wrapper.pos)
  implicit def bindWrapperToBind(wrapper: BindWrapper) = wrapper.bind.copy(pos = wrapper.pos)
  implicit def bindWrapperNelToBindNel(wrappers: NonEmptyList[BindWrapper]) = wrappers.map { bindWrapperToBind(_) }
  implicit def argWrapperToArg(wrapper: ArgWrapper) = wrapper.arg.copy(pos = wrapper.pos)
  implicit def argWrapperNelToArgNel(wrappers: NonEmptyList[ArgWrapper]) = wrappers.map { argWrapperToArg(_) }
  implicit def argWrappersToArgs(wrappers: List[ArgWrapper]) = wrappers.map { argWrapperToArg(_)}
  
  implicit def termToWrapperTerm(term: Term[SimpleTerm]) = TermWrapper(term)
  implicit def wrapperSymbolToSymbol(sym: Symbol) = SymbolWrapper(sym)
  implicit def wrapperBindToBind(bind: Bind) = BindWrapper(bind)
  implicit def wrapperArgToArg(arg: Arg) = ArgWrapper(arg)
  
  def p[T, U <: Positional](parser: Parser[T])(implicit f: T => U) = positioned(parser ^^ f)

  lazy val semi = rep1("\n") | (rep("\n") ~ ";" ~ rep("\n"))

  def parseInteger[T](s: String)(f: (String, Int) => T) =
    if(s.startsWith("0x") || s.startsWith("0X"))
      f(s, 16)
    else if(s.startsWith("0") && s.length >= 2)
      f(s, 8)
    else
      f(s, 10)

  lazy val literalValue = booleanValue | charValue | byteValue | shortValue | intValue | longValue | floatValue | doubleValue | builtinFunValue
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
  lazy val builtinFunValue = "#" ~-> ident								^? ({
    case s if BuiltinFunction.values.forall { _.toString =/= s } => BuiltinFunValue(BuiltinFunction.withName(s))
  }, "unknown built-in function " + _)
  
  lazy val bind = p(ident ~ ("=" ~-> noNlParsers.expr)					^^ { case s ~ t => Bind(s, t, NoPosition) })
  lazy val binds = bind ~ ((semi ~> bind) *)							^^ { case b ~ bs => NonEmptyList.nel(b, bs) }
  lazy val arg = p(ident	 											^^ { Arg(_, NoPosition) })
    
  case class Parsers()(implicit nlMode: NlMode.Value)
  {
    lazy val symbol = p(ident ~~ (("." ~~> ident) *)					^^ { case s ~ ss => Symbol(NonEmptyList.nel(s, ss), NoPosition) })
    
    lazy val expr: PackratParser[TermWrapper] = exprN

    lazy val exprN = app | let | lambda | simpleExpr
    lazy val simpleExpr: PackratParser[TermWrapper] = variable | "(" ~-> expr <~- ")"
    
    lazy val app = p(simpleExpr ~~ (simpleExpr ~:+)						^^ { case t1 ~ ts2 => App(t1, ts2, NoPosition) })
    lazy val let = p("let" ~-> binds ~- ("in" ~-> expr)					^^ { case bs ~ t => Simple(Let(bs, t), NoPosition) })
    lazy val lambda = p("\\" ~> (arg :+) ~- ("=>" ~-> expr)				^^ { case as ~ t => Simple(Lambda(as, t), NoPosition) })
    lazy val variable = p(symbol										^^ { case s => Simple(Var(s), NoPosition) })
    lazy val literal = p(literalValue									^^ { case v => Simple(Literal(v), NoPosition) })
  }
  
  val nlParsers = Parsers()(NlMode.Nl)
  val noNlParsers = Parsers()(NlMode.NoNl)

  lazy val definition: PackratParser[Def] = combinatorDef | moduleDef
  lazy val defs = definition ~ ((semi ~> definition) *)					^^ { case d ~ ds => d :: ds }

  lazy val combinatorDef = noNlParsers.symbol ~ (arg *) ~- ("=" ~-> noNlParsers.expr) ^^ { case s ~ as ~ t => CombinatorDef(s, as, t) }
  lazy val moduleDef = "module" ~> noNlParsers.symbol ~- ("{" ~-> defs <~- ")") ^^ { case s ~ ds => ModuleDef(s, ds) }
  
  lazy val parseTree = rep("\n") ~> defs <~ rep("\n")					^^ ParseTree
}