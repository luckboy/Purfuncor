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
  
  case class TermWrapper(term: Term[SimpleTerm[Symbol, LetInfo]]) extends Positional
  case class SymbolWrapper(sym: Symbol) extends Positional
  case class ModuleSymbolWrapper(sym: ModuleSymbol) extends Positional
  case class BindWrapper(bind: Bind[Symbol, LetInfo]) extends Positional
  case class ArgWrapper(arg: Arg) extends Positional

  implicit def termWrapperToTerm(wrapper: TermWrapper) =
    wrapper.term match {
      case term @ App(_, _, info) => term.copy(pos = wrapper.pos) 
      case term @ Simple(_, info) => term.copy(pos = wrapper.pos)
    }
  implicit def termWrapperNelToTermNel(wrappers: NonEmptyList[TermWrapper]) = wrappers.map { termWrapperToTerm(_) }
  implicit def symbolWrapperToSymbol(wrapper: SymbolWrapper) = 
    wrapper.sym match {
      case sym @ GlobalSymbol(names, _) => sym.copy(pos = wrapper.pos)
      case sym @ NormalSymbol(names, _) => sym.copy(pos = wrapper.pos)
    }
  implicit def moduleSymbolWrapperToModuleSymbol(wrapper: ModuleSymbolWrapper) = 
    wrapper.sym match {
      case sym @ GlobalModuleSymbol(names, _) => sym.copy(pos = wrapper.pos)
      case sym @ NormalModuleSymbol(names, _) => sym.copy(pos = wrapper.pos)
    }
  implicit def bindWrapperToBind(wrapper: BindWrapper) = wrapper.bind.copy(pos = wrapper.pos)
  implicit def bindWrapperNelToBindNel(wrappers: NonEmptyList[BindWrapper]) = wrappers.map { bindWrapperToBind(_) }
  implicit def argWrapperToArg(wrapper: ArgWrapper) = wrapper.arg.copy(pos = wrapper.pos)
  implicit def argWrapperNelToArgNel(wrappers: NonEmptyList[ArgWrapper]) = wrappers.map { argWrapperToArg(_) }
  implicit def argWrappersToArgs(wrappers: List[ArgWrapper]) = wrappers.map { argWrapperToArg(_)}
  
  implicit def termToWrapperTerm(term: Term[SimpleTerm[Symbol, LetInfo]]) = TermWrapper(term)
  implicit def symbolWrapperToSymbol(sym: Symbol) = SymbolWrapper(sym)
  implicit def modulesymbolWrapperToSymbol(sym: ModuleSymbol) = ModuleSymbolWrapper(sym)
  implicit def bindWrapperToBind(bind: Bind[Symbol, LetInfo]) = BindWrapper(bind)
  implicit def argWrapperToArg(arg: Arg) = ArgWrapper(arg)
  
  def p[T, U <: Positional](parser: Parser[T])(implicit f: T => U) = positioned(parser ^^ f)

  lazy val semi = rep1("\n") | (rep("\n") ~ ";" ~ rep("\n"))

  def parseInteger[T](s: String)(f: (String, Int) => T) =
    if(s.startsWith("0x") || s.startsWith("0X"))
      f(s, 16)
    else if(s.startsWith("0") && s.length >= 2)
      f(s, 8)
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
  lazy val tupleFieldFunValue = "#" ~-> integer							^^ { n => TupleFieldFunValue(n - 1) }
  lazy val builtinFunValue = "#" ~-> ident								^? ({
    case s if BuiltinFunction.values.exists { _.toString === s } => BuiltinFunValue(BuiltinFunction.withName(s))
  }, "unknown built-in function " + _)
  
  lazy val integer = elem("integer", _.isInstanceOf[lexical.IntLit])	^^ { t => parseInteger(t.chars)(Integer.parseInt)}
  
  lazy val bind = p(ident ~ ("=" ~-> noNlParsers.expr)					^^ { case s ~ t => Bind(s, t, NoPosition) })
  lazy val binds = bind ~ ((semi ~> bind) *)							^^ { case b ~ bs => NonEmptyList.nel(b, bs) }
  lazy val arg = wildcardArg | namedArg
  lazy val wildcardArg = p("_"											^^^ Arg(none, NoPosition))
  lazy val namedArg = p(ident	 										^^ { case s => Arg(some(s), NoPosition) })
    
  case class Parsers()(implicit nlMode: NlMode.Value)
  {
    lazy val symbol = globalSymbol | normalSymbol
    lazy val normalSymbol = p(ident ~~ (("." ~-> ident) ~*)				^^ { case s ~ ss => NormalSymbol(NonEmptyList.nel(s, ss), NoPosition) })
    lazy val globalSymbol = p("#" ~~ "." ~-> ident ~~ (("." ~-> ident) ~*) ^^ { case s ~ ss => GlobalSymbol(NonEmptyList.nel(s, ss), NoPosition) })
    
    lazy val moduleSymbol = globalModuleSymbol | normalModuleSymbol
    lazy val normalModuleSymbol = p(ident ~~ (("." ~-> ident) ~*)		^^ { case s ~ ss => NormalModuleSymbol(NonEmptyList.nel(s, ss), NoPosition) })
    lazy val globalModuleSymbol = p("#" ~~> (("." ~-> ident) ~*)			^^ { case ss => GlobalModuleSymbol(ss, NoPosition) })
    
    lazy val expr: PackratParser[TermWrapper] = exprN

    lazy val exprN = app | let | lambda | simpleExpr
    lazy val simpleExpr: PackratParser[TermWrapper] = variable | literal | "(" ~-> expr <~- ")"
    
    lazy val app = p(simpleExpr ~~ (simpleExpr ~:+)						^^ { case t ~ ts => App(t, ts, NoPosition) })
    lazy val let = p("let" ~-> binds ~- ("in" ~-> expr)					^^ { case bs ~ t => Simple(Let(bs, t, LetInfo), NoPosition) })
    lazy val lambda = p("\\" ~> (arg :+) ~- ("=>" ~-> expr)				^^ { case as ~ t => Simple(Lambda(as, t, LetInfo), NoPosition) })
    lazy val variable = p(symbol										^^ { case s => Simple(Var[Symbol, LetInfo](s), NoPosition) })
    lazy val literal = p(literalValue									^^ { case v => Simple(Literal(v), NoPosition) })
  }
  
  val nlParsers = Parsers()(NlMode.Nl)
  val noNlParsers = Parsers()(NlMode.NoNl)

  lazy val definition: PackratParser[Def] = importDef | combinatorDef | moduleDef
  lazy val defs = definition ~ ((semi ~> definition) *)					^^ { case d ~ ds => d :: ds }

  lazy val importDef = "import" ~-> noNlParsers.moduleSymbol			^^ { ImportDef(_) }
  lazy val combinatorDef = noNlParsers.symbol ~ (arg *) ~- ("=" ~-> noNlParsers.expr) ^^ { case s ~ as ~ t => CombinatorDef(s, as, t) }
  lazy val moduleDef = "module" ~-> noNlParsers.moduleSymbol ~- ("{" ~-> defs <~- "}") ^^ { case s ~ ds => ModuleDef(s, ds) }
  
  lazy val parseTree = rep("\n") ~> defs <~ rep("\n")					^^ ParseTree
  
  lazy val parseTerm = rep("\n") ~> noNlParsers.expr <~ rep("\n")
  
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
      case Success(termWrapper, _) => termWrapperToTerm(termWrapper).success
      case Failure(msg, next)      => common.Error(msg, none, next.pos).failureNel
      case Error(msg, next)        => common.FatalError(msg, none, next.pos).failureNel
    }
}