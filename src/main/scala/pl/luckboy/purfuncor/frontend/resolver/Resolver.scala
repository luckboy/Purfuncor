package pl.luckboy.purfuncor.frontend.resolver
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.Bind

object Resolver
{
  def transformTermNel[T](terms: NonEmptyList[Term[SimpleTerm[parser.Symbol, T]]])(scope: Scope) =
    terms.tail.foldLeft(transformTerm(terms.head)(scope).map { NonEmptyList(_) }) { 
      (res, t) => (transformTerm(t)(scope) |@| res)(_ <:: _)
    }.map { _.reverse }
  
  def transformBind[T](bind: Bind[parser.Symbol, T])(scope: Scope) =
    bind match {
      case Bind(name, body, pos) => transformTerm(body)(scope).map { Bind(name, _, pos) }
    }

  def transformBindNel[T](binds: NonEmptyList[Bind[parser.Symbol, T]])(scope: Scope) =
    binds.tail.foldLeft((transformBind(binds.head)(scope).map { NonEmptyList(_) }, Set(binds.head.name))) {
      case ((res, usedNames), b) => 
        val res2 = if(usedNames.contains(b.name))
          (res |@| Error("already defined local variable " + b.name, none, b.pos).failureNel[Unit]) { (bs, _) => bs }
        else 
          res
        ((transformBind(b)(scope) |@| res2)(_ <:: _), usedNames + b.name)
    }._1.map { _.reverse }
    
  def transformArgNel(args: NonEmptyList[Arg]) =
    args.foldLeft((().successNel[AbstractError], Set() ++ args.head.name)) { 
      case (p @ (res, usedNames), a) =>
        a.name.map {
          name =>
            val res2 = if(usedNames.contains(name))
              (res |@| Error("already defined argument " + name, none, a.pos).failureNel[Unit]) { (_, _) => () }
            else
              res
            (res2, usedNames + name)
        }.getOrElse(p)
    }._1.map { _ => args }
  
  def transformTerm[T](term: Term[SimpleTerm[parser.Symbol, T]])(scope: Scope): ValidationNel[AbstractError, Term[SimpleTerm[Symbol, T]]] =
    term match {
      case App(fun, args, pos) =>
        (transformTerm(fun)(scope) |@| transformTermNel(args)(scope)) { App(_, _, pos) }
      case Simple(Let(binds, body, letInfo), pos) =>
        val newScope = scope.withLocalVariables(binds.map { _.name }.toSet)
        (transformBindNel(binds)(scope) |@| transformTerm(body)(newScope)) { case (bs, t) => Simple(Let(bs, t, letInfo), pos) }
      case Simple(Lambda(args, body, letInfo), pos) =>
        val newScope = scope.withLocalVariables(args.list.flatMap { _.name }.toSet)
        (transformArgNel(args) |@| transformTerm(body)(newScope)) { case (as, t) => Simple(Lambda(as, t, letInfo), pos) }
      case Simple(Var(sym), pos) =>
        transformSymbol(sym)(scope).map { s => Simple(Var(s), pos) }
      case Simple(Literal(value), pos) =>
        Simple(Literal(value), pos).successNel
    }
  
  private def getSymbol4[T](name: String, pos: Position)(scope: Scope)(prefix: String, contains: (NameTable, String) => Boolean, make: (ModuleSymbol, String) => T, importedSyms: Scope => Map[String, NonEmptyList[T]]) =
    scope.currentModuleSyms.foldLeft((Error("undefined " + prefix + " " + name, none, pos): AbstractError).failureNel[T]) {
      (res, moduleSym) =>
        scope.nameTree.getNameTable(moduleSym).map {
          nameTable => if(contains(nameTable, name)) make(moduleSym, name).successNel[AbstractError] else res
        }.getOrElse(res)
    }.orElse {
      importedSyms(scope).get(name).map { 
        case NonEmptyList(x) => x.successNel[AbstractError]
        case _               => Error("reference to " + name + " is ambiguous", none, pos).failureNel[T]
      }.getOrElse {
        Error("undefined " + prefix + " " + name, none, pos).failureNel[T]
      }
   }
    
  def getGlobalSymbol(name: String, pos: Position)(scope: Scope) =
    getSymbol4(name, pos)(scope)("variable", _.combNames.contains(_), _.globalSymbolFromName(_), _.importedCombSyms)
  
  def getModuleSymbol(name: String, pos: Position)(scope: Scope) =
    getSymbol4(name, pos)(scope)("module", _.moduleNames.contains(_), _ + _, _.importedModuleSyms)
  
  def transformSymbol(sym: parser.Symbol)(scope: Scope) =
    sym match {
      case parser.GlobalSymbol(names, pos) =>
        val combSym = GlobalSymbol(names)
        if(scope.nameTree.containsCombinator(GlobalSymbol(names)))
          combSym.successNel
        else
          Error("undefined global variable " + combSym, none, pos).failureNel
      case parser.NormalSymbol(NonEmptyList(name), pos) =>
        if(scope.localVarNames.contains(name))
          LocalSymbol(name).successNel
        else
          getGlobalSymbol(name, pos)(scope)
      case parser.NormalSymbol(names, pos) =>
        getModuleSymbol(names.head, pos)(scope).flatMap {
          moduleSym =>
            val combSym = moduleSym.globalSymbolFromNames(names)
            if(scope.nameTree.containsCombinator(GlobalSymbol(names)))
              combSym.successNel
            else
              Error("undefined global variable " + combSym, none, pos).failureNel
        }
    }
  
  def addDefToNameTreeS(definition: parser.Def)(currentModuleSym: ModuleSymbol)(nameTree: NameTree): (NameTree, ValidationNel[AbstractError, Unit]) =
    definition match {
      case parser.ImportDef(sym) =>
        (nameTree, ().successNel[AbstractError])
      case parser.CombinatorDef(sym, _, _) =>
        val sym2 = sym match {
          case parser.GlobalSymbol(names, _) => GlobalSymbol(names)
          case parser.NormalSymbol(names, _) => currentModuleSym.globalSymbolFromNames(names)
        }
        if(nameTree.containsCombinator(sym2))
          (nameTree, Error("already defined global variable " + sym2, none, sym.pos).failureNel)
        else
          (nameTree |+| NameTree.fromGlobalSymbol(sym2), ().successNel[AbstractError])
      case parser.ModuleDef(sym, defs) =>
        val sym2 = sym match {
          case parser.GlobalModuleSymbol(names, _) => ModuleSymbol(names)
          case parser.NormalModuleSymbol(names, _) => currentModuleSym ++ names.list
        }
        defs.foldLeft((nameTree, ().successNel[AbstractError])) {
          case ((nt, res), d) =>
            val (nt2, res2) = addDefToNameTreeS(d)(sym2)(nt)
            (nt2, res |+| res2)
        }
    }
  
  def addDefToNameTree(definition: parser.Def)(currentModuleSym: ModuleSymbol) =
    State(addDefToNameTreeS(definition)(currentModuleSym))
  
  def addParseTreeToNameTreeS(parseTree: parser.ParseTree)(nameTree: NameTree) =
    parseTree.defs.foldLeft((nameTree, ().successNel[AbstractError])) {
      case ((nt, res), d) =>
        val (nt2, res2) = addDefToNameTreeS(d)(ModuleSymbol.root)(nt)
        (nt2, res |+| res2)
    }
  
  def addParseTreeToNameTree(parseTree: parser.ParseTree) =
    State(addParseTreeToNameTreeS(parseTree))
}