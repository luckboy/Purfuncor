package pl.luckboy.purfuncor.frontend.resolver
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.frontend.Bind
import pl.luckboy.purfuncor.common.Result._

object Resolver
{
  def treeForFile[T, U](tree: Tree[GlobalSymbol, Combinator[Symbol, T], U], file: Option[java.io.File]) =
    tree.copy(tree.combs.mapValues { _.copy(file = file) })
        
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
    
  def transformArgs(args: List[Arg]) =
    args.toNel.map { transformArgNel(_).map { _.list } }.getOrElse(Nil.successNel)
  
  def transformTerm[T](term: Term[SimpleTerm[parser.Symbol, T]])(scope: Scope): ValidationNel[AbstractError, Term[SimpleTerm[Symbol, T]]] =
    term match {
      case App(fun, args, pos) =>
        (transformTerm(fun)(scope) |@| transformTermNel(args)(scope)) { App(_, _, pos) }
      case Simple(Let(binds, body, letInfo), pos) =>
        val newScope = scope.withLocalVars(binds.map { _.name }.toSet)
        (transformBindNel(binds)(scope) |@| transformTerm(body)(newScope)) { case (bs, t) => Simple(Let(bs, t, letInfo), pos) }
      case Simple(Lambda(args, body, letInfo), pos) =>
        val newScope = scope.withLocalVars(args.list.flatMap { _.name }.toSet)
        (transformArgNel(args) |@| transformTerm(body)(newScope)) { case (as, t) => Simple(Lambda(as, t, letInfo), pos) }
      case Simple(Var(sym), pos) =>
        transformSymbol(sym)(scope).map { s => Simple(Var(s), pos) }
      case Simple(Literal(value), pos) =>
        Simple(Literal(value), pos).successNel
    }
  
  private def getSymbol4[T](name: String, pos: Position)(scope: Scope)(prefix: String, contains: (NameTable, String) => Boolean, make: (ModuleSymbol, String) => T, importedSyms: Scope => Map[String, Set[T]]) = {
    val undefinedSymErrRes = (Error("undefined " + prefix + " " + name, none, pos): AbstractError).failureNel[T]
    scope.currentModuleSyms.foldLeft(undefinedSymErrRes) {
      (res, moduleSym) =>
        res.orElse {
          scope.nameTree.getNameTable(moduleSym).map {
            nameTable => if(contains(nameTable, name)) make(moduleSym, name).successNel[AbstractError] else res
          }.getOrElse(res)
        }
    }.orElse {
      importedSyms(scope).get(name).map { 
        syms =>
          if(syms.size <= 1)
            syms.headOption.map { _.successNel[AbstractError] }.getOrElse(undefinedSymErrRes)
          else
            Error("reference to " + name + " is ambiguous", none, pos).failureNel[T]
      }.getOrElse(undefinedSymErrRes)
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
        if(scope.nameTree.containsComb(combSym))
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
            names.tail.toNel.map {
              tail =>
                val combSym = moduleSym.globalSymbolFromNames(tail)
                if(scope.nameTree.containsComb(combSym))
                  combSym.successNel
                else
                  Error("undefined global variable " + combSym, none, pos).failureNel
            }.getOrElse(FatalError("tail of list is empty", none, pos).failureNel)
        }
    }
  
  def transformGlobalSymbol(sym: parser.Symbol)(currentModuleSym: ModuleSymbol) =
    sym match {
      case parser.GlobalSymbol(names, _) => GlobalSymbol(names)
      case parser.NormalSymbol(names, _) => currentModuleSym.globalSymbolFromNames(names)
    }
  
  def transformModuleSymbol(sym: parser.ModuleSymbol)(currentModuleSym: ModuleSymbol) =
    sym match {
      case parser.GlobalModuleSymbol(names, _) => ModuleSymbol(names)
      case parser.NormalModuleSymbol(names, _) => currentModuleSym ++ names.list
    }
  
  def addDefToNameTreeS(definition: parser.Def)(currentModuleSym: ModuleSymbol)(nameTree: NameTree): (NameTree, ValidationNel[AbstractError, Unit]) =
    definition match {
      case parser.ImportDef(sym) =>
        (nameTree, ().successNel[AbstractError])
      case parser.CombinatorDef(sym, _, _) =>
        val sym2 = transformGlobalSymbol(sym)(currentModuleSym)
        if(nameTree.containsComb(sym2))
          (nameTree, Error("already defined global variable " + sym2, none, sym.pos).failureNel)
        else {
          (nameTree |+| NameTree.fromGlobalSymbol(sym2), ().successNel[AbstractError])
        }
      case parser.ModuleDef(sym, defs) =>
        defs.foldLeft((nameTree, ().successNel[AbstractError])) {
          case ((nt, res), d) =>
            val (nt2, res2) = addDefToNameTreeS(d)(transformModuleSymbol(sym)(currentModuleSym))(nt)
            (nt2, res |+| res2)
        }
    }
  
  def addDefToNameTree(definition: parser.Def)(currentModuleSym: ModuleSymbol) =
    State(addDefToNameTreeS(definition)(currentModuleSym))
  
  def nameTreeFromParseTreeS(parseTree: parser.ParseTree)(nameTree: NameTree) =
    parseTree.defs.foldLeft((nameTree, ().successNel[AbstractError])) {
      case ((nt, res), d) =>
        val (nt2, res2) = addDefToNameTreeS(d)(ModuleSymbol.root)(nt)
        (nt2, res |+| res2)
    }
  
  def nameTreeFromParseTree(parseTree: parser.ParseTree) =
    State(nameTreeFromParseTreeS(parseTree))
    
  def transformImportModuleSymbol(sym: parser.ModuleSymbol)(scope: Scope) =
    sym match {
      case parser.GlobalModuleSymbol(names, pos) =>
        val moduleSym = ModuleSymbol(names)
        if(scope.nameTree.containsModule(moduleSym))
          moduleSym.successNel
        else
          Error("undefined module " + moduleSym, none, pos).failureNel
      case parser.NormalModuleSymbol(names, pos) =>
        getModuleSymbol(names.head, pos)(scope).flatMap {
          moduleSym =>
            val moduleSym2 = moduleSym ++ names.tail
            if(scope.nameTree.containsModule(moduleSym2))
              moduleSym2.successNel
            else
              Error("undefined module " + moduleSym2, none, pos).failureNel
        }
    }
    
  def transformDefsS[T](defs: List[parser.Def])(scope: Scope)(tree: Tree[GlobalSymbol, Combinator[Symbol, parser.LetInfo], T]): (Tree[GlobalSymbol, Combinator[Symbol, parser.LetInfo], T], ValidationNel[AbstractError, Unit]) =
    defs.foldLeft(((tree, ().successNel[AbstractError]), scope)) {
      case ((p @ (tree2, res), scope), d) =>
        d match {
          case parser.ImportDef(sym) =>
            transformImportModuleSymbol(sym)(scope).map {
              sym2 =>
                scope.nameTree.getNameTable(sym2).map {
                  nt => 
                    val combSyms = nt.combNames.map { name => (name, sym2.globalSymbolFromName(name)) }.toMap
                    val moduleSyms = nt.moduleNames.map { name => (name, sym2 + name) }.toMap
                    (p, scope.withImportedCombs(combSyms).withImportedModules(moduleSyms))
                }.getOrElse {
                  ((tree2, res |+| Error("undefined module " + sym2, none, sym.pos).failureNel), scope)
                }
            } match {
              case Success(pp) => pp
              case res2        => ((tree2, res |+| res2.map { _ => () }), scope)
            }
          case parser.CombinatorDef(sym, args, body) =>
            val sym2 = transformGlobalSymbol(sym)(scope.currentModuleSyms.head)
            if(scope.nameTree.containsComb(sym2)) {
              val newScope = scope.withLocalVars(args.flatMap { _.name }.toSet)
              val res2 = transformTerm(body)(newScope)
              res2 match {
                case Success(t) => 
                  ((tree2.copy(combs = tree2.combs + (sym2 -> Combinator(args, t, parser.LetInfo, none))), (res |@| res2) { (u, _) => u }), scope)
                case Failure(_) =>
                  ((tree2, (res |@| res2) { (u, _) => u }), scope)
              }
            } else
              ((tree, res |+| FatalError("name tree doesn't contain combinator", none, sym.pos).failureNel[Unit]), scope)
          case parser.ModuleDef(sym, defs2) =>
            val sym2 = transformModuleSymbol(sym)(scope.currentModuleSyms.head)
            val (newTree2, res2) = transformDefsS(defs2)(scope.withCurrentModule(sym2))(tree2)
            ((newTree2, res |+| res2), scope)
        }
    }._1
    
  def transformDefs[T](defs: List[parser.Def])(scope: Scope) =
    State(transformDefsS[T](defs)(scope))
    
  def transformParseTreeS[T](parseTree: parser.ParseTree)(nameTree: NameTree)(tree: Tree[GlobalSymbol, Combinator[Symbol, parser.LetInfo], T]) =
    transformDefsS[T](parseTree.defs)(Scope.fromNameTree(nameTree))(tree)
    
  def transformParseTree[T](parseTree: parser.ParseTree)(nameTree: NameTree) =
    State(transformParseTreeS[T](parseTree)(nameTree))

  def transform(parseTrees: List[(Option[java.io.File], parser.ParseTree)])(nameTree: NameTree) = {
    val (newNameTree, res1) = parseTrees.foldLeft((nameTree, ().successNel[AbstractError])) {
      case (p @ (nt, res), (file, pt)) =>
        nameTreeFromParseTree(pt).map { 
          res2 => res |+| resultForFile(res2, file)
        }.run(nt)
    }
    val (newTree, res2) = parseTrees.foldLeft((Tree[GlobalSymbol, Combinator[Symbol, parser.LetInfo], TreeInfo](Map(), TreeInfo), res1)) {
      case (p @ (t, res), (file, pt)) => 
        val (newTree, newRes) = transformParseTree[TreeInfo](pt)(newNameTree).map {
          res2 => res |+| resultForFile(res2, file)
        }.run(t)
        (treeForFile(newTree, file), newRes)
    }
    res2.map { _ => newTree }
  }
  
  def transformString(s: String)(nameTree: NameTree) =
    for {
      parseTree <- parser.Parser.parseString(s)
      tree <- transform(List(none -> parseTree))(nameTree)
    } yield tree
}