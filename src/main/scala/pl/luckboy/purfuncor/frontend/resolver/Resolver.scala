package pl.luckboy.purfuncor.frontend.resolver
import scala.util.parsing.input.Position
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.util._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.frontend.Bind
import pl.luckboy.purfuncor.common.Result._

object Resolver
{
  def treeForFile[T, U, V, W](tree: Tree[GlobalSymbol, AbstractCombinator[Symbol, T, U], TreeInfo[V, W]], file: Option[java.io.File])(implicit showing: Showing[Tree[GlobalSymbol, AbstractCombinator[Symbol, T, U], TreeInfo[V, W]]]) =
    tree.copy(
        combs = tree.combs.mapValues { _.withFile(file) },
        treeInfo = tree.treeInfo.copy(
            typeTree = tree.treeInfo.typeTree.copy(
                combs = tree.treeInfo.typeTree.combs.mapValues { _.withFile(file) }
                )
            )
        )

  private def transformTermNel1[T, U](terms: NonEmptyList[T])(transform: T => ValidationNel[AbstractError, U]) =
    terms.tail.foldLeft(transform(terms.head).map { NonEmptyList(_) }) {
      (res, t) => (transform(t) |@| res)(_ <:: _)
    }.map { _.reverse }
    
  private def transformArgNel4[T, U](args: NonEmptyList[T])(prefix: String, getName: T => Option[String], getPos: T => Position, transform: T => ValidationNel[AbstractError, U]) =
    args.tail.foldLeft((transform(args.head).map { NonEmptyList(_) }, Set() ++ getName(args.head))) {
      case (p @ (res, usedNames), a) =>
        getName(a).map {
          name =>
            val res2 = if(usedNames.contains(name))
              (res |@| Error("already defined " + prefix + " " + name, none, getPos(a)).failureNel[Unit]) { (as, _) => as }
            else
              (transform(a) |@| res) { _ <:: _ }
            (res2, usedNames + name)
        }.getOrElse(((transform(a) |@| res) { _ <:: _ }, usedNames))
    }._1.map { _.reverse }
    
  def transformTermNel[T, U](terms: NonEmptyList[Term[SimpleTerm[parser.Symbol, T, TypeSimpleTerm[parser.Symbol, U]]]])(scope: Scope) =
    transformTermNel1(terms)(transformTerm(_)(scope))
  
  def transformBind[T, U](bind: Bind[parser.Symbol, T, TypeSimpleTerm[parser.Symbol, U]])(scope: Scope) =
    bind match {
      case Bind(name, body, pos) => transformTerm(body)(scope).map { Bind(name, _, pos) }
    }

  def transformBindNel[T, U](binds: NonEmptyList[Bind[parser.Symbol, T, TypeSimpleTerm[parser.Symbol, U]]])(scope: Scope) =
    binds.tail.foldLeft((transformBind(binds.head)(scope).map { NonEmptyList(_) }, Set(binds.head.name))) {
      case ((res, usedNames), b) => 
        val res2 = if(usedNames.contains(b.name))
          (res |@| Error("already defined local variable " + b.name, none, b.pos).failureNel[Unit]) { (bs, _) => bs }
        else 
          res
        ((transformBind(b)(scope) |@| res2)(_ <:: _), usedNames + b.name)
    }._1.map { _.reverse }
    
  def transformArgNel[T](args: NonEmptyList[Arg[TypeSimpleTerm[parser.Symbol, T]]])(scope: Scope) =
    transformArgNel4(args)("argument", _.name, _.pos, transformArg(_)(scope))

  def transformArgs[T](args: List[Arg[TypeSimpleTerm[parser.Symbol, T]]])(scope: Scope) =
    args.toNel.map { transformArgNel(_)(scope).map { _.list } }.getOrElse(Nil.successNel)

  def transformArg[T](arg: Arg[TypeSimpleTerm[parser.Symbol, T]])(scope: Scope): ValidationNel[AbstractError, Arg[TypeSimpleTerm[Symbol, T]]] =
    arg.typ.map {
      transformTypeTerm(_)(scope.copy(localVarNames = Set())).map { tt => Arg(arg.name, some(tt), arg.pos) }
    }.getOrElse(Arg(arg.name, none, arg.pos).successNel)

  def transformCase[T, U](cas: Case[parser.Symbol, T, TypeSimpleTerm[parser.Symbol, U]])(scope: Scope) =
    (transformTypeTerm(cas.typ)(scope.copy(localVarNames = Set())) |@| transformTerm(cas.body)(scope.withLocalVars(cas.name.toSet))) {
      Case(cas.name, _, _, cas.lambdaInfo)
    }
    
  def transformCaseNel[T, U](cases: NonEmptyList[Case[parser.Symbol, T, TypeSimpleTerm[parser.Symbol, U]]])(scope: Scope) =
    transformTermNel1(cases)(transformCase(_)(scope))
    
  def transformTerm[T, U](term: Term[SimpleTerm[parser.Symbol, T, TypeSimpleTerm[parser.Symbol, U]]])(scope: Scope): ValidationNel[AbstractError, Term[SimpleTerm[Symbol, T, TypeSimpleTerm[Symbol, U]]]] =
    term match {
      case App(fun, args, pos) =>
        (transformTerm(fun)(scope) |@| transformTermNel(args)(scope)) { App(_, _, pos) }
      case Simple(Let(binds, body, lambdaInfo), pos) =>
        val newScope = scope.withLocalVars(binds.map { _.name }.toSet)
        (transformBindNel(binds)(scope) |@| transformTerm(body)(newScope)) { (bs, t) => Simple(Let(bs, t, lambdaInfo), pos) }
      case Simple(Lambda(args, body, lambdaInfo), pos) =>
        val newScope = scope.withLocalVars(args.list.flatMap { _.name }.toSet)
        (transformArgNel(args)(scope) |@| transformTerm(body)(newScope)) { (as, t) => Simple(Lambda(as, t, lambdaInfo), pos) }
      case Simple(Var(sym), pos) =>
        transformSymbol(sym)(scope).map { s => Simple(Var(s), pos) }
      case Simple(Literal(value), pos) =>
        Simple(Literal(value), pos).successNel
      case Simple(TypedTerm(term, typ), pos) =>
        (transformTerm(term)(scope) |@| transformTypeTerm(typ)(scope.copy(localVarNames = Set()))) { (t, tt) => Simple(TypedTerm(t, tt), pos) }
      case Simple(Construct(n, lambdaInfo), pos) =>
        Simple(Construct(n, lambdaInfo), pos).successNel
      case Simple(Select(term, cases), pos) =>
        (transformTerm(term)(scope) |@| transformCaseNel(cases)(scope)) { (t, cs) => Simple(Select(t, cs), pos) }
      case Simple(Extract(term, args, body, lambdaInfo), pos) =>
        val newScope = scope.withLocalVars(args.list.flatMap { _.name }.toSet)
        (transformTerm(term)(scope) |@| transformArgNel(args)(scope) |@| transformTerm(body)(newScope)) {
          (t1, as, t2) => Simple(Extract(t1, as, t2, lambdaInfo), pos)
        }
    }
  
  def transformTypeTermNel[T](terms: NonEmptyList[Term[TypeSimpleTerm[parser.Symbol, T]]])(scope: Scope) =
    transformTermNel1(terms)(transformTypeTerm(_)(scope))
    
  def transformTypeArgNel(args: NonEmptyList[TypeArg]) =
    transformArgNel4(args)("type argument", _.name, _.pos, _.successNel)
    
  def transformTypeArgs(args: List[TypeArg]) =
    args.toNel.map { transformTypeArgNel(_).map { _.list } }.getOrElse(Nil.successNel)
    
  def transformTypeTermOption[T](term: Option[Term[TypeSimpleTerm[parser.Symbol, T]]])(scope: Scope) =
    term.map { transformTypeTerm(_)(scope).map(some) }.getOrElse(none.successNel)

  def transformTypeTerm[T](term: Term[TypeSimpleTerm[parser.Symbol, T]])(scope: Scope): ValidationNel[AbstractError, Term[TypeSimpleTerm[Symbol, T]]] =
    term match {
      case App(fun, args, pos) =>
        (transformTypeTerm(fun)(scope) |@| transformTypeTermNel(args)(scope)) { App(_, _, pos)}
      case Simple(TypeLambda(args, body, lambdaInfo), pos) =>
        val newScope = scope.withLocalVars(args.list.flatMap { _.name }.toSet)
        (transformTypeArgNel(args) |@| transformTypeTerm(body)(newScope)) { case (as, t) => Simple(TypeLambda(as, t, lambdaInfo), pos) }
      case Simple(TypeVar(sym), pos) =>
        transformTypeSymbol(sym)(scope).map { s => Simple(TypeVar(s), pos) }
      case Simple(TypeLiteral(value), pos) =>
        Simple(TypeLiteral(value), pos).successNel
      case Simple(KindedTypeTerm(term, kind), pos) =>
        transformTypeTerm(term)(scope).map { t => Simple(KindedTypeTerm(t, kind), pos) }
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
  
  def getTypeGlobalSymbol(name: String, pos: Position)(scope: Scope) =
    getSymbol4(name, pos)(scope)("type variable", _.typeCombNames.contains(_), _.globalSymbolFromName(_), _.importedTypeCombSyms)

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
  
  def transformTypeSymbol(sym: parser.Symbol)(scope: Scope) =
    sym match {
      case parser.GlobalSymbol(names, pos) =>
        val typeCombSym = GlobalSymbol(names)
        if(scope.nameTree.containsTypeComb(typeCombSym))
          typeCombSym.successNel
        else
          Error("undefined global type variable " + typeCombSym, none, pos).failureNel
      case parser.NormalSymbol(NonEmptyList(name), pos) =>
        if(scope.localVarNames.contains(name))
          LocalSymbol(name).successNel
        else
          getTypeGlobalSymbol(name, pos)(scope)
      case parser.NormalSymbol(names, pos) =>
        getModuleSymbol(names.head, pos)(scope).flatMap {
          moduleSym =>
            names.tail.toNel.map {
              tail =>
                val typeCombSym = moduleSym.globalSymbolFromNames(tail)
                if(scope.nameTree.containsTypeComb(typeCombSym))
                  typeCombSym.successNel
                else
                  Error("undefined global type variable " + typeCombSym, none, pos).failureNel
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
      case parser.CombinatorDef(sym, _, _, _) =>
        val sym2 = transformGlobalSymbol(sym)(currentModuleSym)
        if(nameTree.containsComb(sym2))
          (nameTree, Error("already defined global variable " + sym2, none, sym.pos).failureNel)
        else
          (nameTree |+| NameTree.fromGlobalSymbol(sym2), ().successNel[AbstractError])
      case parser.TypeCombinatorDef(sym, _, _, _) =>
        val sym2 = transformGlobalSymbol(sym)(currentModuleSym)
        if(nameTree.containsTypeComb(sym2))
          (nameTree, Error("already defined global type variable " + sym2, none, sym.pos).failureNel)
        else
          (nameTree |+| NameTree.fromTypeGlobalSymbol(sym2), ().successNel[AbstractError])
      case parser.UnittypeCombinatorDef(_, sym, _) =>
        val sym2 = transformGlobalSymbol(sym)(currentModuleSym)
        if(nameTree.containsTypeComb(sym2))
          (nameTree, Error("already defined global type variable " + sym2, none, sym.pos).failureNel)
        else
          (nameTree |+| NameTree.fromTypeGlobalSymbol(sym2), ().successNel[AbstractError])
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
    
  def transformDefsS[T](defs: List[parser.Def])(scope: Scope)(tree: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], TreeInfo[parser.TypeLambdaInfo, T]]): (Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], TreeInfo[parser.TypeLambdaInfo, T]], ValidationNel[AbstractError, Unit]) =
    defs.foldLeft(((tree, ().successNel[AbstractError]), scope)) {
      case ((p @ (tree2, res), scope), d) =>
        d match {
          case parser.ImportDef(sym) =>
            transformImportModuleSymbol(sym)(scope).map {
              sym2 =>
                scope.nameTree.getNameTable(sym2).map {
                  nt => 
                    val combSyms = nt.combNames.map { name => (name, sym2.globalSymbolFromName(name)) }.toMap
                    val typeCombSyms = nt.typeCombNames.map { name => (name, sym2.globalSymbolFromName(name)) }.toMap
                    val moduleSyms = nt.moduleNames.map { name => (name, sym2 + name) }.toMap
                    (p, scope.withImportedCombs(combSyms).withImportedCombs(typeCombSyms).withImportedModules(moduleSyms))
                }.getOrElse {
                  ((tree2, res |+| Error("undefined module " + sym2, none, sym.pos).failureNel), scope)
                }
            } match {
              case Success(pp) => pp
              case res2        => ((tree2, res |+| res2.map { _ => () }), scope)
            }
          case parser.CombinatorDef(sym, typ, args, body) =>
            val sym2 = transformGlobalSymbol(sym)(scope.currentModuleSyms.head)
            if(scope.nameTree.containsComb(sym2)) {
              val newScope = scope.withLocalVars(args.flatMap { _.name }.toSet)
              val res2 = (transformTypeTermOption(typ)(scope) |@| transformArgs(args)(scope) |@| transformTerm(body)(newScope)) { (tt, as, t) => (tt, as, t) }
              res2 match {
                case Success((tt, as, t)) => 
                  ((tree2.copy(combs = tree2.combs + (sym2 -> Combinator(tt, as, t, parser.LambdaInfo, none))), (res |@| res2) { (u, _) => u }), scope)
                case Failure(_) =>
                  ((tree2, (res |@| res2) { (u, _) => u }), scope)
              }
            } else
              ((tree, res |+| FatalError("name tree doesn't contain combinator", none, sym.pos).failureNel[Unit]), scope)
          case parser.TypeCombinatorDef(sym, kind, args, body) =>
            val sym2 = transformGlobalSymbol(sym)(scope.currentModuleSyms.head)
            if(scope.nameTree.containsTypeComb(sym2)) {
              val newScope = scope.withLocalVars(args.flatMap { _.name }.toSet)
              val res2 = (transformTypeArgs(args) |@| transformTypeTerm(body)(newScope)) { (as, t) => (as, t) }
              res2 match {
                case Success((as, t)) => 
                  val treeInfo2 = tree2.treeInfo
                  val typeTree2 = treeInfo2.typeTree
                  ((tree2.copy(treeInfo = treeInfo2.copy(typeTree = typeTree2.copy(combs = typeTree2.combs + (sym2 -> TypeCombinator(kind, as, t, parser.TypeLambdaInfo, none))))), (res |@| res2) { (u, _) => u }), scope)
                case Failure(_) =>
                  ((tree2, (res |@| res2) { (u, _) => u }), scope)
              } 
            } else
              ((tree, res |+| FatalError("name tree doesn't contain type combinator", none, sym.pos).failureNel[Unit]), scope)
          case parser.UnittypeCombinatorDef(n, sym, kind) =>
            val sym2 = transformGlobalSymbol(sym)(scope.currentModuleSyms.head)
            if(scope.nameTree.containsTypeComb(sym2)) {
              val treeInfo2 = tree2.treeInfo
              val typeTree2 = treeInfo2.typeTree
              ((tree2.copy(treeInfo = treeInfo2.copy(typeTree = typeTree2.copy(combs = typeTree2.combs + (sym2 -> UnittypeCombinator(n, kind, none))))), res), scope)
            } else
              ((tree, res |+| FatalError("name tree doesn't contain type combinator", none, sym.pos).failureNel[Unit]), scope)
          case parser.ModuleDef(sym, defs2) =>
            val sym2 = transformModuleSymbol(sym)(scope.currentModuleSyms.head)
            val (newTree2, res2) = transformDefsS(defs2)(scope.withCurrentModule(sym2))(tree2)
            ((newTree2, res |+| res2), scope)
        }
    }._1
    
  def transformDefs[T](defs: List[parser.Def])(scope: Scope) =
    State(transformDefsS[T](defs)(scope))
    
  def transformParseTreeS[T](parseTree: parser.ParseTree)(nameTree: NameTree)(tree: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], TreeInfo[parser.TypeLambdaInfo, T]]) =
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
    val (newTree, res2) = parseTrees.foldLeft((Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], TreeInfo[parser.TypeLambdaInfo, TypeTreeInfo]](Map(), TreeInfo(Tree[GlobalSymbol, AbstractTypeCombinator[Symbol, parser.TypeLambdaInfo], TypeTreeInfo](Map(), TypeTreeInfo))), res1)) {
      case (p @ (t, res), (file, pt)) => 
        val (newTree, newRes) = transformParseTree[TypeTreeInfo](pt)(newNameTree).map {
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
    
  def transformFile(file: java.io.File)(nameTree: NameTree) =
    for {
      parseTree <- parser.Parser.parseFile(file)
      tree <- transform(List(some(file) -> parseTree))(nameTree)      
    } yield tree
    
  def transformFiles(files: List[java.io.File])(nameTree: NameTree) = {
    val res1 = files.foldLeft(List[(Option[java.io.File], parser.ParseTree)]().successNel[AbstractError]) {
      case (res, file) => (res |@| parser.Parser.parseFile(file)) { (pts, pt) => pts :+ (some(file) -> pt) }
    }
    res1.flatMap { transform(_)(nameTree) }
  }
    
  def transformTermString(s: String)(scope: Scope) =
    for {
      term <- parser.Parser.parseTermString(s)
      term2 <- transformTerm(term)(scope)
    } yield term2
    
  def transformTypeTermString(s: String)(scope: Scope) =
    for {
      typeTerm <- parser.Parser.parseTypeTermString(s)
      typeTerm2 <- transformTypeTerm(typeTerm)(scope)
    } yield typeTerm2
}