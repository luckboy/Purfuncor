package pl.luckboy.purfuncor.frontend.lmbdindexer
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.util._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.frontend.Bind

object LambdaIndexer
{
  private def transformTermNelFromIndexS1[T, U](terms: NonEmptyList[T])(idx: Int)(transform: (T, Int) => (Int, U)) = {
    val (idx2, term2) = transform(terms.head, idx)
    val (idx3, terms3) = terms.tail.foldLeft((idx2, NonEmptyList(term2))) {
      case ((newIdx, terms2), term) =>
        val (newIdx2, term2) = transform(term, newIdx)
        (newIdx2, term2 <:: terms2)
    }
    (idx3, terms3.reverse)
  }

  def transformTermNelFromIndexS[T, U, V, W](terms: NonEmptyList[Term[SimpleTerm[T, U, TypeSimpleTerm[V, W]]]])(idx: Int) =
    transformTermNelFromIndexS1(terms)(idx)(transformTermFromIndexS(_)(_))
  
  def transformCaseFromIndexS[T, U, V, W](cas: Case[T, U, TypeSimpleTerm[V, W]])(idx: Int): (Int, Case[T, LambdaInfo[U], TypeSimpleTerm[V, TypeLambdaInfo[W]]]) =
    cas match {
      case Case(name, typ, body, lambdaInfo) =>
        val (idx2, body2) = transformTermFromIndexS(body)(idx + 1)
        (idx2, Case(name, transformTypeTermFromIndex(typ).run(0)._2, body2, LambdaInfo(lambdaInfo, idx)))
    }
  
  def transformCaseNelFromIndexS[T, U, V, W](cases: NonEmptyList[Case[T, U, TypeSimpleTerm[V, W]]])(idx: Int) =
    transformTermNelFromIndexS1(cases)(idx)(transformCaseFromIndexS(_)(_))
    
  def transformTermFromIndexS[T, U, V, W](term: Term[SimpleTerm[T, U, TypeSimpleTerm[V, W]]])(idx: Int): (Int, Term[SimpleTerm[T, LambdaInfo[U], TypeSimpleTerm[V, TypeLambdaInfo[W]]]]) =
    term match {
      case App(fun, args, pos) => 
        val (idx2, fun2) = transformTermFromIndexS(fun)(idx)
        val (idx3, args2) = transformTermNelFromIndexS(args)(idx2)
        (idx3, App(fun2, args2, pos))
      case Simple(Let(binds, body, lambdaInfo), pos) =>
        val (idx2, bindTerms2) = transformTermNelFromIndexS(binds.map { _.body })(idx + 1)
        val binds2 = binds.zip(bindTerms2).map { case (Bind(name, _, bindPos), bt2) => Bind(name, bt2, bindPos) }
        val (idx3, body2) = transformTermFromIndexS(body)(idx2)
        (idx3, Simple(Let(binds2, body2, LambdaInfo(lambdaInfo, idx)), pos))
      case Simple(Lambda(args, body, lambdaInfo), pos) =>
        val (idx2, body2) = transformTermFromIndexS(body)(idx + 1)
        val args2 = args.map { case Arg(name, typ, argPos) => Arg(name, typ.map { transformTypeTermFromIndex(_).run(0)._2 }, pos) }
        (idx2, Simple(Lambda(args2, body2, LambdaInfo(lambdaInfo, idx)), pos))
      case Simple(Var(loc, lambdaInfo), pos) =>
        (idx + 1, Simple(Var(loc, LambdaInfo(lambdaInfo, idx)), pos))
      case Simple(Literal(value), pos) =>
        (idx, Simple(Literal(value), pos))
      case Simple(TypedTerm(term, typ), pos) =>
        val (idx2, term2) = transformTermFromIndexS(term)(idx)
        (idx2, Simple(TypedTerm(term2, transformTypeTermFromIndex(typ).run(0)._2), pos))
      case Simple(Construct(n, lambdaInfo), pos) =>
        (idx + 1, Simple(Construct(n, LambdaInfo(lambdaInfo, idx)), pos))
      case Simple(Select(term, cases, lambdaInfo), pos) =>
        val (idx2, term2) = transformTermFromIndexS(term)(idx + 1)
        val (idx3, cases2) = transformCaseNelFromIndexS(cases)(idx2)
        (idx3, Simple(Select(term2, cases2, LambdaInfo(lambdaInfo, idx)), pos))
      case Simple(Extract(term, args, body, lambdaInfo), pos) =>
        val (idx2, term2) = transformTermFromIndexS(term)(idx + 1)
        val args2 = args.map { case Arg(name, typ, argPos) => Arg(name, typ.map { transformTypeTermFromIndex(_).run(0)._2 }, pos) }
        val (idx3, body2) = transformTermFromIndexS(body)(idx2)
        (idx3, Simple(Extract(term2, args2, body2, LambdaInfo(lambdaInfo, idx)), pos))
    }
  
  def transformTermFromIndex[T, U, V, W](term: Term[SimpleTerm[T, U, TypeSimpleTerm[V, W]]]) =
    State(transformTermFromIndexS[T, U, V, W](term))
  
  def transformTerm[T, U, V, W](term: Term[SimpleTerm[T, U, TypeSimpleTerm[V, W]]]) =
    transformTermFromIndex(term).run(0)._2.successNel[AbstractError]
  
  def transformTypeTermNelFromIndexS[T, U](terms: NonEmptyList[Term[TypeSimpleTerm[T, U]]])(idx: Int) =
    transformTermNelFromIndexS1(terms)(idx)(transformTypeTermFromIndexS(_)(_))
    
  def transformTypeTermNelFromIndex[T, U](terms: NonEmptyList[Term[TypeSimpleTerm[T, U]]]) =
    State(transformTypeTermNelFromIndexS[T, U](terms))

  def transformTypeTermFromIndexS[T, U](term: Term[TypeSimpleTerm[T, U]])(idx: Int): (Int, Term[TypeSimpleTerm[T, TypeLambdaInfo[U]]]) =
    term match {
      case App(fun, args, pos) =>
        val (idx2, fun2) = transformTypeTermFromIndexS(fun)(idx)
        val (idx3, args2) = transformTypeTermNelFromIndexS(args)(idx2)
        (idx3, App(fun2, args2, pos))
      case Simple(TypeLambda(args, body, lambdaInfo), pos) =>
        val (idx2, body2) = transformTypeTermFromIndexS(body)(idx + 1)
        (idx2, Simple(TypeLambda(args, body2, TypeLambdaInfo(lambdaInfo, idx)), pos))
      case Simple(TypeVar(loc), pos) =>
        (idx, Simple(TypeVar(loc), pos))
      case Simple(TypeLiteral(value), pos) =>
        (idx, Simple(TypeLiteral(value), pos))
      case Simple(KindedTypeTerm(term, kind), pos) =>
        val (idx2, term2) = transformTypeTermFromIndexS(term)(idx + 1)
        (idx2, Simple(KindedTypeTerm(term2, kind), pos))
    }

  def transformTypeTermFromIndex[T, U](term: Term[TypeSimpleTerm[T, U]]) =
    State(transformTypeTermFromIndexS[T, U](term))

  def transformTypeTerm[T, U](term: Term[TypeSimpleTerm[T, U]]) =
    transformTypeTermFromIndex(term).run(0)._2.successNel[AbstractError]

  def transformTree[T, U, V, W, X, Y](tree: Tree[T, AbstractCombinator[U, V, TypeSimpleTerm[W, X]], Y]) = {
    val combs2 = tree.combs.mapValues {
      case Combinator(typ, args, body, lambdaInfo, file) =>
        val typ2 = typ.map { transformTypeTermFromIndex(_).run(0)._2 }
        val args2 = args.map { case Arg(name, typ, pos) => Arg(name, typ.map { transformTypeTermFromIndex(_).run(0)._2 }, pos) }
        Combinator(typ2, args2, transformTermFromIndex(body).run(1)._2, LambdaInfo(lambdaInfo, 0), file): AbstractCombinator[U, LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X]]]
      case PolyCombinator(typ, file)                     =>
        val typ2 = typ.map { transformTypeTermFromIndex(_).run(0)._2 }
        PolyCombinator(typ2, file)
    }
    Tree(combs = combs2, treeInfo = tree.treeInfo).successNel[AbstractError]
  }
  
  def transformTypeTree[T, U, V, W](tree: Tree[T, AbstractTypeCombinator[U, V], W]) = {
    val combs2 = tree.combs.mapValues {
      case TypeCombinator(kind, args, body, lambdaInfo, file) =>
        TypeCombinator(kind, args, transformTypeTermFromIndex(body).run(1)._2, TypeLambdaInfo(lambdaInfo, 0), file): AbstractTypeCombinator[U, TypeLambdaInfo[V]]
      case UnittypeCombinator(n, kind, file)         =>
        UnittypeCombinator(n, kind, file): AbstractTypeCombinator[U, TypeLambdaInfo[V]]
    }
    Tree(combs = combs2, treeInfo = tree.treeInfo).successNel[AbstractError]
  }

  def transformSelectConstructInstanceFromIndexS[T, U](instance: SelectConstructInstance[T, U])(idx: Int) =
    (for {
      supertype <- transformTypeTermFromIndex(instance.supertype)
      types <- transformTypeTermNelFromIndex(instance.types)
    } yield SelectConstructInstance(supertype, types, instance.file)).run(idx)
    
  def transformSelectConstructInstanceFromIndex[T, U](instance: SelectConstructInstance[T, U]) =
    State(transformSelectConstructInstanceFromIndexS[T, U](instance))
  
  def transformSelectConstructInstances[T, U](instances: List[SelectConstructInstance[T, U]]) =
    instances.map { transformSelectConstructInstanceFromIndex(_).run(0)._2 }.successNel[AbstractError]
  
  def transform[T, U, V, W, X[_, _], Y, Z](tree: Tree[T, AbstractCombinator[U, V, TypeSimpleTerm[W, Y]], X[Y, Z]])(implicit treeInfoTransformer: TreeInfoTransformer[X]) =
    for {
      tree2 <- transformTree(tree)
      treeInfo2 <- treeInfoTransformer.transformTreeInfo(tree.treeInfo)
    } yield Tree(combs = tree2.combs, treeInfo = treeInfo2)
    
  def transformString(s: String)(nameTree: resolver.NameTree) = 
    for {
      tree <- resolver.Resolver.transformString(s)(nameTree)
      tree2 <- transform(tree)
    } yield tree2
}