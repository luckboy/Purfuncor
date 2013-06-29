package pl.luckboy.purfuncor.frontend.lmbdindxer
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.util._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
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

  def transformTermNelFromIndexS[T, U](terms: NonEmptyList[Term[SimpleTerm[T, parser.LambdaInfo, TypeSimpleTerm[U, parser.TypeLambdaInfo]]]])(idx: Int) =
    transformTermNelFromIndexS1(terms)(idx)(transformTermFromIndexS(_)(_))
  
  def transformTermFromIndexS[T, U](term: Term[SimpleTerm[T, parser.LambdaInfo, TypeSimpleTerm[U, parser.TypeLambdaInfo]]])(idx: Int): (Int, Term[SimpleTerm[T, LambdaInfo, TypeSimpleTerm[U, TypeLambdaInfo]]]) =
    term match {
      case App(fun, args, pos) => 
        val (idx2, fun2) = transformTermFromIndexS(fun)(idx)
        val (idx3, args2) = transformTermNelFromIndexS(args)(idx2)
        (idx3, App(fun2, args2, pos))
      case Simple(Let(binds, body, _), pos) =>
        val (idx2, bindTerms2) = transformTermNelFromIndexS(binds.map { _.body })(idx + 1)
        val binds2 = binds.zip(bindTerms2).map { case (Bind(name, _, bindPos), bt2) => Bind(name, bt2, bindPos) }
        val (idx3, body2) = transformTermFromIndexS(body)(idx2)
        (idx3, Simple(Let(binds2, body2, LambdaInfo(idx)), pos))
      case Simple(Lambda(args, body, _), pos) =>
        val (idx2, body2) = transformTermFromIndexS(body)(idx + 1)
        val args2 = args.map { case Arg(name, typ, argPos) => Arg(name, typ.map { transformTypeTermFromIndex(_).run(0)._2 }, pos) }
        (idx2, Simple(Lambda(args2, body2, LambdaInfo(idx)), pos))
      case Simple(Var(loc), pos) =>
        (idx, Simple(Var(loc), pos))
      case Simple(Literal(value), pos) =>
        (idx, Simple(Literal(value), pos))
      case Simple(TypedTerm(term, typ), pos) =>
        val (idx2, term2) = transformTermFromIndexS(term)(idx)
        (idx2, Simple(TypedTerm(term2, transformTypeTermFromIndex(typ).run(0)._2), pos))
    }
  
  def transformTermFromIndex[T, U](term: Term[SimpleTerm[T, parser.LambdaInfo, TypeSimpleTerm[U, parser.TypeLambdaInfo]]]) =
    State(transformTermFromIndexS[T, U](term))
  
  def transformTerm[T, U](term: Term[SimpleTerm[T, parser.LambdaInfo, TypeSimpleTerm[U, parser.TypeLambdaInfo]]]) =
    transformTermFromIndex(term).run(0)._2.successNel[AbstractError]
  
  def transformTypeTermNelFromIndexS[T](terms: NonEmptyList[Term[TypeSimpleTerm[T, parser.TypeLambdaInfo]]])(idx: Int) =
    transformTermNelFromIndexS1(terms)(idx)(transformTypeTermFromIndexS(_)(_))

  def transformTypeTermFromIndexS[T](term: Term[TypeSimpleTerm[T, parser.TypeLambdaInfo]])(idx: Int): (Int, Term[TypeSimpleTerm[T, TypeLambdaInfo]]) =
    term match {
      case App(fun, args, pos) =>
        val (idx2, fun2) = transformTypeTermFromIndexS(fun)(idx)
        val (idx3, args2) = transformTypeTermNelFromIndexS(args)(idx2)
        (idx3, App(fun2, args2, pos))
      case Simple(TypeLambda(args, body, _), pos) =>
        val (idx2, body2) = transformTypeTermFromIndexS(body)(idx + 1)
        (idx2, Simple(TypeLambda(args, body2, TypeLambdaInfo(idx)), pos))
      case Simple(TypeVar(loc), pos) =>
        (idx, Simple(TypeVar(loc), pos))
      case Simple(TypeLiteral(value), pos) =>
        (idx, Simple(TypeLiteral(value), pos))
      case Simple(KindedTypeTerm(term, kind), pos) =>
        val (idx2, term2) = transformTypeTermFromIndexS(term)(idx + 1)
        (idx2, Simple(KindedTypeTerm(term2, kind), pos))
    }

  def transformTypeTermFromIndex[T](term: Term[TypeSimpleTerm[T, parser.TypeLambdaInfo]]) =
    State(transformTypeTermFromIndexS[T](term))

  def transformTypeTerm[T](term: Term[TypeSimpleTerm[T, parser.TypeLambdaInfo]]) =
    transformTypeTermFromIndex(term).run(0)._2.successNel[AbstractError]

  def transformTree[T, U, V, W](tree: Tree[T, AbstractCombinator[U, parser.LambdaInfo, TypeSimpleTerm[V, parser.TypeLambdaInfo]], W]) = {
    val combs2 = tree.combs.mapValues {
      case Combinator(args, body, _, file) =>
        val args2 = args.map { case Arg(name, typ, pos) => Arg(name, typ.map { transformTypeTermFromIndex(_).run(0)._2 }, pos) }
        Combinator(args2, transformTermFromIndex(body).run(1)._2, LambdaInfo(0), file): AbstractCombinator[U, LambdaInfo, TypeSimpleTerm[V, TypeLambdaInfo]]
    }
    Tree(combs = combs2, treeInfo = tree.treeInfo).successNel[AbstractError]
  }
  
  def transformTypeTree[T, U, V](tree: Tree[T, AbstractTypeCombinator[U, parser.TypeLambdaInfo], V]) = {
    val combs2 = tree.combs.mapValues {
      case TypeCombinator(args, body, _, file) =>
        TypeCombinator(args, transformTypeTermFromIndex(body).run(1)._2, TypeLambdaInfo(0), file): AbstractTypeCombinator[U, TypeLambdaInfo]
      case UnittypeCombinator(n, file)         =>
        UnittypeCombinator(n, file): AbstractTypeCombinator[U, TypeLambdaInfo]
    }
    Tree(combs = combs2, treeInfo = tree.treeInfo).successNel[AbstractError]
  }
  
  def transform[T, U, V, W[_, _], X](tree: Tree[T, AbstractCombinator[U, parser.LambdaInfo, TypeSimpleTerm[V, parser.TypeLambdaInfo]], W[parser.TypeLambdaInfo, X]])(implicit treeInfoTransformer: TreeInfoTransformer[W]) =
    for {
      tree2 <- transformTree(tree)
      treeInfo2 <- treeInfoTransformer.transformTreeInfo(tree.treeInfo)
    } yield Tree(combs = tree2.combs, treeInfo = treeInfo2)
}