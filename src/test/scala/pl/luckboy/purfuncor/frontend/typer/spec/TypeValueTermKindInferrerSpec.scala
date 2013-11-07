package pl.luckboy.purfuncor.frontend.typer.spec
import scalaz._
import scalaz.Scalaz._
import org.scalatest.FlatSpec
import org.scalatest.Inside
import org.scalatest.matchers.ShouldMatchers
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.NameTree
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbolTabular
import pl.luckboy.purfuncor.frontend.kinder.Kind
import pl.luckboy.purfuncor.frontend.kinder.NoKind
import pl.luckboy.purfuncor.frontend.kinder.InferredKind
import pl.luckboy.purfuncor.frontend.kinder.InferredKindTable
import pl.luckboy.purfuncor.frontend.typer._
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.frontend.typer.TypeBuiltinFunction

class TypeValueTermKindInferrerSpec extends FlatSpec with ShouldMatchers with Inside
{
  def typeValueTermKindInferrer[T, U, V, W, X, Y, Z, TT, TU, E](emptyEnv: E)(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, lmbdindexer.TypeLambdaInfo[X]]], Y]])(implicit init: Initializer[NoKind, Z, AbstractTypeCombinator[W, lmbdindexer.TypeLambdaInfo[X]], E], inferrer: Inferrer[TU, E, Kind], envSt2: KindInferrenceEnvironmentState[E, Z], treeInfoExtractor: TreeInfoExtractor[Y, Tree[Z, AbstractTypeCombinator[W, lmbdindexer.TypeLambdaInfo[X]], TT]], globalSymTabular: GlobalSymbolTabular[TT, Z])
  {
    it should "infer kind from the type value term" in {
      val typeValueTerm = BuiltinType[Z](TypeBuiltinFunction.Any, Seq())
      val (env, kind) = TypeValueTermKindInferrer.inferTypeValueTermKindS(typeValueTerm)(emptyEnv)
      inside(kind) {
        case InferredKind(Star(KindType, _)) =>
          // *
          ()
      }
    }
  }
  
  "A TypeValueTermKindInferrer" should behave like typeValueTermKindInferrer(kinder.SymbolKindInferenceEnvironment.empty[parser.TypeLambdaInfo])(kinder.Kinder.transformToSymbolTree)
}