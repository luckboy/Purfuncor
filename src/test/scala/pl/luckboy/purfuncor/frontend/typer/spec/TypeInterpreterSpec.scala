package pl.luckboy.purfuncor.frontend.typer.spec
import scalaz._
import scalaz.Scalaz._
import org.scalatest.FlatSpec
import org.scalatest.Inside
import org.scalatest.matchers.ShouldMatchers
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.typer._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol
import pl.luckboy.purfuncor.frontend.resolver.NameTree
import pl.luckboy.purfuncor.frontend.kinder.InferredKindTable
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.frontend.typer.TypeBuiltinFunction

class TypeInterpreterSpec extends FlatSpec with ShouldMatchers with Inside
{
  def typer[T, U, V, W, X, Y, Z, TT, C, E](emptyEnv: E)(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => State[E, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, V, TypeSimpleTerm[W, X]], Y]]])(g: Term[TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]] => ValidationNel[AbstractError, Term[TypeSimpleTerm[W, X]]])(implicit init: Initializer[NoTypeValue[Z, W, X, C], Z, AbstractTypeCombinator[W, X], E], eval: Evaluator[TypeSimpleTerm[W, X], E, TypeValue[Z, W, X, C]], enval: TypeEnvironmental[E, TypeValue[Z, W, X, C]], treeInfoExtractor: TreeInfoExtractor[Y, Tree[Z, AbstractTypeCombinator[W, X], TT]])
  {
    it should "interpret the string of the type term" in {
      val (env, res) = Typer.interpretTypeTermString("##| (##& #Int #NonZero) #Char")(NameTree.empty)(g).run(emptyEnv)
      inside(res) {
        case Success(EvaluatedTypeValue(term)) =>
          term should be ===(TypeDisjunction(Set[TypeValueTerm[Z]](
              TypeConjunction(Set[TypeValueTerm[Z]](
                  BuiltinType(TypeBuiltinFunction.Int, Seq()),
                  BuiltinType(TypeBuiltinFunction.NonZero, Seq()))),
              BuiltinType(TypeBuiltinFunction.Char, Seq()))))
      }
    }
    
    it should "interpret the string of the type tree" in {
      val (env, res) = Typer.interpretTypeTreeFromTreeString("""
type T = #Int
type U = ##& #Short #Zero 
type V = ##-> #Byte U
""")(NameTree.empty)(f).run(emptyEnv)
	  res should be ===(().success.success)
      inside(enval.globalTypeVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("T")))) {
        case EvaluatedTypeValue(term) =>
          term should be ===(BuiltinType(TypeBuiltinFunction.Int, Seq()))
      }
      inside(enval.globalTypeVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("U")))) {
        case EvaluatedTypeValue(term) =>
          term should be ===(TypeConjunction(Set[TypeValueTerm[Z]](
              BuiltinType(TypeBuiltinFunction.Short, Seq()),
              BuiltinType(TypeBuiltinFunction.Zero, Seq()))))
      }
      inside(enval.globalTypeVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("V")))) {
        case EvaluatedTypeValue(term) =>
          term should be ===(BuiltinType(TypeBuiltinFunction.Fun, Seq(
              BuiltinType(TypeBuiltinFunction.Byte, Seq[TypeValueTerm[Z]]()),
              TypeConjunction(Set[TypeValueTerm[Z]](
                  BuiltinType(TypeBuiltinFunction.Short, Seq()),
                  BuiltinType(TypeBuiltinFunction.Zero, Seq()))))))
      }
    }
    
    it should "initialize all independent type variables" is (pending)
    
    it should "initialize all dependent type variables" is (pending)

    it should "interpret the type applications of the type lambda-expressions" is (pending)
    
    it should "interpret the partial type applications" is (pending)
    
    it should "interpret the type term with the covered local type variables" is (pending)    
    
    it should "interpret the type term with the global type variables" is (pending)

    it should "interpret the type term for the type parameters" is (pending)    
  }

  "A Typer" should behave like typer(SymbolTypeEnvironment.empty[kinder.TypeLambdaInfo[parser.TypeLambdaInfo, LocalSymbol]])(Typer.statefullyTransformToSymbolTree2(InferredKindTable.empty))(Typer.transformToSymbolTypeTerm2(InferredKindTable.empty))
}