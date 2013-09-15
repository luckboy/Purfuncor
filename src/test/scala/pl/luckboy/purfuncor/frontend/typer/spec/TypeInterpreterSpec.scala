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
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbolTabular
import pl.luckboy.purfuncor.frontend.kinder.InferredKindTable
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.frontend.typer.TypeBuiltinFunction

class TypeInterpreterSpec extends FlatSpec with ShouldMatchers with Inside
{
  def typer[T, U, V, W, X, Y, Z, TT, C, E](emptyEnv: E)(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => State[E, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, V, TypeSimpleTerm[W, X]], Y]]])(g: Term[TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]] => ValidationNel[AbstractError, Term[TypeSimpleTerm[W, X]]])(implicit init: Initializer[NoTypeValue[Z, W, X, C], Z, AbstractTypeCombinator[W, X], E], eval: Evaluator[TypeSimpleTerm[W, X], E, TypeValue[Z, W, X, C]], enval: TypeEnvironmental[E, TypeValue[Z, W, X, C]], treeInfoExtractor: TreeInfoExtractor[Y, Tree[Z, AbstractTypeCombinator[W, X], TT]], globalSymTabular: GlobalSymbolTabular[E, Z])
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
    
    it should "initialize all independent type variables" in {
      val (env, res) = Typer.interpretTypeTreeFromTreeString("""
type T = #Int
type U = ##& #Int #NonZero
type V = #Char
""")(NameTree.empty)(f).run(emptyEnv)
	  res should be ===(().success.success)
      inside(enval.globalTypeVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("T")))) {
        case EvaluatedTypeValue(term) =>
          term should be ===(BuiltinType(TypeBuiltinFunction.Int, Seq()))
      }
      inside(enval.globalTypeVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("U")))) {
        case EvaluatedTypeValue(term) =>
          term should be ===(TypeConjunction(Set[TypeValueTerm[Z]](
              BuiltinType(TypeBuiltinFunction.Int, Seq()),
              BuiltinType(TypeBuiltinFunction.NonZero, Seq()))))
      }
      inside(enval.globalTypeVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("V")))) {
        case EvaluatedTypeValue(term) =>
          term should be ===(BuiltinType(TypeBuiltinFunction.Char, Seq()))
      }
    }
    
    it should "initialize all dependent type variables" in {
      val (env, res) = Typer.interpretTypeTreeFromTreeString("""
type T = tuple 2 (U #Int) (X #Int #Char)
type U t = ##& (V t) W
type V t = t
type W = #Zero
type X t1 t2 = tuple 3 Z (Y t1) t2
type Y t = ##& t #NonZero
type Z = #Int
""")(NameTree.empty)(f).run(emptyEnv)
	  res should be ===(().success.success)
      inside(enval.globalTypeVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("T")))) {
        case EvaluatedTypeValue(term) =>
          term should be ===(TupleType(Seq(
              TypeConjunction(Set[TypeValueTerm[Z]](
                  BuiltinType(TypeBuiltinFunction.Int, Seq[TypeValueTerm[Z]]()),
                  BuiltinType(TypeBuiltinFunction.Zero, Seq[TypeValueTerm[Z]]()))),
              TupleType(Seq(
                  BuiltinType(TypeBuiltinFunction.Int, Seq[TypeValueTerm[Z]]()),
                  TypeConjunction(Set[TypeValueTerm[Z]](
                      BuiltinType(TypeBuiltinFunction.Int, Seq[TypeValueTerm[Z]]()),
                      BuiltinType(TypeBuiltinFunction.NonZero, Seq[TypeValueTerm[Z]]()))),
                  BuiltinType(TypeBuiltinFunction.Char, Seq[TypeValueTerm[Z]]()))))))
      }
      inside(enval.globalTypeVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("U")))) {
        case TypeCombinatorValue(_, _, _) => ()
      }
      inside(enval.globalTypeVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("V")))) {
        case TypeCombinatorValue(_, _, _) => ()
      }
      inside(enval.globalTypeVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("W")))) {
        case EvaluatedTypeValue(term) =>
          term should be ===(BuiltinType(TypeBuiltinFunction.Zero, Seq[TypeValueTerm[Z]]()))
      }
      inside(enval.globalTypeVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("X")))) {
        case TypeCombinatorValue(_, _, _) => ()
      }
      inside(enval.globalTypeVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("Y")))) {
        case TypeCombinatorValue(_, _, _) => ()
      }
      inside(enval.globalTypeVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("Z")))) {
        case EvaluatedTypeValue(term) =>
          term should be ===(BuiltinType(TypeBuiltinFunction.Int, Seq[TypeValueTerm[Z]]()))
      }
    }

    it should "initialize the recusive type" in {
      val (env, res) = Typer.interpretTypeTreeFromTreeString("type T = tuple 2 (##| T #Int) #Int")(NameTree.empty)(f).run(emptyEnv)
	  res should be ===(().success.success)
      inside(enval.globalTypeVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("T")))) {
        case EvaluatedTypeValue(term) =>
          inside(globalSymTabular.getGlobalLocationFromTable(env)(GlobalSymbol(NonEmptyList("T")))) {
            case Some(loc) =>
              term should be ===(TupleType(Seq[TypeValueTerm[Z]](
                  TypeDisjunction(Set[TypeValueTerm[Z]](
                      GlobalTypeApp(loc, Seq(), GlobalSymbol(NonEmptyList("T"))),
                      BuiltinType(TypeBuiltinFunction.Int, Seq()))),
                  BuiltinType(TypeBuiltinFunction.Int, Seq()))))
          }
      }
    }

    it should "interpret the application of the recusive type combinator" in {
      val (env, res) = Typer.interpretTypeTreeFromTreeString("""
type T = U #Int #Long
type U t1 t2 = tuple 3 t1 (V t1 t2) (U t1 t2)
type V t1 t2 = tuple 2 #Char (U t1 t2)
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalTypeVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("T")))) {
        case EvaluatedTypeValue(term) =>
          inside(globalSymTabular.getGlobalLocationFromTable(env)(GlobalSymbol(NonEmptyList("U")))) {
            case Some(loc) =>
              term should be ===(TupleType(Seq[TypeValueTerm[Z]](
                  BuiltinType(TypeBuiltinFunction.Int, Seq()),
                  TupleType(Seq[TypeValueTerm[Z]](
                      BuiltinType(TypeBuiltinFunction.Char, Seq()),
                      GlobalTypeApp(loc,
                          Seq[TypeValueLambda[Z]](
                              TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Int, Seq())),
                              TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Long, Seq()))),
                          GlobalSymbol(NonEmptyList("U"))))),
                  GlobalTypeApp(loc,
                      Seq[TypeValueLambda[Z]](
                          TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Int, Seq())),
                          TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Long, Seq()))),
                      GlobalSymbol(NonEmptyList("U"))))))
          }
      }
    }
    
    it should "interpret the applications of the type lambda-expressions" in {
      val (env, res) = Typer.interpretTypeTreeFromTreeString("""
type T = tuple 2 ((\t1 t2 => ##& t1 t2) #Int #NonZero) ((\t1 => (\_ t2 => t2 t1) #Any) #Char (\t1 => ##| t1 #Float))
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalTypeVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("T")))) {
        case EvaluatedTypeValue(term) =>
          term should be ===(TupleType(Seq[TypeValueTerm[Z]](
              TypeConjunction(Set[TypeValueTerm[Z]](
                  BuiltinType(TypeBuiltinFunction.Int, Seq[TypeValueTerm[Z]]()),
                  BuiltinType(TypeBuiltinFunction.NonZero, Seq[TypeValueTerm[Z]]()))),
              TypeDisjunction(Set[TypeValueTerm[Z]](
                  BuiltinType(TypeBuiltinFunction.Char, Seq[TypeValueTerm[Z]]()),
                  BuiltinType(TypeBuiltinFunction.Float, Seq[TypeValueTerm[Z]]()))))))
      }
    }
    
    it should "interpret the partial applications of the types" in {
      val (env, res) = Typer.interpretTypeTreeFromTreeString("""
type T = (\t1 t2 => tuple 2 (t1 #NonZero) (t2 #Zero)) ((\t1 t2 => ##& t1 t2) #Int) (U #Int #Any)
type U t1 t2 t3 = ##& (##& t1 t2) t3
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalTypeVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("T")))) {
        case EvaluatedTypeValue(term) =>
          term should be ===(TupleType(Seq[TypeValueTerm[Z]](
              TypeConjunction(Set[TypeValueTerm[Z]](
                  BuiltinType(TypeBuiltinFunction.Int, Seq[TypeValueTerm[Z]]()),
                  BuiltinType(TypeBuiltinFunction.NonZero, Seq[TypeValueTerm[Z]]()))),
              TypeConjunction(Set[TypeValueTerm[Z]](
                  BuiltinType(TypeBuiltinFunction.Int, Seq[TypeValueTerm[Z]]()),
                  BuiltinType(TypeBuiltinFunction.Any, Seq[TypeValueTerm[Z]]()),
                  BuiltinType(TypeBuiltinFunction.Zero, Seq[TypeValueTerm[Z]]()))))))
      }
    }
    
    it should "interpret the type term with the covered local type variables" in {
      val (env, res) = Typer.interpretTypeTreeFromTreeString("""
type T = U #Byte #Short #Int #Long
type U t1 t2 = (\t3 t4 t2 t1 => (##| (##| t1 t2) (tuple 2 t3 t4))) t1 t2
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalTypeVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("T")))) {
        case EvaluatedTypeValue(term) =>
          term should be ===(TypeDisjunction(Set[TypeValueTerm[Z]](
              BuiltinType(TypeBuiltinFunction.Long, Seq[TypeValueTerm[Z]]()),
              BuiltinType(TypeBuiltinFunction.Int, Seq[TypeValueTerm[Z]]()),
              TupleType(Seq[TypeValueTerm[Z]](
                  BuiltinType(TypeBuiltinFunction.Byte, Seq[TypeValueTerm[Z]]()),
                  BuiltinType(TypeBuiltinFunction.Short, Seq[TypeValueTerm[Z]]()))))))
      }
    }
    
    it should "interpret the type term for the type parameters" is (pending)    
  }

  "A Typer" should behave like typer(SymbolTypeEnvironment.empty[kinder.TypeLambdaInfo[parser.TypeLambdaInfo, LocalSymbol]])(Typer.statefullyTransformToSymbolTree2(InferredKindTable.empty))(Typer.transformToSymbolTypeTerm2(InferredKindTable.empty))
}