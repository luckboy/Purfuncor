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
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbolTabular
import pl.luckboy.purfuncor.frontend.kinder.InferredKind
import pl.luckboy.purfuncor.frontend.kinder.InferredKindTable
import pl.luckboy.purfuncor.frontend.kinder.TypeLambdaInfo
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.frontend.typer.TypeBuiltinFunction

class TyperSpec extends FlatSpec with ShouldMatchers with Inside with TyperSpecUtils
{
  def typer[T, U, V, W, X, Y, Z, TT, TU, TV, E, TC, TE, D](emptyEnv: E, emptyTypeEnv: TE, initData: D)(makeData: String => ValidationNel[AbstractError, D])(kindTableFromData: D => InferredKindTable[TT])(withKindTable: (D, InferredKindTable[TT]) => D)(f2: D => Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], Z]])(g3: (InferredKindTable[TT], InferredTypeTable[T, TT]) => State[TE, E])(h2: D => Term[SimpleTerm[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]]] => ValidationNel[AbstractError, Term[SimpleTerm[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]]]])(implicit init: Initializer[NoType[TT], T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], E], inferrer: Inferrer[SimpleTerm[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], E, Type[TT]], typeInit: Initializer[NoTypeValue[TT, W, TypeLambdaInfo[X, Y], TC], TT, AbstractTypeCombinator[W, TypeLambdaInfo[X, Y]], TE], envSt: TypeInferenceEnvironmentState[E, T, TT], enval: TypeInferenceEnvironmental[E, T, TU, TT], typeTreeInfoExtractor: TreeInfoExtractor[Z, Tree[TT, AbstractTypeCombinator[W, TypeLambdaInfo[X, Y]], TV]], globalSymTabular: GlobalSymbolTabular[Z, T], localSymTabular: LocalSymbolTabular[V, TU])
  {
    val f4 = {
      (data: D) => (tree: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]], kindTable: InferredKindTable[TT]) =>
        State((typeEnv: TE) => (typeEnv, f2(withKindTable(data, kindTable))(tree)))
    }
    val f3 = f4(initData)
    val f = f2(initData)
    val h = h2(initData)
    
    it should "infer the types from the string" in {
      val (env, res) = Typer.inferTypesFromTreeString("""
f = true
g x = x
h x y = x y
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      // f
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("f")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Boolean, Seq()), Seq()) =>
          // #Boolean
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("f")))).get(0)) {
        case Some(InferenceLambdaInfo(TypeTable(types), Seq())) =>
          types should have size(0)
      }
      // g
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("g")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), argKinds) =>
          // \t1 => t1 #-> t1
          inside(argType1) {
            case TypeParamApp(param1, Seq(), 0) =>
              inside(retType1) {
                case TypeParamApp(param2, Seq(), 0) =>
                  List(param1, param2).toSet should have size(1)
              }
          }
          inside(argKinds) {
            case Seq(
                InferredKind(Star(KindType, _)) /* * */) =>
              ()
          }
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("g")))).get(0)) {
        case Some(InferenceLambdaInfo(TypeTable(types), Seq())) =>
          types should have size(1)
          inside(types.get(LocalSymbol("x"))) {
            case Some(InferredType(TypeParamApp(_, Seq(), 0), Seq(InferredKind(Star(KindType, _))))) =>
              ()
          }
      }
      // h
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("h")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), argKinds) =>
          // \t1 t2 => (t1 #-> t2) #-> t1 #-> t2
          inside(argType1) {
            case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType11, retType11)) =>
              inside(argType11) {
                case TypeParamApp(param11, Seq(), 0) =>
                  inside(retType11) {
                    case TypeParamApp(param12, Seq(), 0) =>
                      inside(retType1) {
                        case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType2, retType2)) =>
                          inside(argType2) {
                            case TypeParamApp(param2, Seq(), 0) =>
                              inside(retType2) {
                                case TypeParamApp(param3, Seq(), 0) =>
                                  List(param11, param2).toSet should have size(1)
                                  List(param12, param3).toSet should have size(1)
                                  List(param11, param12, param2, param3).toSet should have size(2)
                              }
                          }
                      }
                  }
              }
          }
          inside(argKinds) {
            case Seq(
                InferredKind(Star(KindType, _)) /* * */,
                InferredKind(Star(KindType, _)) /* * */) =>
             ()
          }
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("h")))).get(0)) {
        case Some(InferenceLambdaInfo(TypeTable(types), Seq())) =>
          types should have size(2)
          inside(types.get(LocalSymbol("x"))) {
            case Some(InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), argKinds)) =>
              inside(argType1) {
                case TypeParamApp(param1, Seq(), 0) =>
                  inside(retType1) {
                    case TypeParamApp(param2, Seq(), 0) =>
                      List(param1, param2).toSet should have size(2)
                  }
              }
              inside(argKinds) {
                case Seq(InferredKind(Star(KindType, _)), InferredKind(Star(KindType, _))) =>
                  ()
              }
          }
          inside(types.get(LocalSymbol("y"))) {
            case Some(InferredType(TypeParamApp(_, Seq(), 0), Seq(InferredKind(Star(KindType, _))))) =>
              ()
          }
      }
    }
    
    it should "infer the type from the string with the let-expression" is (pending)
    
    it should "infer the type from the string with the lambda-expression" is (pending)
    
    it should "infer the type from the string with the nested lambda-expression" is (pending)

    it should "infer the type from the covered local variables" is (pending)
    
    it should "infer the type for the inferred type of the returned value" is (pending)
    
    it should "initialize all types of the non-recursive dependent combinators" is (pending)
    
    it should "initialize all types of the recursive dependent combinators" is (pending)
    
    it should "infer the type from the string with the construct-expression" is (pending)
    
    it should "infer the type from the string with the select-expression" is (pending)
    
    it should "infer the type from the string with the extract-expression" is (pending)
    
    it should "infer the type for the recursive function that is the lambda-expression" is (pending)
    
    it should "infer the type for the defined type of the combinator" is (pending)

    it should "infer the type for the defineds type of the arguments" is (pending)
    
    it should "infer the type for the defined type of the expression" is (pending)
    
    it should "unify the two built-in types" is (pending)
    
    it should "unify the two unit types" is (pending)
    
    it should "unify the global type application with the type that isn't global type application" is (pending)
    
    it should "unift the two global type applications which are same" is (pending)
    
    it should "unify the two global type applications which are different" is (pending)
    
    it should "unify the type parameters" is (pending)
    
    it should "unify the type parameter application with the other type" is (pending)

    it should "unify the type parameter application with the type global type application" is (pending)
    
    it should "unify the type parameter applications which have the equal numbers of the arguments" is (pending)
    
    it should "unify the type parameter application which have the unequal numbers of the arguments" is (pending)
    
    it should "unify the two type lambda-expressions which have the equal number of the arguments" is (pending)
    
    it should "unify the two type lambda-expressions which have the unequal number of the arguments" is (pending)
    
    it should "unify the two type disjunctions" is (pending)
    
    it should "unify the two type conjunctions" is (pending)
    
    it should "unify the two types which are the same logical expression" is (pending)

    it should "unify the two types which are the different logical expressions" is (pending)
    
    it should "unify the two types for the first interation of the unification that has the errors" is (pending)
    
    it should "unfiy the supertype with the type for the Any type" is (pending)
    
    it should "unify the supertype with the type for the Nothing type" is (pending)
    
    it should "unify the supertype with the type for the same logical expressions" is (pending)

    it should "unify the supertype with the type for the different logical expressions" is (pending)
    
    it should "normalize the type applications before the unification of the types" is (pending)
  }
  
  "A Typer" should behave like typer(SymbolTypeInferenceEnvironment.empty[parser.LambdaInfo, parser.TypeLambdaInfo], SymbolTypeEnvironment.empty[TypeLambdaInfo[parser.TypeLambdaInfo, LocalSymbol]], InferredKindTable.empty[GlobalSymbol])(makeInferredKindTable)(identity)((_, kt) => kt)(Typer.transformToSymbolTree2)(Typer.statefullyMakeSymbolTypeInferenceEnvironment3)(Typer.transformToSymbolTerm2)
}