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
import pl.luckboy.purfuncor.frontend.kinder.TypeTreeInfo
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.common.Arrow
import pl.luckboy.purfuncor.frontend.typer.TypeBuiltinFunction

class TyperSpec extends FlatSpec with ShouldMatchers with Inside with TyperSpecUtils
{
  def typer[T, U, V, W, X, Y, Z, TT, TU, TV, E, TC, TE, D](emptyEnv: E, emptyTypeEnv: TE, initData: D)(makeData: String => ValidationNel[AbstractError, D])(kindTableFromData: D => InferredKindTable[TT])(withKindTable: (D, InferredKindTable[TT]) => D)(f2: D => Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], Z]])(g3: (InferredKindTable[TT], InferredTypeTable[T, TT]) => State[TE, E])(h2: D => Term[SimpleTerm[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]]] => ValidationNel[AbstractError, Term[SimpleTerm[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]]]])(implicit init: Initializer[NoType[TT], T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], E], inferrer: Inferrer[SimpleTerm[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, TypeLambdaInfo[X, Y]]], E, Type[TT]], typeInit: Initializer[NoTypeValue[TT, W, TypeLambdaInfo[X, Y], TC], TT, AbstractTypeCombinator[W, TypeLambdaInfo[X, Y]], TE], envSt: TypeInferenceEnvironmentState[E, T, TT], enval: TypeInferenceEnvironmental[E, T, TU, TT], treeInfoExtractor: TreeInfoExtractor[Z, Tree[TT, AbstractTypeCombinator[W, TypeLambdaInfo[X, Y]], TypeTreeInfo[TV, TT]]], globalSymTabular: GlobalSymbolTabular[Z, T], typeGlobalSymTabular: GlobalSymbolTabular[TV, TT], localSymTabular: LocalSymbolTabular[V, TU])
  {
    //TODO: add a test for the mismatched types of the recursive dependent combinators.
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
        case Some(InferenceLambdaInfo(TypeTable(types), None, combTypeParams)) =>
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
        case Some(InferenceLambdaInfo(TypeTable(types), None, combTypeParams)) =>
          types should have size(1)
          inside(types.get(LocalSymbol("x"))) {
            case Some(InferredType(TypeParamApp(_, Seq(), 0), Seq(InferredKind(Star(KindType, _))))) =>
              ()
          }
          combTypeParams should have size(1)
          combTypeParams.keySet should be ===(Set(0))
          combTypeParams.values.toSet should be ===(Set(0))
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("g")))).get(1)) {
        case Some(InferenceLambdaInfo(TypeTable(types), None, combTypeParams)) =>
          types should have size(0)
          combTypeParams should have size(0)
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
        case Some(InferenceLambdaInfo(TypeTable(types), None, combTypeParams)) =>
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
          combTypeParams should have size(2)
          combTypeParams.keySet should be ===(Set(0, 1))
          combTypeParams.values.toSet should be ===(Set(0, 1))
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("h")))).get(1)) {
        case Some(InferenceLambdaInfo(TypeTable(types), None, combTypeParams)) =>
          types should have size(0)
          combTypeParams should have size(0)
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("h")))).get(2)) {
        case Some(InferenceLambdaInfo(TypeTable(types), None, combTypeParams)) =>
          types should have size(0)
          combTypeParams should have size(0)
      }
    }
    
    it should "infer the type from the string with the let-expression" in {
      val (env, res) = Typer.inferTypesFromTreeString("""
f g x = let
    a = g x
    b = tuple 2 x true
  in
    tuple 2 a b
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("f")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), argKinds) =>
          // \t1 t2 => (t1 #-> t2) #-> t1 #-> (t2, (t1, #Boolean))
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
                                case TupleType(Seq(type21, type22)) =>
                                  inside(type21) {
                                    case TypeParamApp(param21, Seq(), 0) =>
                                      inside(type22) {
                                        case TupleType(Seq(TypeParamApp(param22, Seq(), 0), BuiltinType(TypeBuiltinFunction.Boolean, Seq()))) =>
                                          List(param11, param2, param22).toSet should have size(1)
                                          List(param12, param21).toSet should have size(1)
                                          List(param11, param12, param2, param21, param22).toSet should have size(2)
                                      }
                                  }
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
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("f")))).get(0)) {
        case Some(InferenceLambdaInfo(TypeTable(types), None, combTypeParams)) =>
          types should have size(2)
          inside(types.get(LocalSymbol("g"))) {
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
          inside(types.get(LocalSymbol("x"))) {
            case Some(InferredType(TypeParamApp(_, Seq(), 0), Seq(InferredKind(Star(KindType, _))))) =>
              ()
          }
          combTypeParams should have size(2)
          combTypeParams.keySet should be ===(Set(0, 1))
          combTypeParams.values.toSet should be ===(Set(0, 1))
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("f")))).get(1)) {
        case Some(InferenceLambdaInfo(TypeTable(types), None, combTypeParams)) =>
          types should have size(2)
          inside(types.get(LocalSymbol("a"))) {
            case Some(InferredType(TypeParamApp(_, Seq(), 0), Seq(InferredKind(Star(KindType, _))))) =>
              ()
          }
          inside(types.get(LocalSymbol("b"))) {
            case Some(InferredType(TupleType(Seq(TypeParamApp(_, Seq(), 0), BuiltinType(TypeBuiltinFunction.Boolean, Seq()))), Seq(InferredKind(Star(KindType, _))))) =>
              ()
          }
          combTypeParams should have size(0)
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("f")))).get(2)) {
        case Some(InferenceLambdaInfo(TypeTable(types), None, combTypeParams)) =>
          types should have size(0)
          combTypeParams should have size(0)
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("f")))).get(3)) {
        case Some(InferenceLambdaInfo(TypeTable(types), None, combTypeParams)) =>
          types should have size(0)
          combTypeParams should have size(0)
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("f")))).get(4)) {
        case Some(InferenceLambdaInfo(TypeTable(types), None, combTypeParams)) =>
          types should have size(0)
          combTypeParams should have size(0)
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("f")))).get(5)) {
        case Some(InferenceLambdaInfo(TypeTable(types), None, combTypeParams)) =>
          types should have size(0)
          combTypeParams should have size(0)
      }
    }
    
    it should "infer the type from the string with the lambda-expression" in {
      val (env, res) = Typer.inferTypesFromTreeString("""
f g x = (\y h => tuple 2 (g x) (h y)) x
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("f")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), argKinds) =>
          // \t1 t2 t3 => (t1 #-> t2) #-> t1 #-> (t1 #-> t3) #-> (t2, t3)
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
                                case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType3, retType3)) =>
                                  inside(argType3) {
                                    case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType31, retType31)) =>
                                      inside(argType31) {
                                        case TypeParamApp(param31, Seq(), 0) =>
                                          inside(retType31) {
                                            case TypeParamApp(param32, Seq(), 0) =>
                                              inside(retType3) {
                                                case TupleType(Seq(TypeParamApp(param33, Seq(), 0), TypeParamApp(param34, Seq(), 0))) =>
                                                  List(param11, param2, param31).toSet should have size (1)
                                                  List(param12, param33).toSet should have size(1)
                                                  List(param32, param34).toSet should have size(1)
                                                  List(param11, param12, param2, param31, param32, param33, param34).toSet should have size(3)
                                              }
                                          }
                                      }
                                  }
                              }
                          }
                      }
                  }
              }
          }
          inside(argKinds) {
            case Seq(
                InferredKind(Star(KindType, _)) /* * */,
                InferredKind(Star(KindType, _)) /* * */,
                InferredKind(Star(KindType, _)) /* * */) =>
              ()
          }
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("f")))).get(0)) {
        case Some(InferenceLambdaInfo(TypeTable(types), None, combTypeParams)) =>
          types should have size(2)
          inside(types.get(LocalSymbol("g"))) {
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
          inside(types.get(LocalSymbol("x"))) {
            case Some(InferredType(TypeParamApp(_, Seq(), 0), Seq(InferredKind(Star(KindType, _))))) =>
              ()
          }
          combTypeParams should have size(3)
          combTypeParams.keySet should be ===((0 to 2).toSet)
          combTypeParams.values.toSet should be ===((0 to 2).toSet)
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("f")))).get(1)) {
        case Some(InferenceLambdaInfo(TypeTable(types), None, combTypeParams)) =>
          types should have size(2)
          inside(types.get(LocalSymbol("y"))) {
            case Some(InferredType(TypeParamApp(_, Seq(), 0), Seq(InferredKind(Star(KindType, _))))) =>
              ()
          }
          inside(types.get(LocalSymbol("h"))) {
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
          combTypeParams should have size(0)
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("f")))).get(2)) {
        case Some(InferenceLambdaInfo(TypeTable(types), None, combTypeParams)) =>
          types should have size(0)
          combTypeParams should have size(0)
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("f")))).get(3)) {
        case Some(InferenceLambdaInfo(TypeTable(types), None, combTypeParams)) =>
          types should have size(0)
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("f")))).get(4)) {
        case Some(InferenceLambdaInfo(TypeTable(types), None, combTypeParams)) =>
          types should have size(0)
          combTypeParams should have size(0)
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("f")))).get(5)) {
        case Some(InferenceLambdaInfo(TypeTable(types), None, combTypeParams)) =>
          types should have size(0)
          combTypeParams should have size(0)
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("f")))).get(6)) {
        case Some(InferenceLambdaInfo(TypeTable(types), None, combTypeParams)) =>
          types should have size(0)
          combTypeParams should have size(0)
      }
    }
    
    it should "infer the type from the string with the nested lambda-expression" in {
      val (env, res) = Typer.inferTypesFromTreeString("""
f x1 = \x2 g => \h x3 => #fAdd (#fMul x1 x3) (g (h x2))
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("f")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), argKinds) =>
          // \t1 t2 => #Float #-> t1 #-> (t2 #-> #Float) #-> (t1 #-> t2) #-> #Float #-> #Float
          inside(argType1) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
          inside(retType1) {
            case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType2, retType2)) =>
              inside(argType2) {
                case TypeParamApp(param2, Seq(), 0) =>
                  inside(retType2) {
                    case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType3, retType3)) =>
                      inside(argType3) {
                        case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType31, retType31)) =>
                          inside(argType31) {
                            case TypeParamApp(param31, Seq(), 0) =>
                              inside(retType31) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
                              inside(retType3) {
                                case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType4, retType4)) =>
                                  inside(argType4) {
                                    case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType41, retType41)) =>
                                      inside(argType41) {
                                        case TypeParamApp(param41, Seq(), 0) =>
                                          inside(retType41) {
                                            case TypeParamApp(param42, Seq(), 0) =>
                                              inside(retType4) {
                                                case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType5, retType5)) =>
                                                  inside(argType5) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
                                                  inside(retType5) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
                                                  List(param2, param41).toSet should have size(1)
                                                  List(param31, param42).toSet should have size(1)
                                                  List(param2, param31, param41, param42).toSet should have size(2)
                                              }
                                          }
                                      }
                                  }
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
    }

    it should "infer the type from the covered local variables" in {
      val (env, res) = Typer.inferTypesFromTreeString("""
f = let
    a = true
    b = 0.1f
  in
    tuple 3 (let
      a = 'a'
      b = 0.1
    in
      tuple 2 a b) a b
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("f")))) {
        case InferredType(TupleType(Seq(type1, type2, type3)), Seq()) =>
          // ((#Char, #Double), #Boolean, #Float)
          inside(type1) { case TupleType(Seq(BuiltinType(TypeBuiltinFunction.Char, Seq()), BuiltinType(TypeBuiltinFunction.Double, Seq()))) => () }
          inside(type2) { case BuiltinType(TypeBuiltinFunction.Boolean, Seq()) => () }
          inside(type3) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
      }
    }
    
    it should "infer the type for the inferred type of the returned value" in {
      val (env, res) = Typer.inferTypesFromTreeString("""
f x y z = tuple 3 x y (z y)
g x y = f (#dAdd 1.0 x) y
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("g")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), argKinds) =>
          // \t1 t2 => #Double #-> t1 #-> (t1 #-> t2) #-> (#Double, t1, t2)
          inside(argType1) { case BuiltinType(TypeBuiltinFunction.Double, Seq()) => () }
          inside(retType1) {
            case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType2, retType2)) =>
              inside(argType2) {
                case TypeParamApp(param2, Seq(), 0) =>
                  inside(retType2) {
                    case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType3, retType3)) =>
                      inside(argType3) {
                        case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType31, retType31)) =>
                          inside(argType31) {
                            case TypeParamApp(param31, Seq(), 0) =>
                              inside(retType31) {
                                case TypeParamApp(param32, Seq(), 0) =>
                                  inside(retType3) {
                                    case TupleType(Seq(BuiltinType(TypeBuiltinFunction.Double, Seq()), TypeParamApp(param33, Seq(), 0), TypeParamApp(param34, Seq(), 0))) =>
                                      List(param2, param31, param33).toSet should have size(1)
                                      List(param32, param34).toSet should have size(1)
                                      List(param2, param31, param32, param33, param34).toSet should have size(2)
                                  }
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
    }
    
    it should "initialize all types of the non-recursive dependent combinators" in {
      val (env, res) = Typer.inferTypesFromTreeString("""
f x y = tuple 3 (g y) (h x y) (#zXor y true)
g x = tuple 2 x (i x)
h x y = i (x y)
i x = j
j = 'a'
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      // f
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("f")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), argKinds) =>
          // \t1 => (#Boolean #-> t1) #-> #Boolean #-> ((#Boolean, #Char), #Char, #Boolean)
          inside(argType1) {
            case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType11, retType11)) =>
              inside(argType11) { case BuiltinType(TypeBuiltinFunction.Boolean, Seq()) => () }
              inside(retType11) { case TypeParamApp(_, Seq(), 0) => () }
          }
          inside(retType1) {
            case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType2, retType2)) =>
              inside(argType2) { case BuiltinType(TypeBuiltinFunction.Boolean, Seq()) => () }
              inside(retType2) {
                case TupleType(Seq(type21, type22, type23)) =>
                 inside(type21) { case TupleType(Seq(BuiltinType(TypeBuiltinFunction.Boolean, Seq()), BuiltinType(TypeBuiltinFunction.Char, Seq()))) => () }
                 inside(type22) { case BuiltinType(TypeBuiltinFunction.Char, Seq()) => () }
                 inside(type23) { case BuiltinType(TypeBuiltinFunction.Boolean, Seq()) => () }
              }
          }
          inside(argKinds) {
            case Seq(
                InferredKind(Star(KindType, _)) /* * */) =>
              ()
          }
      }
      // g
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("g")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), argKinds) =>
          // \t1 => t1 #-> (t1, #Char)
          inside(argType1) {
            case TypeParamApp(param1, Seq(), 0) =>
              inside(retType1) {
                case TupleType(Seq(TypeParamApp(param11, Seq(), 0), BuiltinType(TypeBuiltinFunction.Char, Seq()))) =>
                  List(param1, param11).toSet should have size(1)
              }
          }
          inside(argKinds) {
            case Seq(
                InferredKind(Star(KindType, _)) /* * */) =>
              ()
          }
      }
      // h
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("h")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), argKinds) =>
          // \t1 t2 => (t1 #-> t2) #-> t1 #-> #Char
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
                                case BuiltinType(TypeBuiltinFunction.Char, Seq()) =>
                                  List(param11, param2).toSet should have size(1)
                                  List(param11, param12, param2).toSet should have size(2)
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
      // i
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("i")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), argKinds) =>
          // \t1 => t1 #-> #Char
          inside(argType1) { case TypeParamApp(_, Seq(), 0) => () }
          inside(retType1) { case BuiltinType(TypeBuiltinFunction.Char, Seq()) => () }
          inside(argKinds) {
            case Seq(
                InferredKind(Star(KindType, _)) /* * */) =>
              ()
          }
      }
      // j
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("j")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Char, Seq()), Seq()) =>
          // #Char
          ()
      }
    }
    
    it should "initialize all types of the recursive dependent combinators" in {
      val (env, res) = Typer.inferTypesFromTreeString("""
f = g true 0.1f
g = h 
h x y = i (#fAdd y y) x
i x y = #fAdd (#fAdd (j y x 1.0) (k x (\_ _ => #fMul (#fAdd x x) 0.2f))) (#floatFromDouble (l m))
j x y z = #cond (\_ => #fAdd y (#floatFromDouble z)) (\_ => h x y) x 
k x y = #cond (\_ => x) (\_ => f) true
l = #dNeg
m = 2.0
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      // f
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("f")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Float, Seq()), Seq()) =>
          // #Float
          ()
      }
      // g
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("g")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), Seq()) =>
          // #Boolean #-> #Float #-> #Float
          inside(argType1) { case BuiltinType(TypeBuiltinFunction.Boolean, Seq()) => () }
          inside(retType1) {
            case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType2, retType2)) =>
              inside(argType2) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
              inside(retType2) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
          }
      }
      // h
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("h")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), Seq()) =>
          // #Boolean #-> #Float #-> #Float
          inside(argType1) { case BuiltinType(TypeBuiltinFunction.Boolean, Seq()) => () }
          inside(retType1) {
            case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType2, retType2)) =>
              inside(argType2) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
              inside(retType2) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
          }
      }
      // i
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("i")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), Seq()) =>
          // #Float #-> #Boolean #-> #Float
          inside(argType1) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
          inside(retType1) {
            case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType2, retType2)) =>
              inside(argType2) { case BuiltinType(TypeBuiltinFunction.Boolean, Seq()) => () }
              inside(retType2) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
          }
      }
      // j
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("j")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), Seq()) =>
          // #Boolean #-> #Float #-> #Double #-> #Float
          inside(argType1) { case BuiltinType(TypeBuiltinFunction.Boolean, Seq()) => () }
          inside(retType1) {
            case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType2, retType2)) =>
              inside(argType2) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
              inside(retType2) { 
                case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType3, retType3)) => 
                  inside(argType3) { case BuiltinType(TypeBuiltinFunction.Double, Seq()) => () }
                  inside(retType3) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
              }
          }
      }
      // k
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("k")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), argKinds) =>
          // \t1 t2 => #Float #-> (t1 #-> t2 #-> #Float) #-> #Float
          inside(argType1) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
          inside(retType1) {
            case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType2, retType2)) =>
              inside(argType2) {
                case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType21, retType21)) =>
                  inside(argType21) {
                    case TypeParamApp(param21, Seq(), 0) =>
                      inside(retType21) {
                        case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType22, retType22)) =>
                          inside(argType22) {
                            case TypeParamApp(param22, Seq(), 0) =>
                              inside(retType22) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
                              List(param21, param22).toSet should have size(2)
                          }
                      }
                  }
              }
              inside(retType2) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
          }
          inside(argKinds) {
            case Seq(
                InferredKind(Star(KindType, _)) /* * */,
                InferredKind(Star(KindType, _)) /* * */) =>
              ()
          }
      }
      // l
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("l")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), Seq()) =>
          // #Double #-> #Double
          inside(argType1) { case BuiltinType(TypeBuiltinFunction.Double, Seq()) => () }
          inside(retType1) { case BuiltinType(TypeBuiltinFunction.Double, Seq()) => () }
      }
      // m
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("m")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Double, Seq()), Seq()) =>
          // #Double
          ()
      }
    }
    
    it should "infer the type from the string with the construct-expression" in {
      val (env, res) = Typer.inferTypesFromTreeString("f = construct 3 'a'")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("f")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), argKinds) =>
          // \t1 t2 t3 => t1 #-> t2 #-> t3 #& (#Char, t1, t2)
          inside(argType1) {
            case TypeParamApp(param1, Seq(), 0) =>
              inside(retType1) {
                case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType2, retType2)) =>
                  inside(argType2) {
                    case TypeParamApp(param2, Seq(), 0) =>
                      inside(retType2) {
                        case TypeConjunction(types2) =>
                          types2 should have size(2)
                          inside(for {
                            x1 <- types2.collectFirst { case TypeParamApp(param21, Seq(), 0) => param21 }
                            x2 <- types2.collectFirst { case TupleType(Seq(BuiltinType(TypeBuiltinFunction.Char, Seq()), TypeParamApp(param22, Seq(), 0), TypeParamApp(param23, Seq(), 0))) => (param22, param23) }
                          } yield (x1, x2)) {
                            case Some((param21, (param22, param23))) =>
                              List(param1, param22).toSet should have size(1)
                              List(param2, param23).toSet should have size(1)
                              List(param1, param2, param21, param22, param23).toSet should have size(3)
                          }
                      }
                  }
              }
          }
          inside(argKinds) {
            case Seq(
                InferredKind(Star(KindType, _)) /* * */,
                InferredKind(Star(KindType, _)) /* * */,
                InferredKind(Star(KindType, _)) /* * */) =>
              ()
          }
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("f")))).get(0)) {
        case Some(InferenceLambdaInfo(TypeTable(types), None, combTypeParams)) =>
          types should have size(0)
          combTypeParams should have size(3)
          combTypeParams.keySet should be ===((0 to 2).toSet)
          combTypeParams.values.toSet should be ===((0 to 2).toSet)
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("f")))).get(1)) {
        case Some(InferenceLambdaInfo(TypeTable(types), polyFunType, combTypeParams)) =>
          types should have size (0)
          inside(polyFunType) {
            case Some(InferredType(TypeConjunction(types1), argKinds)) =>
              types1 should have size(2)
              inside(for {
                x1 <- types1.collectFirst { case TypeParamApp(param11, Seq(), 0) => param11 }
                x2 <- types1.collectFirst { case TupleType(Seq(BuiltinType(TypeBuiltinFunction.Char, Seq()), TypeParamApp(param12, Seq(), 0), TypeParamApp(param13, Seq(), 0))) => (param12, param13) }
              } yield (x1, x2)) {
                case Some((param11, (param12, param13))) =>
                  List(param11, param12, param13).toSet should have size(3)
              }
              inside(argKinds) {
                case Seq(
                    InferredKind(Star(KindType, _)) /* * */,
                    InferredKind(Star(KindType, _)) /* * */,
                    InferredKind(Star(KindType, _)) /* * */) =>
                 ()
              }
          }
          combTypeParams should have size(0)
      }
    }
    
    it should "infer the type from the string with the select-expression" in {
      val s = """
unittype 3 T
unittype 0 U
unittype 1 V
f x = x select {
    (y: \t1 t2 => T #Char t1 t2) => 'a'
    (y: U)                       => 'b'
    (y: V #Boolean)              => 'c'
  }
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              res2 should be ===(().success.success)
              val syms = List(
                  GlobalSymbol(NonEmptyList("T")),
                  GlobalSymbol(NonEmptyList("U")),
                  GlobalSymbol(NonEmptyList("V")))
              inside(syms.flatMap(typeGlobalSymTabular.getGlobalLocationFromTable(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo).treeInfo.treeInfo))) {
                case List(tLoc, uLoc, vLoc) =>
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("f")))) {
                    case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), argKinds) =>
                      // \t1 t2 => ((T #Char t1 t2) | U | (V #Boolean)) #-> #Char
                      inside(argType1) {
                        case TypeDisjunction(types1) =>
                          types1 should have size(3)
                          inside(for {
                            x1 <- types1.collectFirst { case GlobalTypeApp(loc11, Seq(arg11, arg12, arg13), GlobalSymbol(NonEmptyList("T"))) => (loc11, arg11, arg12, arg13) }
                            x2 <- types1.collectFirst { case GlobalTypeApp(loc12, Seq(), GlobalSymbol(NonEmptyList("U"))) => loc12 }
                            x3 <- types1.collectFirst { case GlobalTypeApp(loc13, Seq(arg14), GlobalSymbol(NonEmptyList("V"))) => (loc13, arg14) }
                          } yield (x1, x2, x3)) {
                            case Some(((loc11, arg11, arg12, arg13), loc12, (loc13, arg14))) =>
                              loc11 should be ===(tLoc)
                              loc12 should be ===(uLoc)
                              loc13 should be ===(vLoc)
                              inside(arg11) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Char, Seq())) => () }
                              inside(arg12) {
                                case TypeValueLambda(Seq(), TypeParamApp(param12, Seq(), 0)) =>
                                  inside(arg13) {
                                    case TypeValueLambda(Seq(), TypeParamApp(param13, Seq(), 0)) =>
                                      List(param12, param13).toSet should have size(2)
                                  }
                              }
                              inside(arg14) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Boolean, Seq())) => () }
                          }
                      }
                      inside(retType1) { case BuiltinType(TypeBuiltinFunction.Char, Seq()) => () }
                      inside(argKinds) {
                        case Seq(
                          InferredKind(Star(KindType, _)) /* * */,
                          InferredKind(Star(KindType, _)) /* * */) =>
                            ()
                      }
                  }
                  inside(enval.lambdaInfosFromEnvironment(env2)(Some(GlobalSymbol(NonEmptyList("f")))).get(0)) {
                    case Some(InferenceLambdaInfo(TypeTable(types), None, combTypeParams)) =>
                      types should have size(1)
                      inside(types.get(LocalSymbol("x"))) {
                        case Some(InferredType(TypeDisjunction(types1), argKinds)) =>
                          types1 should have size(3)
                          inside(for {
                            x1 <- types1.collectFirst { case GlobalTypeApp(loc11, Seq(arg11, arg12, arg13), GlobalSymbol(NonEmptyList("T"))) => (loc11, arg11, arg12, arg13) }
                            x2 <- types1.collectFirst { case GlobalTypeApp(loc12, Seq(), GlobalSymbol(NonEmptyList("U"))) => loc12 }
                            x3 <- types1.collectFirst { case GlobalTypeApp(loc13, Seq(arg14), GlobalSymbol(NonEmptyList("V"))) => (loc13, arg14) }
                          } yield (x1, x2, x3)) {
                            case Some(((loc11, arg11, arg12, arg13), loc12, (loc13, arg14))) =>
                              loc11 should be ===(tLoc)
                              loc12 should be ===(uLoc)
                              loc13 should be ===(vLoc)
                              inside(arg11) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Char, Seq())) => () }
                              inside(arg12) {
                                case TypeValueLambda(Seq(), TypeParamApp(param12, Seq(), 0)) =>
                                  inside(arg13) {
                                    case TypeValueLambda(Seq(), TypeParamApp(param13, Seq(), 0)) =>
                                      List(param12, param13).toSet should have size(2)
                                  }
                              }
                              inside(arg14) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Boolean, Seq())) => () }
                          }
                          inside(argKinds) {
                            case Seq(InferredKind(Star(KindType, _)), InferredKind(Star(KindType, _))) =>
                              ()
                          }
                      }
                      combTypeParams should have size(2)
                      combTypeParams.keySet should be ===(Set(0, 1))
                      combTypeParams.values.toSet should be ===(Set(0, 1))
                  }
                  inside(enval.lambdaInfosFromEnvironment(env2)(Some(GlobalSymbol(NonEmptyList("f")))).get(1)) {
                    case Some(InferenceLambdaInfo(TypeTable(types), polyFunType, combTypeParams)) =>
                      types should have size(0)
                      inside(polyFunType) {
                        case Some(InferredType(TypeDisjunction(types1), argKinds)) =>
                          types1 should have size(3)
                          inside(for {
                            x1 <- types1.collectFirst { case GlobalTypeApp(loc11, Seq(arg11, arg12, arg13), GlobalSymbol(NonEmptyList("T"))) => (loc11, arg11, arg12, arg13) }
                            x2 <- types1.collectFirst { case GlobalTypeApp(loc12, Seq(), GlobalSymbol(NonEmptyList("U"))) => loc12 }
                            x3 <- types1.collectFirst { case GlobalTypeApp(loc13, Seq(arg14), GlobalSymbol(NonEmptyList("V"))) => (loc13, arg14) }
                          } yield (x1, x2, x3)) {
                            case Some(((loc11, arg11, arg12, arg13), loc12, (loc13, arg14))) =>
                              loc11 should be ===(tLoc)
                              loc12 should be ===(uLoc)
                              loc13 should be ===(vLoc)
                              inside(arg11) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Char, Seq())) => () }
                              inside(arg12) {
                                case TypeValueLambda(Seq(), TypeParamApp(param12, Seq(), 0)) =>
                                  inside(arg13) {
                                    case TypeValueLambda(Seq(), TypeParamApp(param13, Seq(), 0)) =>
                                      List(param12, param13).toSet should have size(2)
                                  }
                              }
                              inside(arg14) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Boolean, Seq())) => () }
                          }
                          inside(argKinds) {
                            case Seq(InferredKind(Star(KindType, _)), InferredKind(Star(KindType, _))) =>
                              ()
                          }
                      }
                      combTypeParams should have size(0)
                  }
                  inside(enval.lambdaInfosFromEnvironment(env2)(Some(GlobalSymbol(NonEmptyList("f")))).get(2)) {
                    case Some(InferenceLambdaInfo(TypeTable(types), None, combTypeParams)) =>
                      types should have size(0)
                      combTypeParams should have size(0)
                  }
                  inside(enval.lambdaInfosFromEnvironment(env2)(Some(GlobalSymbol(NonEmptyList("f")))).get(3)) {
                    case Some(InferenceLambdaInfo(TypeTable(types), polyFunType, combTypeParams)) =>
                      types should have size(1)
                      inside(types.get(LocalSymbol("y"))) {
                        case Some(InferredType(GlobalTypeApp(loc1, Seq(type1, type2, type3), GlobalSymbol(NonEmptyList("T"))), argKinds)) =>
                          loc1 should be ===(tLoc)
                          inside(type1) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Char, Seq())) => () }
                          inside(type2) { 
                            case TypeValueLambda(Seq(), TypeParamApp(param2, Seq(), 0)) =>
                              inside(type3) {
                                case TypeValueLambda(Seq(), TypeParamApp(param3, Seq(), 0)) =>
                                  List(param2, param3).toSet should have size(2)
                              }
                          }
                          inside(argKinds) {
                            case Seq(InferredKind(Star(KindType, _)), InferredKind(Star(KindType, _))) =>
                              ()
                          }
                      }
                      inside(polyFunType) {
                        case Some(InferredType(GlobalTypeApp(loc1, Seq(type1, type2, type3), GlobalSymbol(NonEmptyList("T"))), argKinds)) =>
                          loc1 should be ===(tLoc)
                          inside(type1) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Char, Seq())) => () }
                          inside(type2) { 
                            case TypeValueLambda(Seq(), TypeParamApp(param2, Seq(), 0)) =>
                              inside(type3) {
                                case TypeValueLambda(Seq(), TypeParamApp(param3, Seq(), 0)) =>
                                  List(param2, param3).toSet should have size(2)
                              }
                          }
                          inside(argKinds) {
                            case Seq(InferredKind(Star(KindType, _)), InferredKind(Star(KindType, _))) =>
                              ()
                          }
                      }
                      combTypeParams should have size(0)
                  }
                  inside(enval.lambdaInfosFromEnvironment(env2)(Some(GlobalSymbol(NonEmptyList("f")))).get(4)) {
                    case Some(InferenceLambdaInfo(TypeTable(types), polyFunType, combTypeParams)) =>
                      types should have size(1)
                      inside(types.get(LocalSymbol("y"))) {
                        case Some(InferredType(GlobalTypeApp(loc1, Seq(), GlobalSymbol(NonEmptyList("U"))), Seq())) =>
                          loc1 should be ===(uLoc)
                      }
                      inside(polyFunType) {
                        case Some(InferredType(GlobalTypeApp(loc1, Seq(), GlobalSymbol(NonEmptyList("U"))), argKinds)) =>
                          loc1 should be ===(uLoc)
                          inside(argKinds) {
                            case Seq(InferredKind(Star(KindType, _)), InferredKind(Star(KindType, _))) =>
                              ()
                          }
                      }
                      combTypeParams should have size(0)
                  }
                  inside(enval.lambdaInfosFromEnvironment(env2)(Some(GlobalSymbol(NonEmptyList("f")))).get(5)) {
                    case Some(InferenceLambdaInfo(TypeTable(types), polyFunType, combTypeParams)) =>
                      types should have size(1)
                      inside(types.get(LocalSymbol("y"))) {
                        case Some(InferredType(GlobalTypeApp(loc1, Seq(type1), GlobalSymbol(NonEmptyList("V"))), Seq())) =>
                          loc1 should be ===(vLoc)
                          inside(type1) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Boolean, Seq())) => () }
                      }
                      inside(polyFunType) {
                        case Some(InferredType(GlobalTypeApp(loc1, Seq(type1), GlobalSymbol(NonEmptyList("V"))), argKinds)) =>
                          loc1 should be ===(vLoc)
                          inside(type1) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Boolean, Seq())) => () }
                          inside(argKinds) {
                            case Seq(InferredKind(Star(KindType, _)), InferredKind(Star(KindType, _))) =>
                              ()
                          }
                      }
                      combTypeParams should have size(0)
                  }
              }
          }
      }
    }
    
    it should "infer the type from the string with the extract-expression" in {
      val (env, res) = Typer.inferTypesFromTreeString("""
f x = x extract {
    y1 y2 y3 => #fAdd y1 (#floatFromDouble y2)
  }
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("f")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), argKinds) =>
          // \t1 => (#Float, #Double, t1) #-> #Float
          inside(argType1) { case TupleType(Seq(BuiltinType(TypeBuiltinFunction.Float, Seq()), BuiltinType(TypeBuiltinFunction.Double, Seq()), TypeParamApp(_, Seq(), 0))) => () }
          inside(retType1) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
          inside(argKinds) {
            case Seq(
                InferredKind(Star(KindType, _)) /* * */) =>
              ()
          }
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("f")))).get(0)) {
        case Some(InferenceLambdaInfo(TypeTable(types), None, combTypeParams)) =>
          types should have size(1)
          inside(types.get(LocalSymbol("x"))) {
            case Some(InferredType(TupleType(Seq(BuiltinType(TypeBuiltinFunction.Float, Seq()), BuiltinType(TypeBuiltinFunction.Double, Seq()), TypeParamApp(_, Seq(), 0))), argKinds)) =>
              inside(argKinds) {
                case Seq(InferredKind(Star(KindType, _))) =>
                  ()
              }
          }
          combTypeParams should have size(1)
          combTypeParams.keySet should be ===(Set(0))
          combTypeParams.values.toSet should be ===(Set(0))
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("f")))).get(1)) {
        case Some(InferenceLambdaInfo(TypeTable(types), polyFunType, combTypeParams)) =>
          types should have size(3)
          inside(types.get(LocalSymbol("y1"))) {
            case Some(InferredType(BuiltinType(TypeBuiltinFunction.Float, Seq()), Seq())) =>
              ()
          }
          inside(types.get(LocalSymbol("y2"))) {
            case Some(InferredType(BuiltinType(TypeBuiltinFunction.Double, Seq()), Seq())) =>
              ()
          }
          inside(types.get(LocalSymbol("y3"))) {
            case Some(InferredType(TypeParamApp(_, Seq(), 0), argKinds)) =>
              inside(argKinds) {
                case Seq(InferredKind(Star(KindType, _))) =>
                  ()
              }
          }
          inside(polyFunType) {
            case Some(InferredType(TupleType(Seq(BuiltinType(TypeBuiltinFunction.Float, Seq()), BuiltinType(TypeBuiltinFunction.Double, Seq()), TypeParamApp(_, Seq(), 0))), argKinds)) =>
              inside(argKinds) {
                case Seq(InferredKind(Star(KindType, _))) =>
                  ()
              }
          }
          combTypeParams should have size(0)
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("f")))).get(2)) {
        case Some(InferenceLambdaInfo(TypeTable(types), None, combTypeParams)) =>
          types should have size(0)
          combTypeParams should have size(0)
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("f")))).get(3)) {
        case Some(InferenceLambdaInfo(TypeTable(types), None, combTypeParams)) =>
          types should have size(0)
          combTypeParams should have size(0)
      }
    }
    
    it should "infer the type for the recursive function that is the lambda-expression" in {
      val (env, res) = Typer.inferTypesFromTreeString("""
f = \x y => #cond (\_ => f x false) (\_ => x) y 
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("f")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), argKinds) =>
          // \t1 => t1 #-> #Boolean #-> t1
          inside(argType1) {
            case TypeParamApp(param1, Seq(), 0) =>
              inside(retType1) {
                case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType2, retType2)) =>
                  inside(argType2) { case BuiltinType(TypeBuiltinFunction.Boolean, Seq()) => () }
                  inside(retType2) {
                    case TypeParamApp(param2, Seq(), 0) =>
                      List(param1, param2).toSet should have size(1)
                  }
              } 
          }
          inside(argKinds) {
            case Seq(
                InferredKind(Star(KindType, _)) /* * */) =>
              ()
          }
      }
    }
    
    it should "infer the type for the defined type of the combinator" in {
      val (env, res) = Typer.inferTypesFromTreeString("""
(f: \t1 t2 => ##-> #Float (##-> (##-> t1 t2) #Double)) x y = #doubleFromFloat x
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("f")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), argKinds) =>
          // \t1 t2 => #Float #-> (t1 #-> t2) #-> #Double
          inside(argType1) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
          inside(retType1) {
            case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType2, retType2)) =>
              inside(argType2) {
                case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType21, retType21)) =>
                  inside(argType21) { 
                    case TypeParamApp(param21, Seq(), 0) =>
                      inside(retType21) {
                        case TypeParamApp(param22, Seq(), 0) =>
                          List(param21, param22).toSet should have size(2)
                      }
                  }
              }
              inside(retType2) { case BuiltinType(TypeBuiltinFunction.Double, Seq()) => () }
          }
          inside(argKinds) {
            case Seq(
                InferredKind(Star(KindType, _)) /* * */,
                InferredKind(Star(KindType, _)) /* * */) =>
              ()
          }
      }
    }

    it should "infer the type for the defineds type of the arguments" in {
      val (env, res) = Typer.inferTypesFromTreeString("""
f (g: ##-> #Int #Int) x (y: \(t1: * -> * -> *) => (\t2 t3 => t1 t2 t3)) = tuple 2 (g x) y
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("f")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), argKinds) =>
          // \(t1: * -> * -> *) (t2: *) (t3: *) => (#Int #-> #-> #Int) #-> #Int #-> (t1 t2 t3) #-> (#Int, t1 t2 t3)
          inside(argType1) {
            case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType11, retType11)) =>
              inside(argType11) { case BuiltinType(TypeBuiltinFunction.Int, Seq()) => () }
              inside(retType11) { case BuiltinType(TypeBuiltinFunction.Int, Seq()) => () }
          }
          inside(retType1) {
            case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType2, retType2)) =>
              inside(argType2) { case BuiltinType(TypeBuiltinFunction.Int, Seq()) => () }
              inside(retType2) {
                case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType3, retType3)) =>
                  inside(argType3) {
                    case TypeParamApp(param31, Seq(arg31, arg32), 0) =>
                      inside(arg31) { 
                        case TypeValueLambda(Seq(), TypeParamApp(param311, Seq(), 0)) => 
                          inside(arg32) {
                            case TypeValueLambda(Seq(), TypeParamApp(param321, Seq(), 0)) =>
                              inside(retType3) { 
                                case TupleType(Seq(BuiltinType(TypeBuiltinFunction.Int, Seq()), TypeParamApp(param33, Seq(arg33, arg34), 0))) =>
                                  inside(arg33) {
                                    case TypeValueLambda(Seq(), TypeParamApp(param331, Seq(), 0)) =>
                                      inside(arg34) {
                                        case TypeValueLambda(Seq(), TypeParamApp(param341, Seq(), 0)) =>
                                          List(param31, param33).toSet should have size(1)
                                          List(param311, param331).toSet should have size(1)
                                          List(param321, param341).toSet should have size(1)
                                          List(param31, param311, param321, param33, param331, param341).toSet should have size(3)
                                          inside(argKinds.lift(param31)) {
                                            case Some(InferredKind(Arrow(Star(KindType, _), Arrow(Star(KindType, _), Star(KindType, _), _), _))) =>
                                              // * -> * -> *
                                              ()
                                          }
                                          inside(argKinds.lift(param311)) {
                                            case Some(InferredKind(Star(KindType, _))) =>
                                              // *
                                              ()
                                          }
                                          inside(argKinds.lift(param321)) {
                                            case Some(InferredKind(Star(KindType, _))) =>
                                              // *
                                              ()
                                          }
                                      }
                                  }
                              }
                          }
                      }
                  }
              }
          }
      }
    }
    
    it should "infer the type for the defined type of the expression" in {
      val (env, res) = Typer.inferTypesFromTreeString("""
f g x y = (g: \t1 => ##-> #Float (##-> t1 #Double)) x y
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("f")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), argKinds) =>
          // \t1 => (#Float #-> t1 #-> #Double) #-> #Float #-> t1 #-> #Double
          inside(argType1) {
            case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType11, retType11)) =>
              inside(argType11) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
              inside(retType11) {
                case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType12, retType12)) =>
                  inside(argType12) {
                    case TypeParamApp(param12, Seq(), 0) =>
                      inside(retType12) { case BuiltinType(TypeBuiltinFunction.Double, Seq()) => () }
                      inside(retType1) {
                        case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType2, retType2)) =>
                          inside(argType2) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
                          inside(retType2) {
                            case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType3, retType3)) =>
                              inside(argType3) {
                                case TypeParamApp(param3, Seq(), 0) =>
                                  inside(retType3) { case BuiltinType(TypeBuiltinFunction.Double, Seq()) => () }
                                  List(param12, param3).toSet should have size(1)
                              }
                          }
                      }
                  }
              }
          }
          inside(argKinds) {
            case Seq(
                InferredKind(Star(KindType, _)) /* * */) =>
              ()
          }
      }
    }
    
    it should "unify the two built-in types" in {
      // Unifies \t1 t2 => #Array (t1 #-> t2 #-> #Float)
      // with    \t1 t2 => #Array (#Double #-> t1 #-> t2).
      val (env, res) = Typer.inferTypesFromTreeString("""
f = construct 0: \t1 t2 => #Array (##-> t1 (##-> t2 #Float))
g (x: \t1 t2 => #Array (##-> #Double (##-> t1 t2))) = x
h = g f
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("f")))) {
        case InferredType(BuiltinType(_, _), Seq(_, _)) =>
          ()
      }
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("g")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq(_, _)) =>
          ()
      }
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("h")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Array, Seq(type1)), argKinds) =>
          // \t1 => #Array (#Double #-> t1 #-> #Float)
          inside(type1) {
            case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType11, retType11)) =>
              inside(argType11) { case BuiltinType(TypeBuiltinFunction.Double, Seq()) => () }
              inside(retType11) { 
                case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType12, retType12)) =>
                  inside(argType12) { case TypeParamApp(_, Seq(), 0) => () }
                  inside(retType12) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
              }
          }
          inside(argKinds) {
            case Seq(
                InferredKind(Star(KindType, _)) /* * */) =>
              ()
          }
      }
    }
    
    it should "unify the two unit types" in {
      // Unifies T #Boolean #Char
      // with    U #Boolean #Char
      // for type T t1 t2 = V t1 t2 and type U t1 t2 = V t1 t2 and unittype 2 V.
      val s = """
type T t1 t2 = V t1 t2
type U t1 t2 = V t1 t2
unittype 2 V
f = construct 0: T #Boolean #Char
g (x: U #Boolean #Char) = x
h = g f
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              res2 should be ===(().success.success)
              inside(typeGlobalSymTabular.getGlobalLocationFromTable(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo).treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("U")))) {
                case Some(uLoc) =>
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("f")))) {
                    case InferredType(GlobalTypeApp(_, Seq(_, _), _), Seq()) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("g")))) {
                    case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq()) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("h")))) {
                    case InferredType(GlobalTypeApp(loc1, Seq(arg1, arg2), GlobalSymbol(NonEmptyList("U"))), Seq()) =>
                      // U #Boolean #Char
                      loc1 should be ===(uLoc)
                      inside(arg1) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Boolean, Seq())) => () }
                      inside(arg2) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Char, Seq())) => () }
                  }
              }
          }
      }
    }
    
    it should "unify the global type application with the type that isn't global type application" in {
      // Unifies \t1 => T #Array t1 #Int
      // with    \t1 => (#Array #Byte, #Short, t1) for (type T t1 t2 t3 = (t1 #Byte, t2, t3).
      val s = """
type T t1 t2 t3 = tuple 3 (t1 #Byte) t2 t3
f = construct 0: \t1 => T #Array t1 #Int
g (x: \t1 => tuple 3 (#Array #Byte) #Short t1) = x
h = g f
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              res2 should be ===(().success.success)
              inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("f")))) {
                case InferredType(GlobalTypeApp(_, Seq(_, _, _), _), Seq(_)) =>
                  ()
              }
              inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("g")))) {
                case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq(_)) =>
                  ()
              }
              inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("h")))) {
                case InferredType(TupleType(Seq(type1, type2, type3)), Seq()) =>
                  // (#Array #Byte, #Short, #Int)
                  inside(type1) { case BuiltinType(TypeBuiltinFunction.Array, Seq(BuiltinType(TypeBuiltinFunction.Byte, Seq()))) => () }
                  inside(type2) { case BuiltinType(TypeBuiltinFunction.Short, Seq()) => () }
                  inside(type3) { case BuiltinType(TypeBuiltinFunction.Int, Seq()) => () }
              }
          }
      }
    }
    
    it should "unify the two global type applications which have the same symbol" in {
      // Unifies \t1 t2 => T (t1 #-> #Int) #Float t2 t2
      // with    \t1 t2 => T t1 #Float (#Double #-> t2) t1.
      val s = """
type T t1 t2 t3 t4 = tuple 4 t1 t2 t3 t4
f = construct 0: \t1 t2 => T (##-> t1 #Int) #Float t2 t2
g (x: \t1 t2 => T t1 #Float (##-> #Double t2) t1) = x
h = g f
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              res2 should be ===(().success.success)
              inside(typeGlobalSymTabular.getGlobalLocationFromTable(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo).treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("T")))) {
                case Some(tLoc) =>
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("f")))) {
                    case InferredType(GlobalTypeApp(_, Seq(_, _, _, _), _), Seq(_, _)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("g")))) {
                    case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq(_, _)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("h")))) {
                    case InferredType(GlobalTypeApp(loc1, Seq(arg1, arg2, arg3, arg4), GlobalSymbol(NonEmptyList("T"))), Seq()) =>
                      // T (#Double #-> #Int) #Float (#Double #-> #Int) (#Double #-> #Int)
                      loc1 should be ===(tLoc)
                      inside(arg1) {
                        case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Fun, Seq(argType11, retType11))) =>
                          inside(argType11) { case BuiltinType(TypeBuiltinFunction.Double, Seq()) => () }
                          inside(retType11) { case BuiltinType(TypeBuiltinFunction.Int, Seq()) => () }
                      }
                      inside(arg2) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Float, Seq())) => () }
                      inside(arg3) {
                        case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Fun, Seq(argType31, retType31))) =>
                          inside(argType31) { case BuiltinType(TypeBuiltinFunction.Double, Seq()) => () }
                          inside(retType31) { case BuiltinType(TypeBuiltinFunction.Int, Seq()) => () }
                      }
                      inside(arg4) {
                        case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Fun, Seq(argType41, retType41))) =>
                          inside(argType41) { case BuiltinType(TypeBuiltinFunction.Double, Seq()) => () }
                          inside(retType41) { case BuiltinType(TypeBuiltinFunction.Int, Seq()) => () }
                      }
                  }
              }
          }
      }
    }
    
    it should "unify the two global type applications which have the different symbols" in {
      // Unifies \t1 => T #Int #Long t1
      // with    \t1 => U t1 #Long #Float
      // for type T t1 t2 t3 = (t1, t2, t3) and type U t1 t2 t3 = (t1, t2, t3).
      val s = """
type T t1 t2 t3 = tuple 3 t1 t2 t3
type U t1 t2 t3 = tuple 3 t1 t2 t3
f = construct 0: \t1 => T #Int #Long t1
g (x: \t1 => U t1 #Long #Float) = x
h = g f
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              res2 should be ===(().success.success)
              inside(typeGlobalSymTabular.getGlobalLocationFromTable(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo).treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("U")))) {
                case Some(uLoc) =>
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("f")))) {
                    case InferredType(GlobalTypeApp(_, Seq(_, _, _), _), Seq(_)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("g")))) {
                    case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq(_)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("h")))) {
                    case InferredType(GlobalTypeApp(loc1, Seq(arg1, arg2, arg3), GlobalSymbol(NonEmptyList("U"))), Seq()) =>
                      // U #Int #Long #Float
                      loc1 should be ===(uLoc)
                      inside(arg1) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Int, Seq())) => () }
                      inside(arg2) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Long, Seq())) => () }
                      inside(arg3) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Float, Seq())) => () }
                  }
              }
          }
      }
    }
    
    it should "unify the two global type applications which have the unequal number of the arguments" in {
      // Unifies \t1 => T t1 #Float #Double
      // with    \t1 => U #Float t1
      // for type T t1 t2 t3 = (t1 t2, t3) and type U t1 t2 = (t2 t1, #Double).
val s = """
type T t1 t2 t3 = tuple 2 (t1 t2) t3
type U t1 t2 = tuple 2 (t2 t1) #Double
f = construct 0: \t1 => T t1 #Float #Double
g (x: \t1 => U #Float t1) = x
h = g f
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              res2 should be ===(().success.success)
              inside(typeGlobalSymTabular.getGlobalLocationFromTable(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo).treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("U")))) {
                case Some(uLoc) =>
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("f")))) {
                    case InferredType(GlobalTypeApp(_, Seq(_, _, _), _), Seq(_)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("g")))) {
                    case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq(_)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("h")))) {
                    case InferredType(GlobalTypeApp(loc1, Seq(arg1, arg2), GlobalSymbol(NonEmptyList("U"))), argKinds) =>
                      // \(t1: * -> *) => U #Float t1
                      loc1 should be ===(uLoc)
                      inside(arg1) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Float, Seq())) => () }
                      inside(arg2) { case TypeValueLambda(Seq(), TypeParamApp(_, Seq(), 0)) => () }
                      inside(argKinds) {
                        case Seq(
                            InferredKind(Arrow(Star(KindType, _), Star(KindType, _), _)) /* * -> * */) =>
                          ()
                      }
                  }
              }
          }
      }
    }

    it should "unify the two global type applications which are recursive" in {
      // Unifies \t1 => V t1
      // with    W
      // for unittype 1 T and unittype 2 U and type V t1 = (T t1) #| (U t1 (V t1))
      // and type W = (T #Char) #| (U #Char (V #Char)).
val s = """
unittype 1 T
unittype 2 U
type V t1 = ##| (T t1) (U t1 (V t1))
type W = ##| (T #Char) (U #Char (V #Char))
f = construct 0: \t1 => V t1
g (x: W) = x
h = g f
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              res2 should be ===(().success.success)
              inside(typeGlobalSymTabular.getGlobalLocationFromTable(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo).treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("W")))) {
                case Some(wLoc) =>
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("f")))) {
                    case InferredType(GlobalTypeApp(_, Seq(_), _), Seq(_)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("g")))) {
                    case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq()) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("h")))) {
                    case InferredType(GlobalTypeApp(loc1, Seq(), GlobalSymbol(NonEmptyList("W"))), Seq()) =>
                      // W
                      loc1 should be ===(wLoc)
                  }
              }
          }
      }
    }
    
    it should "unify the two global type applications for the partial type application" in {
      // Unifies T #Boolean #Char
      // with    U #Boolean #Char for type T t1 t2 t3 = (t1, t2, t3).
      val s = """
type T t1 t2 t3 = tuple 3 t1 t2 t3
f = construct 0: T #Boolean #Char
g (x: T #Boolean #Char) = x
h = g f
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              res2 should be ===(().success.success)
              inside(typeGlobalSymTabular.getGlobalLocationFromTable(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo).treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("T")))) {
                case Some(tLoc) =>
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("f")))) {
                    case InferredType(GlobalTypeApp(_, Seq(_, _, _), _), Seq(_)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("g")))) {
                    case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq(_)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("h")))) {
                    case InferredType(GlobalTypeApp(loc1, Seq(arg1, arg2, arg3), GlobalSymbol(NonEmptyList("T"))), argKinds) =>
                      // \t1 => T #Boolean #Char t1
                      inside(arg1) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Boolean, Seq())) => () }
                      inside(arg2) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Char, Seq())) => () }
                      inside(arg3) { case TypeValueLambda(Seq(), TypeParamApp(_, Seq(), 0)) => () }
                      inside(argKinds) {
                        case Seq(
                            InferredKind(Star(KindType, _)) /* * */) =>
                          ()
                      }
                  }
              }
          }
      }
    }
    
    it should "unify the type parameters" in {
      // Unifies \t1 => t1 with itself.
      val (env, res) = Typer.inferTypesFromTreeString("""
f x = x
g x = f x
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("f")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq(_)) =>
          ()
      }
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
    }
    
    it should "unify the type parameter with the other type" in {
      val (env, res) = Typer.inferTypesFromTreeString("""
f (x: \t1 => #Array t1) = x
g x = f x
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("f")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq(_)) =>
          ()
      }
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("g")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), argKinds) =>
          // \t1 => #Array t1 #-> #Array t1
          inside(argType1) {
            case BuiltinType(TypeBuiltinFunction.Array, Seq(type1)) =>
              inside(type1) {
                case TypeParamApp(param11, Seq(), 0) =>
                  inside(retType1) {
                    case BuiltinType(TypeBuiltinFunction.Array, Seq(type2)) =>
                      inside(type2) {
                        case TypeParamApp(param21, Seq(), 0) =>
                          List(param11, param21).toSet should have size(1)
                      }
                  }
              }
          }
          inside(argKinds) {
            case Seq(
                InferredKind(Star(KindType, _)) /* * */) =>
              ()
          }
      }
    }

    it should "unify the type parameter application with the type global type application" in {
      // Unifies \t1 => T #Char t1 #Double
      // with    \t1 t2 => t1 #Float t2 for type T t1 t2 t3 = (t1, t2, t3).
      val s = """
type T t1 t2 t3 = tuple 3 t1 t2 t3
f = construct 0: \t1 => T #Char t1 #Double
g (x: \t1 t2 => t1 #Float t2) = x
h = g f
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              res2 should be ===(().success.success)
              inside(typeGlobalSymTabular.getGlobalLocationFromTable(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo).treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("T")))) {
                case Some(tLoc) =>
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("f")))) {
                    case InferredType(GlobalTypeApp(_, Seq(_, _, _), _), Seq(_)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("g")))) {
                    case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq(_, _)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("h")))) {
                    case InferredType(GlobalTypeApp(tLoc, Seq(arg1, arg2, arg3), GlobalSymbol(NonEmptyList("T"))), Seq()) =>
                      // T #Char #Float #Double
                      inside(arg1) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Char, Seq())) => () }
                      inside(arg2) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Float, Seq())) => () }
                      inside(arg3) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Double, Seq())) => () }
                  }
              }
          }
      }
    }
    
    it should "unify the type parameter applications which have the equal numbers of the arguments" in {
      // Unifies \t1 t2 => t1 t2 (t2 #-> #Char) #Double
      // with    \t1 t2 t3 => t1 #Boolean (t2 #-> t3) #Double.
      val (env, res) = Typer.inferTypesFromTreeString("""
f = construct 0: \t1 t2 => t1 t2 (##-> t2 #Char) #Double
g (x: \t1 t2 t3 => t1 #Boolean (##-> t2 t3) #Double) = x
h = g f
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("f")))) {
        case InferredType(TypeParamApp(_, Seq(_, _, _), _), Seq(_, _)) =>
          ()
      }
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("g")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq(_, _, _)) =>
          ()
      }
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("h")))) {
        case InferredType(TypeParamApp(_, Seq(arg1, arg2, arg3), 0), argKinds) =>
          // \t1 => t1 #Boolean (#Boolean #-> #Char) #Double
          inside(arg1) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Boolean, Seq())) => () }
          inside(arg2) {
            case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Fun, Seq(argType21, retType21))) =>
              inside(argType21) { case BuiltinType(TypeBuiltinFunction.Boolean, Seq()) => () }
              inside(retType21) { case BuiltinType(TypeBuiltinFunction.Char, Seq()) => () }
          }
          inside(arg3) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Double, Seq())) => () }
          inside(argKinds)  {
            case Seq(
                InferredKind(Arrow(Star(KindType, _), Arrow(Star(KindType, _), Arrow(Star(KindType, _), Star(KindType, _), _), _), _)) /* * -> (* -> *) -> * -> * */) =>
              ()
          }
      }
    }
    
    it should "unify the type parameter application which have the unequal numbers of the arguments" in {
      // Unifies \t1 t2 t3 => t1 (t2 #-> #Char) #Byte t3
      // with    \t1 t2 => t2 #Byte t1.
      val (env, res) = Typer.inferTypesFromTreeString("""
f = construct 0: \t1 t2 t3 => t1 (##-> t2 #Char) #Byte t3
g (x: \t1 t2 => t2 #Byte t1) = x
h = g f
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("f")))) {
        case InferredType(TypeParamApp(_, Seq(_, _, _), _), Seq(_, _, _)) =>
          ()
      }
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("g")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq(_, _)) =>
          ()
      }
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("h")))) {
        case InferredType(TypeParamApp(param1, Seq(arg1, arg2, arg3), 0), argKinds) =>
          // \t1 t2 t3 => t1 (t2 #-> #Char) #Byte t3
          inside(arg1) {
            case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Fun, Seq(argType11, retType11))) =>
              inside(argType11) {
                case TypeParamApp(param11, Seq(), 0) =>
                  inside(retType11) { case BuiltinType(TypeBuiltinFunction.Char, Seq()) => () }
                  inside(arg2) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Byte, Seq())) => () }
                  inside(arg3) {
                    case TypeValueLambda(Seq(), TypeParamApp(param3, Seq(), 0)) =>
                      List(param1, param11, param3).toSet should have size(3)
                      argKinds should have size(3)
                      inside(argKinds.lift(param1)) {
                        case Some(InferredKind(Arrow(Star(KindType, _), ret1, _))) =>
                          // * -> * -> k1 -> *
                          inside(ret1) {
                            case Arrow(Star(KindType, _), ret2, _) =>
                              inside(ret2) { case Arrow(Star(KindParam(_), _), Star(KindType, _), _) => () }
                          }
                      }
                      inside(argKinds.lift(param11)) {
                        case Some(InferredKind(Star(KindType, _))) =>
                          // *
                          ()
                      }
                      inside(argKinds.lift(param3)) {
                        case Some(InferredKind(Star(KindParam(_), _))) =>
                          // k1
                          ()
                      }
                  }
              }
          }
      }
    }
    
    it should "unify the two type lambda-expressions which have the equal number of the arguments" in {
      // Unifies \t1 => t1 (\t2 t3 => t2 #-> #Char #-> (t3, #Float)) 
      // with    \t1 t2 => t1 (\t3 t4 => t3 #-> t2 #-> (t4, #Float)).
      val (env, res) = Typer.inferTypesFromTreeString("""
f = construct 0: \t1 => t1 (\t2 t3 => ##-> t2 (##-> #Char (tuple 2 t3 #Float)))
g (x: \t1 t2 => t1 (\t3 t4 => ##-> t3 (##-> t2 (tuple 2 t4 #Float)))) = x
h = g f
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("f")))) {
        case InferredType(TypeParamApp(_, Seq(_), _), Seq(_)) =>
          ()
      }
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("g")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq(_, _)) =>
          ()
      }
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("h")))) {
        case InferredType(TypeParamApp(param1, Seq(arg1), 0), argKinds) =>
          // \t1 => t1 (\t2 t3 => t2 #-> #Char #-> (t3, #Float))
          inside(arg1) {
            case TypeValueLambda(Seq(param11, param12), BuiltinType(TypeBuiltinFunction.Fun, Seq(argType11, retType11))) =>
              inside(argType11) {
                case TypeParamApp(param111, Seq(), 0) =>
                  inside(retType11) {
                    case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType12, retType12)) =>
                      inside(argType12) { case BuiltinType(TypeBuiltinFunction.Char, Seq()) => () }
                      inside(retType12) {
                        case TupleType(Seq(TypeParamApp(param121, Seq(), 0), BuiltinType(TypeBuiltinFunction.Float, Seq()))) =>
                          List(param11, param111).toSet should have size(1)
                          List(param12, param121).toSet should have size(1)
                          List(param1, param11, param12, param111, param121).toSet should have size(3)
                      }
                  }
              }
          }
          inside(argKinds) {
            case Seq(
                InferredKind(Arrow(Arrow(Star(KindType, _), Arrow(Star(KindType, _), Star(KindType, _), _), _), Star(KindType, _), _)) /* * -> * -> * */) =>
              ()
          }
      }
    }
    
    it should "unify the two type lambda-expressions which have the unequal number of the arguments" in {
      // Unifies T (\t1 t2 t3 => U t2 t1 t3)
      // with    T (\t1 t2 => U t2 t1).
      val s = """
type T t1 = tuple 2 #Char (t1 #Int #Long #Float)
type U t1 t2 t3 = tuple 3 t1 t2 t3
f = construct 0: T (\t1 t2 t3 => U t2 t1 t3)
g (x: T (\t1 t2 => U t2 t1)) = x
h = g f
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              res2 should be ===(().success.success)
              val syms = List(GlobalSymbol(NonEmptyList("T")), GlobalSymbol(NonEmptyList("U")))
              inside(syms.flatMap(typeGlobalSymTabular.getGlobalLocationFromTable(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo).treeInfo.treeInfo))) {
                case List(tLoc, uLoc) =>
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("f")))) {
                    case InferredType(GlobalTypeApp(_, Seq(_), _), Seq()) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("g")))) {
                    case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq()) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("h")))) {
                    case InferredType(GlobalTypeApp(loc1, Seq(arg1), GlobalSymbol(NonEmptyList("T"))), Seq()) =>
                      // T (\t1 t2 => U t2 t1)
                      loc1 should be ===(tLoc)
                      inside(arg1) {
                        case TypeValueLambda(Seq(param11, param12), GlobalTypeApp(loc11, Seq(arg11, arg12), GlobalSymbol(NonEmptyList("U")))) =>
                          loc11 should be ===(uLoc)
                          inside(arg11) { 
                            case TypeValueLambda(Seq(), TypeParamApp(param111, Seq(), 0)) =>
                              inside(arg12) {
                                case TypeValueLambda(Seq(), TypeParamApp(param121, Seq(), 0)) =>
                                  List(param11, param121).toSet should have size(1)
                                  List(param12, param111).toSet should have size(1)
                                  List(param11, param12, param111, param121).toSet should have size(2)
                              }
                          }
                      }
                  }
              }
          }
      }
    }
    
    it should "unify the two type conjunctions" in {
      // Unifies \t1 t2 => TWI ((T t1 t2) #& U #& (V (#Int #-> t2)))
      // with    \t1 => TWI ((T #Boolean t1) #& U #& (V (#Int #-> #Char)))
      // for unittype 1 TWI and unittype 2 T and unittype 0 U and unittype 1 V.
      val s = """
unittype 1 TWI
unittype 2 T
unittype 0 U
unittype 1 V
f = construct 0: \t1 t2 => TWI (##& (##& (T t1 t2) U) (V (##-> #Int t2)))
g (x: \t1 => TWI (##& (##& (T #Boolean t1) U) (V (##-> #Int #Char)))) = x
h = g f
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              res2 should be ===(().success.success)
              val syms = List(
                  GlobalSymbol(NonEmptyList("TWI")),
                  GlobalSymbol(NonEmptyList("T")),
                  GlobalSymbol(NonEmptyList("U")),
                  GlobalSymbol(NonEmptyList("V")))
              inside(syms.flatMap(typeGlobalSymTabular.getGlobalLocationFromTable(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo).treeInfo.treeInfo))) {
                case List(twiLoc, tLoc, uLoc, vLoc) =>
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("f")))) {
                    case InferredType(GlobalTypeApp(_, Seq(_), _), Seq(_, _)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("g")))) {
                    case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq(_)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("h")))) {
                    case InferredType(GlobalTypeApp(loc1, Seq(TypeValueLambda(Seq(), TypeConjunction(types1))), GlobalSymbol(NonEmptyList("TWI"))), Seq()) =>
                      // TWI ((T #Boolean #Char) #& U #& (V (#Int #-> #Char)))
                      loc1 should be ===(twiLoc)
                      types1 should have size(3)
                      inside(for {
                        x1 <- types1.collectFirst { case GlobalTypeApp(loc11, Seq(arg11, arg12), GlobalSymbol(NonEmptyList("T"))) => (loc11, arg11, arg12) }
                        x2 <- types1.collectFirst { case GlobalTypeApp(loc12, Seq(), GlobalSymbol(NonEmptyList("U"))) => loc12 }
                        x3 <- types1.collectFirst { case GlobalTypeApp(loc13, Seq(arg13), GlobalSymbol(NonEmptyList("V"))) => (loc13, arg13) }
                      } yield (x1, x2, x3)) {
                        case Some(((loc11, arg11, arg12), loc12, (loc13, arg13))) =>
                          loc11 should be ===(tLoc)
                          loc12 should be ===(uLoc)
                          loc13 should be ===(vLoc)
                          inside(arg11) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Boolean, Seq())) => () }
                          inside(arg12) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Char, Seq())) => () }
                          inside(arg13) {
                            case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Fun, Seq(argType131, retType131))) =>
                              inside(argType131) { case BuiltinType(TypeBuiltinFunction.Int, Seq()) => () }
                              inside(retType131) { case BuiltinType(TypeBuiltinFunction.Char, Seq()) => () }
                          }
                      }
                  }
              }
          }
      }
    }
    
    it should "unify the two type disjunctions" in {
      // Unifies \t1 t2 t3 => TWI ((T #Boolean t1) #| U #| (V t2 t3))
      // with    \t1 t2 => TWI ((T t2 t1) #| U #| (V t1 t2))
      // for unittype 1 TWI and unittype 2 T and unittype 0 U and unittype 2 V.
      val s = """
unittype 1 TWI
unittype 2 T
unittype 0 U
unittype 2 V
f = construct 0: \t1 t2 t3 => TWI (##| (##| (T #Boolean t1) U) (V t2 t3))
g (x: \t1 t2 => TWI (##| (##| (T t2 t1) U) (V t1 t2))) = x
h = g f
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              res2 should be ===(().success.success)
              val syms = List(
                  GlobalSymbol(NonEmptyList("TWI")),
                  GlobalSymbol(NonEmptyList("T")),
                  GlobalSymbol(NonEmptyList("U")),
                  GlobalSymbol(NonEmptyList("V")))
              inside(syms.flatMap(typeGlobalSymTabular.getGlobalLocationFromTable(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo).treeInfo.treeInfo))) {
                case List(twiLoc, tLoc, uLoc, vLoc) =>
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("f")))) {
                    case InferredType(GlobalTypeApp(_, Seq(_), _), Seq(_, _, _)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("g")))) {
                    case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq(_, _)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("h")))) {
                    case InferredType(GlobalTypeApp(loc1, Seq(TypeValueLambda(Seq(), TypeDisjunction(types1))), GlobalSymbol(NonEmptyList("TWI"))), argKinds) =>
                      // \t1 => TWI ((T #Boolean t1) #| U #| (V t1 #Boolean))
                      loc1 should be ===(twiLoc)
                      types1 should have size(3)
                      inside(for {
                        x1 <- types1.collectFirst { case GlobalTypeApp(loc11, Seq(arg11, arg12), GlobalSymbol(NonEmptyList("T"))) => (loc11, arg11, arg12) }
                        x2 <- types1.collectFirst { case GlobalTypeApp(loc12, Seq(), GlobalSymbol(NonEmptyList("U"))) => loc12 }
                        x3 <- types1.collectFirst { case GlobalTypeApp(loc13, Seq(arg13, arg14), GlobalSymbol(NonEmptyList("V"))) => (loc13, arg13, arg14) }
                      } yield (x1, x2, x3)) {
                        case Some(((loc11, arg11, arg12), loc12, (loc13, arg13, arg14))) =>
                          loc11 should be ===(tLoc)
                          loc12 should be ===(uLoc)
                          loc13 should be ===(vLoc)
                          inside(arg11) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Boolean, Seq())) => () }
                          inside(arg12) {
                            case TypeValueLambda(Seq(), TypeParamApp(param12, Seq(), 0)) =>
                              inside(arg13) {
                                case TypeValueLambda(Seq(), TypeParamApp(param13, Seq(), 0)) =>
                                  List(param12, param13).toSet should have size(1)
                              }
                          }
                          inside(arg14) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Boolean, Seq())) => () }
                      }
                      inside(argKinds) {
                        case Seq(
                            InferredKind(Star(KindType, _)) /* * */) =>
                          ()
                      }
                  }
              }
          }
      }
    }
    
    it should "unify the two types which are the same logical expression" in {
      // Unifies \t1 t2 => TWI (((T t1) #& (U t1 t2)) #| (V t2) #| (T t1 #& W #& ((V t2) #| X))) with itself
      // for unittype 1 TWI and unittype 1 T and unittype 2 U and unittype 1 V and unittype 0 W and unittype 0 X.
      val s = """
unittype 1 TWI
unittype 1 T
unittype 2 U
unittype 1 V
unittype 0 W
unittype 0 X
f = construct 0: \t1 t2 => TWI (##| (##| (##& (T t1) (U t1 t2)) (V t2)) (##& (##& (T t1) W) (##| (V t2) X)))
g (x: \t1 t2 => TWI (##| (##| (##& (T t1) (U t1 t2)) (V t2)) (##& (##& (T t1) W) (##| (V t2) X)))) = x
h = g f
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              res2 should be ===(().success.success)
              val syms = List(
                  GlobalSymbol(NonEmptyList("TWI")),
                  GlobalSymbol(NonEmptyList("T")),
                  GlobalSymbol(NonEmptyList("U")),
                  GlobalSymbol(NonEmptyList("V")),
                  GlobalSymbol(NonEmptyList("W")),
                  GlobalSymbol(NonEmptyList("X")))
              inside(syms.flatMap(typeGlobalSymTabular.getGlobalLocationFromTable(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo).treeInfo.treeInfo))) {
                case List(twiLoc, tLoc, uLoc, vLoc, wLoc, xLoc) =>
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("f")))) {
                    case InferredType(GlobalTypeApp(_, Seq(_), _), Seq(_, _)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("g")))) {
                    case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq(_, _)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("h")))) {
                    case InferredType(GlobalTypeApp(loc1, Seq(TypeValueLambda(Seq(), TypeDisjunction(types1))), GlobalSymbol(NonEmptyList("TWI"))), argKinds) =>
                      // \t1 t2 => TWI (((T t1) #& (U t1 t2)) #| (V t2) #| (T t1 #& W #& ((V t2) #| X)))
                      loc1 should be ===(twiLoc)
                      types1 should have size(3)
                      inside(for {
                        x1 <- types1.collectFirst { case TypeConjunction(types11) if types11.size == 2 => types11 }
                        x2 <- types1.collectFirst { case GlobalTypeApp(loc12, Seq(arg12), GlobalSymbol(NonEmptyList("V"))) => (loc12, arg12) }
                        x3 <- types1.collectFirst { case TypeConjunction(types13) if types13.size == 3 => types13 }
                      } yield (x1, x2, x3)) {
                        case Some((types11, (loc12, arg12), types13)) =>
                          loc12 should be ===(vLoc)
                          inside(for {
                            x1 <- types11.collectFirst { case GlobalTypeApp(loc111, Seq(arg111), GlobalSymbol(NonEmptyList("T"))) => (loc111, arg111) }
                            x2 <- types11.collectFirst { case GlobalTypeApp(loc112, Seq(arg112, arg113), GlobalSymbol(NonEmptyList("U"))) => (loc112, arg112, arg113) }
                          } yield (x1, x2)) {
                            case Some(((loc111, arg111), (loc112, arg112, arg113))) =>
                              loc111 should be ===(tLoc)
                              loc112 should be ===(uLoc)
                              inside(arg111) {
                                case TypeValueLambda(Seq(), TypeParamApp(param111, Seq(), 0)) =>
                                  inside(arg112) {
                                    case TypeValueLambda(Seq(), TypeParamApp(param112, Seq(), 0)) =>
                                      inside(arg113) {
                                        case TypeValueLambda(Seq(), TypeParamApp(param113, Seq(), 0)) =>
                                          inside(arg12) {
                                            case TypeValueLambda(Seq(), TypeParamApp(param12, Seq(), 0)) =>
                                              inside(for {
                                                x1 <- types13.collectFirst { case GlobalTypeApp(loc131, Seq(arg131), GlobalSymbol(NonEmptyList("T"))) => (loc131, arg131) }
                                                x2 <- types13.collectFirst { case GlobalTypeApp(loc132, Seq(), GlobalSymbol(NonEmptyList("W"))) => loc132 }
                                                x3 <- types13.collectFirst { case TypeDisjunction(types133) => types133 }
                                              } yield (x1, x2, x3)) {
                                                case Some(((loc131, arg131), loc132, types133)) =>
                                                  loc131 should be ===(tLoc)
                                                  loc132 should be ===(wLoc)
                                                  inside(arg131) {
                                                    case TypeValueLambda(Seq(), TypeParamApp(param131, Seq(), 0)) =>
                                                      types133 should have size(2)
                                                      inside(for {
                                                        x1 <- types133.collectFirst { case GlobalTypeApp(loc1331, Seq(arg1331), GlobalSymbol(NonEmptyList("V"))) => (loc1331, arg1331) }
                                                        x2 <- types133.collectFirst { case GlobalTypeApp(loc1332, Seq(), GlobalSymbol(NonEmptyList("X"))) => loc1332 }
                                                      } yield (x1, x2)) {
                                                        case Some(((loc1331, arg1331), loc1332)) =>
                                                          loc1331 should be ===(vLoc)
                                                          loc1332 should be ===(xLoc)
                                                          inside(arg1331) {
                                                            case TypeValueLambda(Seq(), TypeParamApp(param1331, Seq(), 0)) =>
                                                              List(param111, param112, param131).toSet should have size(1)
                                                              List(param113, param12, param1331).toSet should have size(1)
                                                              List(param111, param112, param113, param12, param131, param1331).toSet should have size(2)
                                                          }
                                                      }
                                                  }
                                              }
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
              }
          }
      }
    }

    it should "unify the two types which are the different logical expressions" in {
      // Unifies \t1 t2 => TWI ((T #& (W #| #Empty)) #| ((U t1) #& W) #| ((U t2) #& #Empty) #| (V #& (W #| #Empty)))
      // with    \t1 => TWI ((T #| (U t1) #| V) #& (W #| #Empty)) 
      // for unittype 1 TWI and unittype 0 T and unittype 1 U and unittype 0 V and unittype 0 W.
      val s = """
unittype 1 TWI
unittype 0 T
unittype 1 U
unittype 0 V
unittype 0 W
f = construct 0: \t1 t2 => TWI (##| (##| (##| (##& T (##| W #Empty)) (##& (U t1) W)) (##& (U t2) #Empty)) (##& V (##| W #Empty)))
g (x: \t1 => TWI (##& (##| (##| T (U t1)) V) (##| W #Empty))) = x
h = g f
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              res2 should be ===(().success.success)
              val syms = List(
                  GlobalSymbol(NonEmptyList("TWI")),
                  GlobalSymbol(NonEmptyList("T")),
                  GlobalSymbol(NonEmptyList("U")),
                  GlobalSymbol(NonEmptyList("V")),
                  GlobalSymbol(NonEmptyList("W")))
              inside(syms.flatMap(typeGlobalSymTabular.getGlobalLocationFromTable(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo).treeInfo.treeInfo))) {
                case List(twiLoc, tLoc, uLoc, vLoc, wLoc) =>
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("f")))) {
                    case InferredType(GlobalTypeApp(_, Seq(_), _), Seq(_, _)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("g")))) {
                    case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq(_)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("h")))) {
                    case InferredType(GlobalTypeApp(loc1, Seq(TypeValueLambda(Seq(), TypeConjunction(types1))), GlobalSymbol(NonEmptyList("TWI"))), argKinds) =>
                      // \t1 => TWI ((T #| (U t1) #| V) #& (W #| #Empty))
                      loc1 should be ===(twiLoc)
                      types1.size should be ===(2)
                      inside(for {
                        x1 <- types1.collectFirst { case TypeDisjunction(types11) if types11.size == 3 => types11 }
                        x2 <- types1.collectFirst { case TypeDisjunction(types12) if types12.size == 2 => types12 }
                      } yield (x1, x2)) {
                        case Some((types11, types12)) =>
                          inside(for {
                            x1 <- types11.collectFirst { case GlobalTypeApp(loc111, Seq(), GlobalSymbol(NonEmptyList("T"))) => loc111 }
                            x2 <- types11.collectFirst { case GlobalTypeApp(loc112, Seq(arg112), GlobalSymbol(NonEmptyList("U"))) => (loc112, arg112) }
                            x3 <- types11.collectFirst { case GlobalTypeApp(loc113, Seq(), GlobalSymbol(NonEmptyList("V"))) => loc113 }
                          } yield (x1, x2, x3)) {
                            case Some((loc111, (loc112, arg112), loc113)) =>
                              loc111 should be ===(tLoc)
                              loc112 should be ===(uLoc)
                              loc113 should be ===(vLoc)
                              inside(arg112) { case TypeValueLambda(Seq(), TypeParamApp(_, Seq(), 0)) => () }
                          }
                          inside(for {
                            x1 <- types12.collectFirst { case GlobalTypeApp(loc121, Seq(), GlobalSymbol(NonEmptyList("W"))) => loc121 }
                            _ <- types12.collectFirst { case BuiltinType(TypeBuiltinFunction.Empty, Seq()) => () }
                          } yield x1) {
                            case Some(loc121) =>
                              loc121 should be ===(wLoc)
                          }
                      }
                      inside(argKinds) {
                        case Seq(
                            InferredKind(Star(KindType, _)) /* * */) =>
                          ()
                      }
                  }
              }
          }
      }
    }
    
    it should "unify the two types for the first interation of the unification that has the errors" in {
      // Unifies \t1 t2 => T (V t2) (t1 t2 #Int) (t1 t2 #Char)
      // with    \t1 t2 => T (t1 t2 #Int) (U t2 #Int) (t1 t2 #Char)
      // for unittype 3 T and type U t1 t2 = V t1 and unittype 1 V.
      val s = """
unittype 3 T
type U t1 t2 = V t1
unittype 1 V
f = construct 0: \t1 t2 => T (V t2) (t1 t2 #Int) (t1 t2 #Char)
g (x: \t1 t2 => T (t1 t2 #Int) (U t2 #Int) (t1 t2 #Char)) = x
h = g f
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              res2 should be ===(().success.success)
              val syms = List(GlobalSymbol(NonEmptyList("T")), GlobalSymbol(NonEmptyList("U")))
              inside(syms.flatMap(typeGlobalSymTabular.getGlobalLocationFromTable(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo).treeInfo.treeInfo))) {
                case List(tLoc, uLoc) =>
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("f")))) {
                    case InferredType(GlobalTypeApp(_, Seq(_, _, _), GlobalSymbol(NonEmptyList("T"))), Seq(_, _)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("g")))) {
                    case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq(_, _)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("h")))) {
                    case InferredType(GlobalTypeApp(loc1, Seq(arg1, arg2, arg3), GlobalSymbol(NonEmptyList("T"))), argKinds)=>
                      // \t1 => T (U t1 #Int) (U t1 #Int) (U t1 #Char)
                      loc1 should be ===(tLoc)
                      inside(arg1) {
                        case TypeValueLambda(Seq(), GlobalTypeApp(loc11, Seq(arg11, arg12), GlobalSymbol(NonEmptyList("U")))) =>
                          loc11 should be ===(uLoc)
                          inside(arg11) {
                            case TypeValueLambda(Seq(), TypeParamApp(param11, Seq(), 0)) =>
                              inside(arg12) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Int, Seq())) => () }
                              inside(arg2) {
                                case TypeValueLambda(Seq(), GlobalTypeApp(loc21, Seq(arg21, arg22), GlobalSymbol(NonEmptyList("U")))) =>
                                  loc21 should be ===(uLoc)
                                  inside(arg21) {
                                    case TypeValueLambda(Seq(), TypeParamApp(param21, Seq(), 0)) =>
                                      inside(arg22) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Int, Seq())) => () }
                                      inside(arg3) {
                                        case TypeValueLambda(Seq(), GlobalTypeApp(loc31, Seq(arg31, arg32), GlobalSymbol(NonEmptyList("U")))) =>
                                          loc31 should be ===(uLoc)
                                          inside(arg31) {
                                            case TypeValueLambda(Seq(), TypeParamApp(param31, Seq(), 0)) =>
                                              inside(arg32) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Char, Seq())) => () }
                                              List(param11, param21, param31).toSet should have size(1)
                                          }
                                      }
                                  }
                              }
                          }
                      }
                      inside(argKinds) {
                        case Seq(
                            InferredKind(Star(KindType, _)) /* * */) =>
                          ()
                      }
                  }
              }
          }
      }
    }
    
    it should "unify the two types which are the logical expression with the type parameters" in {
      // Unifies \t1 t2 => TWI (t1 #& (t2 #Char) #& T)
      // with    \t1 t2 t3 => #Array (t1 #& (t2 t3) #& T)
      // for unittype 1 TWI and unittype 0 T.
      val s = """
unittype 1 TWI
unittype 0 T
f = construct 0: \t1 t2 => TWI (##& (##& t1 (t2 #Char)) T)
g (x: \t1 t2 t3 => TWI (##& (##& t1 (t2 t3)) T)) = x
h = g f
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              res2 should be ===(().success.success)
              val syms = List(GlobalSymbol(NonEmptyList("TWI")), GlobalSymbol(NonEmptyList("T")))
              inside(syms.flatMap(typeGlobalSymTabular.getGlobalLocationFromTable(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo).treeInfo.treeInfo))) {
                case List(twiLoc, tLoc) =>
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("f")))) {
                    case InferredType(GlobalTypeApp(_, Seq(_), _), Seq(_, _)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("g")))) {
                    case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq(_, _, _)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("h")))) {
                    case InferredType(GlobalTypeApp(loc1, Seq(TypeValueLambda(Seq(), TypeConjunction(types1))), GlobalSymbol(NonEmptyList("TWI"))), argKinds) =>
                      // \t1 t2 t3 => TWI (t1 #& (t2 #Char) #& T)
                      loc1 should be ===(twiLoc)
                      types1 should have size(3)
                      inside(for {
                        x1 <- types1.collectFirst { case TypeParamApp(param11, Seq(), 0) => param11 }
                        x2 <- types1.collectFirst { case TypeParamApp(param12, Seq(arg12), 0 ) => (param12, arg12) }
                        x3 <- types1.collectFirst { case GlobalTypeApp(loc13, Seq(), GlobalSymbol(NonEmptyList("T"))) => loc13 }
                      } yield (x1, x2, x3)) {
                        case Some((param11, (param12, arg12), loc13)) =>
                          loc13 should be ===(tLoc)
                          inside(arg12) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Char, Seq())) => () }
                          List(param11, param12).toSet should have size(2)
                          argKinds should have size(2)
                          inside(argKinds.lift(param11)) {
                            case Some(InferredKind(Star(KindType, _))) =>
                              // *
                              ()
                          }
                          inside(argKinds.lift(param12)) {
                            case Some(InferredKind(Arrow(Star(KindType, _), Star(KindType, _), _))) =>
                              // * -> *
                              ()
                          }
                      }
                  }
              }
          }
      }
    }
    
    it should "unify the supertype with the type for the Any type" in {
      // Unifies \t1 => T t1 #Char 
      // with    #Any for unittype 2 T.
      val s = """
unittype 2 T
f = construct 0: \t1 => T t1 #Char
g (x: #Any) = x
h = g f
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              res2 should be ===(().success.success)
              inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("f")))) {
                case InferredType(GlobalTypeApp(_, Seq(_, _), _), Seq(_)) =>
                  ()
              }
              inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("g")))) {
                case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq()) =>
                  ()
              }
              inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("h")))) {
                case InferredType(BuiltinType(TypeBuiltinFunction.Any, Seq()), Seq()) =>
                  // #Any
              }
          }
      }
    }
    
    it should "unify the supertype with the type for the Nothing type" in {
      // Unifies #Nothing
      // with    \t1 => t1 #-> #Float.
      val (env, res) = Typer.inferTypesFromTreeString("""
f = construct 0: #Nothing
g (x: \t1 => ##-> t1 #Float) = x
h = g f
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("f")))) {
        case InferredType(BuiltinType(_, Seq()), Seq()) =>
          ()
      }
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("g")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq(_)) =>
          ()
      }
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("h")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), argKinds) =>
          // \t1 => t1 #-> #Float
          inside(argType1) { case TypeParamApp(_, Seq(), 0) => () }
          inside(retType1) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
          inside(argKinds) {
            case Seq(
                InferredKind(Star(KindType, _)) /* * */) =>
              ()
          }
      }
    }
    
    it should "unify the supertype with the type for the similar logical expressions" in {
      // Unifies \t1 => ((T t1) #& U) #| (V #& (W #| X))
      // with    \t1 t2 => (T t1) #| (V #& (W #| X #| #Empty)) #| (Y t1 t2)
      // for unittype 1 T and unittype 0 U and unittype 0 V and unittype 0 W and unittype 0 X and unittype 2 Y.
      val s = """
unittype 1 T
unittype 0 U
unittype 0 V
unittype 0 W
unittype 0 X
unittype 2 Y
f = construct 0: \t1 => ##| (##& (T t1) U) (##& V (##| W  X))
g (x: \t1 t2 => ##| (##| (T t1) (##& V (##| (##| W X) #Empty))) (Y t1 t2)) = x
h = g f
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              res2 should be ===(().success.success)
              val syms = List(
                  GlobalSymbol(NonEmptyList("T")),
                  GlobalSymbol(NonEmptyList("V")),
                  GlobalSymbol(NonEmptyList("W")),
                  GlobalSymbol(NonEmptyList("X")),
                  GlobalSymbol(NonEmptyList("Y")))
              inside(syms.flatMap(typeGlobalSymTabular.getGlobalLocationFromTable(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo).treeInfo.treeInfo))) {
                case List(tLoc, vLoc, wLoc, xLoc, yLoc) =>
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("f")))) {
                    case InferredType(TypeDisjunction(types1), Seq(_)) =>
                      types1 should have size(2)
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("g")))) {
                    case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq(_, _)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("h")))) {
                    case InferredType(TypeDisjunction(types1), argKinds) =>
                      // (T t1) #| (V #& (W #| X #| #Empty)) #| (Y t1 t2)
                      types1 should have size(3)
                      inside(for {
                        x1 <- types1.collectFirst { case GlobalTypeApp(loc11, Seq(arg11), GlobalSymbol(NonEmptyList("T"))) => (loc11, arg11) }
                        x2 <- types1.collectFirst { case TypeConjunction(types12) => types12 }
                        x3 <- types1.collectFirst { case GlobalTypeApp(loc13, Seq(arg13, arg14), GlobalSymbol(NonEmptyList("Y"))) => (loc13, arg13, arg14) }
                      } yield (x1, x2, x3)) {
                        case Some(((loc11, arg11), types12, (loc13, arg13, arg14))) =>
                          loc11 should be ===(tLoc)
                          loc13 should be ===(yLoc)
                          inside(arg11) {
                            case TypeValueLambda(Seq(), TypeParamApp(param11, Seq(), 0)) =>
                              types12 should have size(2)
                              inside(for {
                                x1 <- types12.collectFirst { case GlobalTypeApp(loc121, Seq(), GlobalSymbol(NonEmptyList("V"))) => loc121 }
                                x2 <- types12.collectFirst { case TypeDisjunction(types122) => types122 }
                              } yield (x1, x2)) {
                                case Some((loc121, types122)) =>
                                  loc121 should be ===(vLoc)
                                  types122 should have size(3)
                                  inside(for {
                                    x1 <- types122.collectFirst { case GlobalTypeApp(loc1221, Seq(), GlobalSymbol(NonEmptyList("W"))) => loc1221 }
                                    x2 <- types122.collectFirst { case GlobalTypeApp(loc1222, Seq(), GlobalSymbol(NonEmptyList("X"))) => loc1222 }
                                    _ <- types122.collectFirst { case BuiltinType(TypeBuiltinFunction.Empty, Seq()) => () }
                                  } yield (x1, x2)) {
                                    case Some((loc1221, loc1222)) =>
                                      loc1221 should be ===(wLoc)
                                      loc1222 should be ===(xLoc)
                                  }
                                  inside(arg13) {
                                    case TypeValueLambda(Seq(), TypeParamApp(param13, Seq(), 0)) =>
                                      inside(arg14) {
                                        case TypeValueLambda(Seq(), TypeParamApp(param14, Seq(), 0)) =>
                                          List(param11, param13).toSet should have size(1)
                                          List(param11, param13, param14).toSet should have size(2)
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
              }
          }
      }
    }

    it should "unify the supertype with the type for the different logical expressions" in {
      // Unifies \t1 => (T t1 #& U) #| (V #& (W t1) #& X)
      // with    \t1 => (V #| (T t1) #| #Empty) #& ((W t1) #| (T t1))
      // for unittype 1 T and unittype 0 U and unittype 0 V and unittype 1 W and unittype 0 X.
      val s = """
unittype 1 T
unittype 0 U
unittype 0 V
unittype 1 W
unittype 0 X
f = construct 0: \t1 => ##| (##& (T t1) U) (##& (##& V (W t1)) X)
g (x: \t1 => ##& (##| (##| V (T t1)) #Empty) (##| (W t1) (T t1))) = x
h = g f
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              res2 should be ===(().success.success)
              val syms = List(
                  GlobalSymbol(NonEmptyList("T")),
                  GlobalSymbol(NonEmptyList("V")),
                  GlobalSymbol(NonEmptyList("W")))
              inside(syms.flatMap(typeGlobalSymTabular.getGlobalLocationFromTable(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo).treeInfo.treeInfo))) {
                case List(tLoc, vLoc, wLoc) =>
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("f")))) {
                    case InferredType(TypeDisjunction(types1), Seq(_)) =>
                      types1 should have size(2)
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("g")))) {
                    case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq(_)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("h")))) {
                    case InferredType(TypeConjunction(types1), argKinds) =>
                      // \t1 => (V #| (T t1) #| #Empty) #& ((W t1) #| (T t1))
                      types1 should have size(2)
                      inside(for {
                        x1 <- types1.collectFirst { case TypeDisjunction(types11) if types11.size == 3 => types11 }
                        x2 <- types1.collectFirst { case TypeDisjunction(types12) if types12.size == 2 => types12 }
                      } yield (x1, x2)) {
                        case Some((types11, types12)) =>
                          inside(for {
                            x1 <- types11.collectFirst { case GlobalTypeApp(loc111, Seq(), GlobalSymbol(NonEmptyList("V"))) => loc111 }
                            x2 <- types11.collectFirst { case GlobalTypeApp(loc112, Seq(arg112), GlobalSymbol(NonEmptyList("T"))) => (loc112, arg112) }
                            _ <- types11.collectFirst { case BuiltinType(TypeBuiltinFunction.Empty, Seq()) => () }
                          } yield (x1, x2)) {
                            case Some((loc111, (loc112, arg112))) =>
                              loc111 should be ===(vLoc)
                              loc112 should be ===(tLoc)
                              inside(arg112) {
                                case TypeValueLambda(Seq(), TypeParamApp(param112, Seq(), 0)) =>
                                  inside(for {
                                    x1 <- types12.collectFirst { case GlobalTypeApp(loc121, Seq(arg121), GlobalSymbol(NonEmptyList("W"))) => (loc121, arg121) }
                                    x2 <- types12.collectFirst { case GlobalTypeApp(loc122, Seq(arg122), GlobalSymbol(NonEmptyList("T"))) => (loc122, arg122) }
                                  } yield (x1, x2)) {
                                    case Some(((loc121, arg121), (loc122, arg122))) =>
                                      loc121 should be ===(wLoc)
                                      loc122 should be ===(tLoc)
                                      inside(arg121) {
                                        case TypeValueLambda(Seq(), TypeParamApp(param121, Seq(), 0)) =>
                                          inside(arg122) {
                                            case TypeValueLambda(Seq(), TypeParamApp(param122, Seq(), 0)) =>
                                              List(param112, param121, param122).toSet should have size(1)
                                          }
                                      }
                                  }
                              }
                          }
                      }
                      inside(argKinds) {
                        case Seq(
                            InferredKind(Star(KindType, _)) /* * */) =>
                          ()
                      }
                  }
              }
          }
      }
    }
    
    it should "unify the supertype with the type for the logical expressions with the type parameters" in {
      // Unifies \t1 t2 t3 => t1 #& t2 #& (t3 #Char) #& T #& U
      // with    \t1 t2 => t1 #& (t2 #Char) #& T
      // for unittype 0 T and unittype 0 U.
      val s = """
unittype 0 T
unittype 0 U
f = construct 0: \t1 t2 t3 => ##& (##& (##& (##& t1 t2) (t3 #Char)) T) U
g (x: \t1 t2 => ##& (##& t1 (t2 #Char)) T) = x
h = g f
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              res2 should be ===(().success.success)
              inside(typeGlobalSymTabular.getGlobalLocationFromTable(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo).treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("T")))) {
                case Some(tLoc) =>
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("f")))) {
                    case InferredType(TypeConjunction(types1), Seq(_, _, _)) =>
                      types1 should have size(5)
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("g")))) {
                    case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(_, _)), Seq(_, _)) =>
                      ()
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("h")))) {
                    case InferredType(TypeConjunction(types1), argKinds) =>
                      // \t1 t2 => t #& (t2 #Char) #& T
                      types1 should have size(3)
                      inside(for { 
                        x1 <- types1.collectFirst { case TypeParamApp(param11, Seq(), 0) => param11 }
                        x2 <- types1.collectFirst { case TypeParamApp(param12, Seq(arg12), 0) => (param12, arg12) }
                        x3 <- types1.collectFirst { case GlobalTypeApp(loc13, Seq(), GlobalSymbol(NonEmptyList("T"))) => loc13 }
                      } yield (x1, x2, x3)) {
                        case Some((param11, (param12, arg12), loc13)) =>
                          loc13 should be ===(tLoc)
                          inside(arg12) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Char, Seq())) => () }
                          List(param11, param12).toSet should have size(2)
                          argKinds should have size(2)
                          inside(argKinds.lift(param11)) {
                            case Some(InferredKind(Star(KindType, _))) =>
                              // *
                              ()
                          }
                          inside(argKinds.lift(param12)) {
                            case Some(InferredKind(Arrow(Star(KindType, _), Star(KindType, _), _))) =>
                              // * -> *
                              ()
                          }
                      }
                  }
              }
          }
      }
    }
    
    it should "normalize the type applications after the evaluation of the defined types" in {
      val s = """
type T t1 t2 t3 t4 = tuple 3 t1 t2 t3 
(f: T #Boolean #Char) = construct 0
g = tuple 2 (construct 0: \(t1: (k1 -> k2) -> k1 -> *) t2 => t1 t2) true
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              res2 should be ===(().success.success)
              inside(typeGlobalSymTabular.getGlobalLocationFromTable(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo).treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("T")))) {
                case Some(tLoc) =>
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("f")))) {
                    case InferredType(GlobalTypeApp(loc1, args1, GlobalSymbol(NonEmptyList("T"))), argKinds) =>
                      // \t1 t2 => T #Boolean #Char t1 t2
                      loc1 should be ===(tLoc)
                      inside(args1) {
                        case Seq(arg11, arg12, arg13, arg14) =>
                          inside(arg11) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Boolean, Seq())) => () }
                          inside(arg12) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Char, Seq())) => () }
                          inside(arg13) {
                            case TypeValueLambda(Seq(), TypeParamApp(param13, Seq(), 0)) =>
                              inside(arg14) {
                                case TypeValueLambda(Seq(), TypeParamApp(param14, Seq(), 0)) =>
                                  List(param13, param14).toSet should have size(2)
                                  argKinds should have size(2)
                                  inside(argKinds.lift(param13)) {
                                    case Some(InferredKind(Star(KindType, _))) =>
                                      // *
                                      ()
                                  }
                                  inside(argKinds.lift(param14)) {
                                    case Some(InferredKind(Star(KindParam(_), _))) =>
                                      // k1
                                      ()
                                  }
                              }
                          }
                      }
                  }
                  inside(enval.globalVarTypeFromEnvironment(env2)(GlobalSymbol(NonEmptyList("g")))) {
                    case InferredType(TupleType(Seq(type1, type2)), argKinds) =>
                      // \(t1: (k1 -> k2) -> k1 -> *) t2 t3 => (t1 t2 t3, #Boolean)
                      inside(type1) {
                        case TypeParamApp(param1, args1, 0) =>
                          inside(args1) {
                            case Seq(arg11, arg12) =>
                              inside(arg11) {
                                case TypeValueLambda(Seq(), TypeParamApp(param11, Seq(), 0)) =>
                                  inside(arg12) {
                                    case TypeValueLambda(Seq(), TypeParamApp(param12, Seq(), 0)) =>
                                      List(param1, param11, param12).toSet should have size(3)
                                      argKinds should have size(3)
                                      inside(argKinds.lift(param1)) {
                                        case Some(InferredKind(Arrow(arg1, ret1, _))) =>
                                          // (k1 -> k2) -> k1 -> *
                                          inside(arg1) {
                                            case Arrow(Star(KindParam(kindParam11), _), Star(KindParam(kindParam12), _), _) =>
                                              inside(ret1) {
                                                case Arrow(Star(KindParam(kindParam13), _), Star(KindType, _), _) =>
                                                  List(kindParam11, kindParam13).toSet should have size(1)
                                                  List(kindParam11, kindParam12, kindParam13).toSet should have size(2)
                                              }
                                          }
                                      }
                                      inside(argKinds.lift(param11)) {
                                        case Some(InferredKind(Arrow(Star(KindParam(kindParam1), _), Star(KindParam(kindParam2), _), _))) =>
                                          // k1 -> k2
                                          List(kindParam1, kindParam2).toSet should have size(2)
                                      }
                                      inside(argKinds.lift(param12)) {
                                        case Some(InferredKind(Star(KindParam(_), _))) =>
                                          // k1
                                          ()
                                      }
                                  }
                              }
                          }
                      }
                      inside(type2) { case BuiltinType(TypeBuiltinFunction.Boolean, Seq()) => () }
                  }
              }
          }
      }
    }
    
    it should "complain on the mismatched types" in {
      val (env, res) = Typer.inferTypesFromTreeString("""
f x y = tuple 2 (x y) y
g x y = #zXor (f x y) y
h x y = #cond 1 f true
""")(NameTree.empty)(f).run(emptyEnv)
      inside(res) {
        case Success(Failure(noType)) =>
          noType.errs.map { _.msg } should be ===(List(
              "couldn't match type #Boolean with type \\(t1: *) (t2: *) => (t1, t2)",
              "couldn't match type \\(t1: *) => () #-> t1 with type #NonZero #& #Int",
              "couldn't match type \\(t1: *) => () #-> t1 with type \\(t1: *) (t2: *) => (t1 #-> t2) #-> t1 #-> (t2, t1)"))
      }
    }
    
    it should "complain on the infinity types" in {
      val (env, res) = Typer.inferTypesFromTreeString("""
f x y z = tuple 3 (x y) (z x) (z y)
""")(NameTree.empty)(f).run(emptyEnv)
     inside(res) {
        case Success(Failure(noType)) =>
          noType.errs.map { _.msg } should be ===(List(
              "couldn't match type \\(t1: *) (t2: *) => t1 #-> t2 with type \\(t1: *) => t1"))
      }
    }
    
    it should "complain on the instantiation of the parameters of the defined types" in {
      val (env, res) = Typer.inferTypesFromTreeString("""
f (g: \t1 => ##-> #Float (##-> t1 #Float)) = g 0.1f 'a'  
""")(NameTree.empty)(f).run(emptyEnv)
      inside(res) {
        case Success(Failure(noType)) =>
          noType.errs.map { _.msg } should be ===(List(
              "couldn't instantiate parameter at defined type \\t1 => #Float #-> t1 #-> #Float"))
      }
    }
    
    it should "complain on the distinct parameters in the defined types" in {
      val (env, res) = Typer.inferTypesFromTreeString("""
f (g: \t1 t2 t3 => ##-> t1 (##-> t3 #Char)) (h: \t1 t2 t3 => ##-> t1 (##-> t2 (##-> t3 #Char))) x y z = tuple 3 (g x y) (h x y x) (g x x)  
""")(NameTree.empty)(f).run(emptyEnv)
      inside(res) {
        case Success(Failure(noType)) =>
          noType.errs.map { _.msg } should be ===(List(
              "parameters are distinct at defined type \\t1 t2 t3 => t1 #-> t2 #-> t3 #-> #Char",
              "parameters are distinct at defined type \\t1 _ t2 => t1 #-> t2 #-> #Char"))
      }
    }
    
    it should "complain on the supertype and the type which were matched as the types" in {
      // Unifies \t1 => TWI (T #& (U t1) #& V)
      // with    \t1 => TWI (T #& (U t1))
      // for unittype 1 TWI and unittype 0 T and unittype 1 U and unittype 0 V.
      val s = """
unittype 1 TWI
unittype 0 T
unittype 1 U
unittype 0 V
f = construct 0: \t1 => TWI (##& (##& T (U t1)) V)
g (x: \t1 => TWI (##& T (U t1))) = x
h = g f
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              inside(res2) {
                case Success(Failure(noType)) =>
                  noType.errs.map { _.msg } should be ===(List(
                      "couldn't match type \\(t1: *) => #.TWI (#.T #& (#.U t1)) with type \\(t1: *) => #.TWI (#.T #& (#.U t1) #& #.V)"))
              }
          }
      }
    }
    
    it should "complain on the type and the supertype which were matched as the supertype and the type" in {
      // Unifies \t1 t2 => (T t1) #| (U #& (V t2)) #| W
      // with    \t1 => U #& (V t1)
      // for unittype 1 T and unittype 0 U and unittype 1 V and unittype 0 W.
      val s = """
unittype 1 T
unittype 0 U
unittype 1 V
unittype 0 W
f = construct 0: \t1 t2 => ##| (##| (T t1) (##& U (V t2))) W
g (x: \t1 => ##& U (V t1)) = x
h = g f
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          inside(makeData(s)) {
            case Success(data) =>
              val kindTable = kindTableFromData(data)
              val (typeEnv, res) = Typer.interpretTypeTreeFromTreeS(tree)(emptyTypeEnv)
              res should be ===(().success)
              val (_, env) = g3(kindTable, InferredTypeTable.empty).run(typeEnv)
              val (env2, res2) = Typer.inferTypesFromTreeString(s)(NameTree.empty)(f).run(env)
              inside(res2) {
                case Success(Failure(noType)) =>
                  noType.errs.map { _.msg } should be ===(List(
                      "couldn't match type \\(t1: *) => #.U #& (#.V t1) with type \\(t1: *) (t2: *) => (#.T t1) #| #.U #& (#.V t2) #| #.W"))
              }
          }
      }
    }
    
    it should "complain on the lambda argument that was matched with the other lambda argument" in {
      // Unifies \t1 => t1 (\t2 t3 => (t2, t3, t2))
      // with    \t1 => t1 (\t2 t3 => (t2, t3, t3)).
      val (env, res) = Typer.inferTypesFromTreeString("""
f = construct 0: \t1 => t1 (\t2 t3 => tuple 3 t2 t3 t2)
g (x: \t1 => t1 (\t2 t3 => tuple 3 t2 t3 t3)) = x
h = g f
""")(NameTree.empty)(f).run(emptyEnv)
      inside(res) {
        case Success(Failure(noType)) =>
          noType.errs.map { _.msg } should be ===(List(
              "couldn't match type \\(t1: (* -> * -> *) -> *) => t1 (\\t2 t3 => (t2, t3, t3)) with type \\(t1: (* -> * -> *) -> *) => t1 (\\t2 t3 => (t2, t3, t2))"))
      }
    }

    it should "complain on the lambda argument that was matched with the other paremeter" in {
      // Unifies \t1 t2 => t1 (\t3 => (t2, t3, t2))
      // with    \t1 t2 => t1 (\t3 => (t2, t3, t3)).
      val (env, res) = Typer.inferTypesFromTreeString("""
f = construct 0: \t1 t2 => t1 (\t3 => tuple 3 t2 t3 t2)
g (x: \t1 t2 => t1 (\t3 => tuple 3 t2 t3 t3)) = x
h = g f
""")(NameTree.empty)(f).run(emptyEnv)
      inside(res) {
        case Success(Failure(noType)) =>
          noType.errs.map { _.msg } should be ===(List(
              "couldn't match type \\(t1: (* -> *) -> *) (t2: *) => t1 (\\t3 => (t2, t3, t3)) with type \\(t1: (* -> *) -> *) (t2: *) => t1 (\\t3 => (t2, t3, t2))"))
      }
    }
    
    it should "complain on the mismatched types with the mismatched kinds" in {
      // Unifies \t1 (t2: k1 -> *) => t1 t2
      // with    \t1 (t2: *) => t1 t2.
      val (env, res) = Typer.inferTypesFromTreeString("""
f = construct 0: \t1 (t2: k1 -> *) => t1 t2
g (x: \t1 (t2: *) => t1 t2) = x
h = g f
""")(NameTree.empty)(f).run(emptyEnv)
      inside(res) {
        case Success(Failure(noType)) =>
          noType.errs.map { _.msg } should be ===(List(
              "couldn't match kind * -> * with kind (k1 -> *) -> *"))
      }
    }
    
    it should "infer the types from the string with the integer numbers" in {
      val (env, res) = Typer.inferTypesFromTreeString("""
f = 2
g = 0
h x = #iAdd (#iAdd 0 10) x
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      // f
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("f")))) {
        case InferredType(TypeConjunction(types1), Seq()) =>
          // #NonZero #& #Int
          types1 should have size(2)
          inside(for {
            _ <- types1.collectFirst { case BuiltinType(TypeBuiltinFunction.NonZero, Seq()) => () }
            _ <- types1.collectFirst { case BuiltinType(TypeBuiltinFunction.Int, Seq()) => () }
          } yield ()) {
            case Some(_) =>
              ()
          }
       }
       // g
       inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("g")))) {
         case InferredType(TypeConjunction(types1), argKinds) =>
           // #Zero #& #Int
           types1 should have size(2)
           inside(for {
             _ <- types1.collectFirst { case BuiltinType(TypeBuiltinFunction.Zero, Seq()) => () }
             _ <- types1.collectFirst { case BuiltinType(TypeBuiltinFunction.Int, Seq()) => () }
           } yield ()) {
             case Some(_) =>
               ()
           }
       }
       // h
       inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("h")))) {
         case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), Seq()) =>
           // ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int)
           inside(argType1) {
             case TypeConjunction(types1) =>
               types1 should have size(2)
               inside(for { 
                 x1 <- types1.collectFirst { case TypeDisjunction(types11) => types11 }
                 _ <- types1.collectFirst { case BuiltinType(TypeBuiltinFunction.Int, Seq()) => () }
               } yield x1) {
                 case Some(types11) =>
                   types11 should have size(2)
                   inside(for {
                     _ <- types11.collectFirst { case BuiltinType(TypeBuiltinFunction.Zero, Seq()) => () }
                     _ <- types11.collectFirst { case BuiltinType(TypeBuiltinFunction.NonZero, Seq()) => () }
                   } yield ()) {
                     case Some(_) =>
                       ()
                   }
              }
           }
           inside(retType1) {
             case TypeConjunction(types1) =>
               types1 should have size(2)
               inside(for { 
                 x1 <- types1.collectFirst { case TypeDisjunction(types11) => types11 }
                 _ <- types1.collectFirst { case BuiltinType(TypeBuiltinFunction.Int, Seq()) => () }
               } yield x1) {
                 case Some(types11) =>
                  types11 should have size(2)
                    inside(for {
                    _ <- types11.collectFirst { case BuiltinType(TypeBuiltinFunction.Zero, Seq()) => () }
                    _ <- types11.collectFirst { case BuiltinType(TypeBuiltinFunction.NonZero, Seq()) => () }
                  } yield ()) {
                    case Some(_) =>
                      ()
                  }
             }
          }
      }
    }
    
    it should "transform the string" in {
      val (typeEnv, res) = Typer.transformString("""
f x = #iAdd x 1
""")(NameTree.empty, kindTableFromData(initData), InferredTypeTable.empty)(f3)(g3).run(emptyTypeEnv)
      inside(res) {
        case Success(Tree(combs, treeInfo)) =>
          val combSyms = Set(GlobalSymbol(NonEmptyList("f")))
          val combLocs = combSyms.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo))
          combLocs should have size(1)
          treeInfo.typeTable.types.keySet should be ===(combLocs)
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))).flatMap(combs.get)) {
            case Some(Combinator(None, args, body, LambdaInfo(lambdaInfo, 0, typeTable, None, combTypeParams), _)) =>
              val syms = Set(LocalSymbol("x"))
              val locs = syms.flatMap(localSymTabular.getLocalLocationFromTable(lambdaInfo))
              locs should have size(1)
              typeTable.types.keySet should be ===(locs)
              inside(args) { case List(Arg(Some("x"), None, _)) => () }
              inside(body) {
                case App(Simple(Literal(BuiltinFunValue(BuiltinFunction.IAdd)), _), args1, _) =>
                  inside(args1) {
                    case NonEmptyList(arg11, arg12) =>
                      inside(arg11) { 
                        case Simple(Var(loc11, LambdaInfo(lambdaInfo11, 1, typeTable11, None, combTypeParams11)), _) =>
                          some(loc11) should be ===(localSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("x")))
                          typeTable11.types should be ('empty)
                          combTypeParams11 should be ('empty)
                      }
                      inside(arg12) { case Simple(Literal(IntValue(1)), _) => () }
                  }
              }
              inside(localSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("x")).flatMap(typeTable.types.get)) {
                case Some(InferredType(TypeConjunction(types1), Seq())) =>
                  // (#Zero #| #NonZero) #& #Int
                  types1 should have size(2)
                  inside(for {
                    x1 <- types1.collectFirst { case TypeDisjunction(types11) => types11 }
                    _ <- types1.collectFirst { case BuiltinType(TypeBuiltinFunction.Int, Seq()) => () }
                  } yield x1) {
                    case Some(types11) =>
                      types11 should have size(2)
                      inside(for {
                        _ <- types11.collectFirst { case BuiltinType(TypeBuiltinFunction.Zero, Seq()) => () }
                        _ <- types11.collectFirst { case BuiltinType(TypeBuiltinFunction.NonZero, Seq()) => () }
                      } yield ()) {
                        case Some(_) =>
                          ()
                      }
                  }
              }
              combTypeParams should be ('empty)
          }
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))).flatMap(treeInfo.typeTable.types.get)) {
            case Some(InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), Seq())) =>
              // ((#Zero #| #NonZero) #& #Int) #-> ((#Zero #| #NonZero) #& #Int)
              inside(argType1) {
                case TypeConjunction(types1) =>
                  types1 should have size(2)
                  inside(for {
                    x1 <- types1.collectFirst { case TypeDisjunction(types11) => types11 }
                    _ <- types1.collectFirst { case BuiltinType(TypeBuiltinFunction.Int, Seq()) => () }
                  } yield x1) {
                    case Some(types11) =>
                      types11 should have size(2)
                      inside(for {
                        _ <- types11.collectFirst { case BuiltinType(TypeBuiltinFunction.Zero, Seq()) => () }
                        _ <- types11.collectFirst { case BuiltinType(TypeBuiltinFunction.NonZero, Seq()) => () }
                      } yield ()) {
                        case Some(_) =>
                          ()
                      }
                  }
              }
              inside(retType1) {
                case TypeConjunction(types1) =>
                  types1 should have size(2)
                  inside(for {
                    x1 <- types1.collectFirst { case TypeDisjunction(types11) => types11 }
                    _ <- types1.collectFirst { case BuiltinType(TypeBuiltinFunction.Int, Seq()) => () }
                  } yield x1) {
                    case Some(types11) =>
                      types11 should have size(2)
                      inside(for {
                        _ <- types11.collectFirst { case BuiltinType(TypeBuiltinFunction.Zero, Seq()) => () }
                        _ <- types11.collectFirst { case BuiltinType(TypeBuiltinFunction.NonZero, Seq()) => () }
                      } yield ()) {
                        case Some(_) =>
                          ()
                      }
                  }
              }
          }
      }
    }
    
    it should "transform inferred types to global type table" in {
      val (typeEnv, res) = Typer.transformString("""
f x y = x y
g = #fAdd 1.0f 2.0f
""")(NameTree.empty, kindTableFromData(initData), InferredTypeTable.empty)(f3)(g3).run(emptyTypeEnv)
      inside(res) {
        case Success(Tree(combs, treeInfo)) =>
          val combSyms = Set(GlobalSymbol(NonEmptyList("f")), GlobalSymbol(NonEmptyList("g")))
          val combLocs = combSyms.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo))
          combLocs should have size(2)
          treeInfo.typeTable.types.keySet should be ===(combLocs)
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))).flatMap(treeInfo.typeTable.types.get)) {
            case Some(InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), argKinds)) =>
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
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("g"))).flatMap(treeInfo.typeTable.types.get)) {
            case Some(InferredType(BuiltinType(TypeBuiltinFunction.Float, Seq()), Seq())) =>
              // #Float
              ()
          }
      }
    }
    
    it should "transform inferred types to local type tables" in {
      val (typeEnv, res) = Typer.transformString("""
f x y = x y
g x = \y z => tuple 2 (#zAnd x z) y
""")(NameTree.empty, kindTableFromData(initData), InferredTypeTable.empty)(f3)(g3).run(emptyTypeEnv)
      inside(res) {
        case Success(Tree(combs, treeInfo)) =>
          val combSyms = Set(GlobalSymbol(NonEmptyList("f")), GlobalSymbol(NonEmptyList("g")))
          val combLocs = combSyms.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo))
          combLocs should have size(2)
          treeInfo.typeTable.types.keySet should be ===(combLocs)
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))).flatMap(combs.get)) {
            case Some(Combinator(None, _, _, LambdaInfo(lambdaInfo, 0, typeTable, None, combTypeParams), _)) =>
              val syms = Set(LocalSymbol("x"), LocalSymbol("y"))
              val locs = syms.flatMap(localSymTabular.getLocalLocationFromTable(lambdaInfo))
              locs should have size(2)
              typeTable.types.keySet should be ===(locs)
              inside(localSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("x")).flatMap(typeTable.types.get)) {
                case Some(InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), argKinds)) =>
                  // \t1 t2 => t1 #-> t2
                  inside(argType1) {
                    case TypeParamApp(param1, Seq(), 0) =>
                      inside(retType1) {
                        case TypeParamApp(param2, Seq(), 0) =>
                          List(param1, param2).toSet should have size(2)
                      }
                  }
                  inside(argKinds) {
                    case Seq(
                        InferredKind(Star(KindType, _)) /* * */,
                        InferredKind(Star(KindType, _)) /* * */) =>
                      ()
                  }
              }
              inside(localSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("y")).flatMap(typeTable.types.get)) {
                case Some(InferredType(TypeParamApp(_, Seq(), 0), argKinds)) =>
                  // \t1 => t1
                  inside(argKinds) {
                    case Seq(
                        InferredKind(Star(KindType, _)) /* * */) =>
                      ()
                  }
              }
              combTypeParams.keySet should be ===(Set(0, 1))
              combTypeParams.values.toSet should be ===(Set(0, 1))
          }
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("g"))).flatMap(combs.get)) {
            case Some(Combinator(None, _, body, LambdaInfo(lambdaInfo, 0, typeTable, None, combTypeParams), _)) =>
              val syms = Set(LocalSymbol("x"))
              val locs = syms.flatMap(localSymTabular.getLocalLocationFromTable(lambdaInfo))
              locs should have size(1)
              typeTable.types.keySet should be ===(locs)
              inside(localSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("x")).flatMap(typeTable.types.get)) {
                case Some(InferredType(BuiltinType(TypeBuiltinFunction.Boolean, Seq()), Seq())) =>
                  // #Boolean
                  ()
              }
              inside(body) {
                case Simple(Lambda(_, _, LambdaInfo(lambdaInfo1, 1, typeTable1, None, combTypeParams1)), _) =>
                  val syms1 = Set(LocalSymbol("y"), LocalSymbol("z"))
                  val locs1 = syms1.flatMap(localSymTabular.getLocalLocationFromTable(lambdaInfo1))
                  locs1 should have size(2)
                  typeTable1.types.keySet should be ===(locs1)
                  inside(localSymTabular.getLocalLocationFromTable(lambdaInfo1)(LocalSymbol("y")).flatMap(typeTable1.types.get)) {
                    case Some(InferredType(TypeParamApp(_, Seq(), _), argKinds)) =>
                      // \t1 => t1
                      inside(argKinds) {
                        case Seq(
                            InferredKind(Star(KindType, _)) /* * */) =>
                          ()
                      }
                  }
                  inside(localSymTabular.getLocalLocationFromTable(lambdaInfo1)(LocalSymbol("z")).flatMap(typeTable1.types.get)) {
                    case Some(InferredType(BuiltinType(TypeBuiltinFunction.Boolean, Seq()), Seq())) =>
                      // #Boolean
                      ()
                  }
                  combTypeParams1 should be ('empty)
              }
              combTypeParams.keySet should be ===(Set(0))
              combTypeParams.values.toSet should be ===(Set(0))
          }
      }
    }
    
    it should "transform instance types to instance types of lambda informations" in {
      val (typeEnv, res) = Typer.transformString("""
unittype 2 T
unittype 1 U
unittype 0 V
f x y = (construct 2 x y): \t1 t2 => ##& (T t1 t2) (tuple 2 t1 t2)
g x = x select {
    (y: \t1 t2 => T t1 t2) => 1
    (y: \t1 => U t1)       => 2
    (y: V)                 => 3
  }
""")(NameTree.empty, kindTableFromData(initData), InferredTypeTable.empty)(f3)(g3).run(emptyTypeEnv)
      inside(res) {
        case Success(Tree(combs, treeInfo)) =>
          val typeTree = treeInfoExtractor.typeTreeFromTreeInfo(treeInfo.treeInfo)
          val typeCombs = typeTree.combs
          val typeTreeInfo = typeTree.treeInfo
          val combSyms = Set(GlobalSymbol(NonEmptyList("f")), GlobalSymbol(NonEmptyList("g")))
          val combLocs = combSyms.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo))
          combLocs should have size(2)
          treeInfo.typeTable.types.keySet should be ===(combLocs)
          val typeCombSyms = List(
              GlobalSymbol(NonEmptyList("T")),
              GlobalSymbol(NonEmptyList("U")),
              GlobalSymbol(NonEmptyList("V")))
          inside(typeCombSyms.flatMap(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo))) {
            case List(tLoc, uLoc, vLoc) =>
              inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))).flatMap(combs.get)) {
                case Some(Combinator(None, _, body, _, _)) =>
                  inside(body) {
                    case Simple(TypedTerm(term1, _), _) =>
                      inside(term1) {
                        case App(Simple(Construct(2, LambdaInfo(lambdaInfo1, 1, typeTable1, polyFunType1, combTypeParams1)), _), NonEmptyList(_, _), _) =>
                          typeTable1.types should be ('empty)
                          inside(polyFunType1) {
                            case Some(InferredType(TypeConjunction(types1), argKinds1)) =>
                              // \t1 t2 => (T t1 t2) #& (t1, t2)
                              types1 should have size(2)
                              inside(for {
                                x1 <- types1.collectFirst { case Unittype(loc11, Seq(arg11, arg12), GlobalSymbol(NonEmptyList("T"))) => (loc11, arg11, arg12) }
                                x2 <- types1.collectFirst { case TupleType(Seq(type13, type14)) => (type13, type14) }
                              } yield (x1, x2)) {
                                case Some(((loc11, arg11, arg12), (type13, type14))) =>
                                  loc11 should be ===(tLoc)
                                  inside(arg11) {
                                    case TypeParamApp(param11, Seq(), 0) =>
                                      inside(arg12) {
                                        case TypeParamApp(param12, Seq(), 0) =>
                                          inside(type13) {
                                            case TypeParamApp(param13, Seq(), 0) =>
                                              inside(type14) {
                                                case TypeParamApp(param14, Seq(), 0) =>
                                                  List(param11, param13).toSet should have size(1)
                                                  List(param12, param14).toSet should have size(1)
                                                  List(param11, param12, param13, param14).toSet should have size(2)
                                              }
                                          }
                                      }
                                  }
                              }
                              inside(argKinds1) {
                                case Seq(
                                    InferredKind(Star(KindType, _)) /* * */,
                                    InferredKind(Star(KindType, _)) /* * */) =>
                                  ()
                              }
                          }
                          combTypeParams1 should be ('empty)
                      }
                  }
              }
              inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("g"))).flatMap(combs.get)) {
                case Some(Combinator(None, _, body, _, _)) =>
                  inside(body) {
                    case Simple(Select(_, cases1, LambdaInfo(lambdaInfo1, 1, typeTable1, polyFunType1, combTypeParams1)), _) =>
                      typeTable1.types should be ('empty)
                      inside(polyFunType1) {
                        case Some(InferredType(TypeDisjunction(types1), argKinds1)) =>
                          // \t1 t2 t3 => (T t1 t2) #& (U t3) #& V
                          inside(for {
                            x1 <- types1.collectFirst { case GlobalTypeApp(loc11, Seq(arg11, arg12), GlobalSymbol(NonEmptyList("T"))) => (loc11, arg11, arg12) }
                            x2 <- types1.collectFirst { case GlobalTypeApp(loc12, Seq(arg13), GlobalSymbol(NonEmptyList("U"))) => (loc12, arg13) }
                            x3 <- types1.collectFirst { case GlobalTypeApp(loc13, Seq(), GlobalSymbol(NonEmptyList("V"))) => loc13 }
                          } yield (x1, x2, x3)) {
                            case Some(((loc11, arg11, arg12), (loc12, arg13), loc13)) =>
                              loc11 should be ===(tLoc)
                              loc12 should be ===(uLoc)
                              loc13 should be ===(vLoc)
                              inside(arg11) {
                                case TypeValueLambda(Seq(), TypeParamApp(param11, Seq(), 0)) =>
                                  inside(arg12) {
                                    case TypeValueLambda(Seq(), TypeParamApp(param12, Seq(), 0)) =>
                                      inside(arg13) {
                                        case TypeValueLambda(Seq(), TypeParamApp(param13, Seq(), 0)) =>
                                          List(param11, param12, param13).toSet should have size(3)
                                      }
                                  }
                              }
                          }
                          inside(argKinds1) {
                            case Seq(
                                 InferredKind(Star(KindType, _)) /* * */,
                                 InferredKind(Star(KindType, _)) /* * */,
                                 InferredKind(Star(KindType, _)) /* * */) =>
                              ()
                          }
                      }
                      inside(cases1) {
                        case NonEmptyList(case11, case12, case13) =>
                          inside(case11) {
                            case Case(_, _, _, LambdaInfo(lambdaInfo11, 3, typeTable11, polyFunType11, combTypeParams11)) =>
                              val syms11 = Set(LocalSymbol("y"))
                              val locs11 = syms11.flatMap(localSymTabular.getLocalLocationFromTable(lambdaInfo11))
                              locs11 should have size(1)
                              typeTable11.types.keySet should be ===(locs11)
                              inside(localSymTabular.getLocalLocationFromTable(lambdaInfo11)(LocalSymbol("y")).flatMap(typeTable11.types.get)) {
                                case Some(InferredType(GlobalTypeApp(loc111, args111, GlobalSymbol(NonEmptyList("T"))), argKinds11)) =>
                                  // \t1 t2 => T t1 t2
                                  loc111 should be ===(tLoc)
                                  inside(args111) {
                                    case Seq(arg1111, arg1112) =>
                                      inside(arg1111) {
                                        case TypeValueLambda(Seq(), TypeParamApp(param1111, Seq(), 0)) =>
                                          inside(arg1112) {
                                            case TypeValueLambda(Seq(), TypeParamApp(param1112, Seq(), 0)) =>
                                              List(param1111, param1112).toSet should have size(2)
                                          }
                                      }
                                  }
                                  inside(argKinds11) {
                                    case Seq(
                                        InferredKind(Star(KindType, _)) /* * */,
                                        InferredKind(Star(KindType, _)) /* * */) =>
                                      ()
                                  }
                              }
                              inside(polyFunType11) {
                                case Some(InferredType(GlobalTypeApp(loc111, args111, GlobalSymbol(NonEmptyList("T"))), argKinds11)) =>
                                  // \t1 t2 => T t1 t2
                                  loc111 should be ===(tLoc)
                                  inside(args111) {
                                    case Seq(arg1111, arg1112) =>
                                      inside(arg1111) {
                                        case TypeValueLambda(Seq(), TypeParamApp(param1111, Seq(), 0)) =>
                                          inside(arg1112) {
                                            case TypeValueLambda(Seq(), TypeParamApp(param1112, Seq(), 0)) =>
                                              List(param1111, param1112).toSet should have size(2)
                                          }
                                      }
                                  }
                                  inside(argKinds11) {
                                    case Seq(
                                         InferredKind(Star(KindType, _)) /* * */,
                                         InferredKind(Star(KindType, _)) /* * */,
                                         InferredKind(Star(KindType, _)) /* * */) =>
                                      ()
                                  }
                              }
                              combTypeParams11 should be ('empty)
                          }
                          inside(case12) {
                            case Case(_, _, _, LambdaInfo(lambdaInfo12, 4, typeTable12, polyFunType12, combTypeParams12)) =>
                              val syms12 = Set(LocalSymbol("y"))
                              val locs12 = syms12.flatMap(localSymTabular.getLocalLocationFromTable(lambdaInfo12))
                              locs12 should have size(1)
                              typeTable12.types.keySet should be ===(locs12)
                              inside(localSymTabular.getLocalLocationFromTable(lambdaInfo12)(LocalSymbol("y")).flatMap(typeTable12.types.get)) {
                                case Some(InferredType(GlobalTypeApp(loc121, args121, GlobalSymbol(NonEmptyList("U"))), argKinds12)) =>
                                  // \t1 => U t1
                                  loc121 should be ===(uLoc)
                                  inside(args121) {
                                    case Seq(arg1211) =>
                                      inside(arg1211) { case TypeValueLambda(Seq(), TypeParamApp(_, Seq(), 0)) => () }
                                  }
                                  inside(argKinds12) {
                                    case Seq(
                                        InferredKind(Star(KindType, _)) /* * */) =>
                                      ()
                                  }
                              }
                              inside(polyFunType12) {
                                case Some(InferredType(GlobalTypeApp(loc121, args121, GlobalSymbol(NonEmptyList("U"))), argKinds12)) =>
                                  // \t1 => U t1
                                  loc121 should be ===(uLoc)
                                  inside(args121) {
                                    case Seq(arg1211) =>
                                      inside(arg1211) { case TypeValueLambda(Seq(), TypeParamApp(_, Seq(), 0)) => () }
                                  }
                                  inside(argKinds12) {
                                    case Seq(
                                        InferredKind(Star(KindType, _)) /* * */,
                                        InferredKind(Star(KindType, _)) /* * */,
                                        InferredKind(Star(KindType, _)) /* * */) =>
                                     ()
                                  }
                              }
                              combTypeParams12 should be ('empty)
                          }
                          inside(case13) {
                            case Case(_, _, _, LambdaInfo(lambdaInfo13, 5, typeTable13, polyFunType13, combTypeParams13)) =>
                              val syms13 = Set(LocalSymbol("y"))
                              val locs13 = syms13.flatMap(localSymTabular.getLocalLocationFromTable(lambdaInfo13))
                              locs13 should have size(1)
                              typeTable13.types.keySet should be ===(locs13)
                              inside(localSymTabular.getLocalLocationFromTable(lambdaInfo13)(LocalSymbol("y")).flatMap(typeTable13.types.get)) {
                                case Some(InferredType(GlobalTypeApp(loc131, Seq(), GlobalSymbol(NonEmptyList("V"))), Seq())) =>
                                  // V
                                  loc131 should be ===(vLoc)
                              }
                              inside(polyFunType13) {
                                case Some(InferredType(GlobalTypeApp(loc131, Seq(), GlobalSymbol(NonEmptyList("V"))), argKinds13)) =>
                                  // V
                                  loc131 should be ===(vLoc)
                                  inside(argKinds13) {
                                    case Seq(
                                        InferredKind(Star(KindType, _)) /* * */,
                                        InferredKind(Star(KindType, _)) /* * */,
                                        InferredKind(Star(KindType, _)) /* * */) =>
                                     ()
                                  }
                              }
                              combTypeParams13 should be ('empty)
                          }
                      }
                      combTypeParams1 should be ('empty)
                  }
              }
          }
      }
    }
    
    it should "transform the string with the references of the other tree" in {
      val s = """
unittype 0 T 
f x = #fMul x x
"""
      val (typeEnv, res) = Typer.transformString(s)(NameTree.empty, kindTableFromData(initData), InferredTypeTable.empty)(f3)(g3).run(emptyTypeEnv)
      val res2 = makeData(s)
      val nameTree = NameTree.fromTypeGlobalSymbol(GlobalSymbol(NonEmptyList("T"))) |+| NameTree.fromGlobalSymbol(GlobalSymbol(NonEmptyList("f")))
      inside((res |@| res2) { (t, d) => (t, d) }) {
        case Success((Tree(_, treeInfo), data)) =>
          val typeTree = treeInfoExtractor.typeTreeFromTreeInfo(treeInfo.treeInfo)
          val typeTreeInfo = typeTree.treeInfo
          val typeCombSyms = List(GlobalSymbol(NonEmptyList("T")))
          val (typeEnv2, res3) = Typer.transformString("""
unittype 0 U
g x y = #fAdd (f x) y
h = tuple 2 (construct 0: T) (construct 0: U)
""")(nameTree, kindTableFromData(data), treeInfo.typeTable)(f4(data))(g3).run(typeEnv)
          inside(res3) {
            case Success(Tree(combs2, treeInfo2)) =>
              val typeTree2 = treeInfoExtractor.typeTreeFromTreeInfo(treeInfo2.treeInfo)
              val typeCombs2 = typeTree2.combs
              val typeTreeInfo2 = typeTree2.treeInfo
              val combSyms2 = Set(GlobalSymbol(NonEmptyList("g")), GlobalSymbol(NonEmptyList("h")))
              val combLocs2 = combSyms2.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo))
              combLocs2 should have size(2)
              treeInfo2.typeTable.types.keySet should be ===(combLocs2)
              val typeCombSyms2 = List(GlobalSymbol(NonEmptyList("U")))
              inside(typeCombSyms.flatMap(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)) ++ typeCombSyms2.flatMap(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo2.treeInfo))) {
                case List(tLoc, uLoc) =>
                  inside(globalSymTabular.getGlobalLocationFromTable(treeInfo2.treeInfo)(GlobalSymbol(NonEmptyList("g"))).flatMap(combs2.get)) {
                    case Some(Combinator(None, args, body, LambdaInfo(lambdaInfo, 0, typeTable, None, combTypeParams), _)) =>
                      inside(args) {
                        case List(arg1, arg2) =>
                          inside(arg1) { case Arg(Some("x"), None, _) => () }
                          inside(arg2) { case Arg(Some("y"), None, _) => () }
                      }
                      inside(body) {
                        case App(Simple(Literal(BuiltinFunValue(BuiltinFunction.FAdd)), _), args1, _) =>
                          inside(args1) {
                            case NonEmptyList(arg11, arg12) =>
                              inside(arg11) { 
                                case App(Simple(Var(loc11, LambdaInfo(lambdaInfo11, 1, typeTable11, polyFunType11, combTypeParams11)), _), args11, _) =>
                                  some(loc11) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))))
                                  typeTable11.types should be ('empty)
                                  inside(args11) { 
                                    case NonEmptyList(Simple(Var(loc111, LambdaInfo(lambdaInfo111, 2, typeTable111, None, combTypeParams111)), _)) =>
                                      some(loc111) should be ===(localSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("x")))
                                      typeTable111.types should be ('empty)
                                      combTypeParams111 should be ('empty)
                                  }
                                  inside(polyFunType11) {
                                    case Some(InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(arg111, ret111)), Seq())) =>
                                      // #Float #-> #Float
                                      inside(arg111) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
                                      inside(ret111) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
                                  }
                                  combTypeParams11 should be ('empty)
                              }
                              inside(arg12) {
                                case Simple(Var(loc12, LambdaInfo(lambdaInfo12, 3, typeTable12, None, combTypeParams12)), _) =>
                                  some(loc12) should be ===(localSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("y")))
                                  typeTable12.types should be ('empty)
                                  combTypeParams12 should be ('empty)
                              }
                          }
                      }
                      val syms = Set(LocalSymbol("x"), LocalSymbol("y"))
                      val locs = syms.flatMap(localSymTabular.getLocalLocationFromTable(lambdaInfo))
                      locs should have size(2)
                      typeTable.types.keySet should be ===(locs)
                      inside(localSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("x")).flatMap(typeTable.types.get)) {
                        case Some(InferredType(BuiltinType(TypeBuiltinFunction.Float, Seq()), Seq())) =>
                          // #Float
                          ()
                      }
                      inside(localSymTabular.getLocalLocationFromTable(lambdaInfo)(LocalSymbol("y")).flatMap(typeTable.types.get)) {
                        case Some(InferredType(BuiltinType(TypeBuiltinFunction.Float, Seq()), Seq())) =>
                          // #Float
                      }
                      combTypeParams should be ('empty)
                  }
                  inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("g"))).flatMap(treeInfo2.typeTable.types.get)) {
                    case Some(InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), Seq())) =>
                      // Float #-> #Float #-> #Float
                      inside(argType1) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
                      inside(retType1) {
                        case BuiltinType(TypeBuiltinFunction.Fun, Seq(argType2, retType2)) =>
                          inside(argType2) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
                          inside(retType2) { case BuiltinType(TypeBuiltinFunction.Float, Seq()) => () }
                      }
                  }
                  inside(globalSymTabular.getGlobalLocationFromTable(treeInfo2.treeInfo)(GlobalSymbol(NonEmptyList("h"))).flatMap(combs2.get)) {
                    case Some(Combinator(None, Nil, body, LambdaInfo(lambdaInfo, 0, typeTable, None, combTypeParams), _)) =>
                      inside(body) {
                        case App(Simple(Literal(TupleFunValue(2)), _), args1, _) =>
                          inside(args1) {
                            case NonEmptyList(arg11, arg12) =>
                              inside(arg11) {
                                case Simple(TypedTerm(term11, typ11), _) =>
                                  inside(term11) { 
                                    case Simple(Construct(0, LambdaInfo(lambdaInfo11, 1, typeTable11, polyFunType11, combTypeParams11)), _) =>
                                      typeTable11.types should be ('empty)
                                      inside(polyFunType11) {
                                        case Some(InferredType(TypeConjunction(types111), Seq())) =>
                                          // T #& ()
                                          types111 should have size(2)
                                          inside(for {
                                            x1 <- types111.collectFirst { case Unittype(loc1111, Seq(), GlobalSymbol(NonEmptyList("T"))) => loc1111 }
                                            _ <- types111.collectFirst { case TupleType(Seq()) => () }
                                          } yield x1) {
                                            case Some(loc1111) =>
                                            loc1111 should be ===(tLoc)
                                          }
                                      }
                                      combTypeParams11 should be ('empty)
                                  }
                                  inside(typ11) { 
                                    case Simple(TypeVar(typLoc11), _) =>
                                      typLoc11 should be ===(tLoc)
                                  }
                              }
                              inside(arg12) {
                                case Simple(TypedTerm(term12, typ12), _) =>
                                  inside(term12) {
                                    case Simple(Construct(0, LambdaInfo(lambdaInfo12, 2, typeTable12, polyFunType12, combTypeParams12)), _) =>
                                      typeTable12.types should be ('empty)
                                      inside(polyFunType12) {
                                        case Some(InferredType(TypeConjunction(types121), Seq())) =>
                                          // U #& ()
                                          types121 should have size(2)
                                          inside(for {
                                            x1 <- types121.collectFirst { case Unittype(loc1211, Seq(), GlobalSymbol(NonEmptyList("U"))) => loc1211 }
                                            _ <- types121.collectFirst { case TupleType(Seq()) => () }
                                          } yield x1) {
                                            case Some(loc1211) =>
                                              loc1211 should be ===(uLoc)
                                          }
                                      }
                                      combTypeParams12 should be ('empty)
                                  }
                                  inside(typ12) {
                                    case Simple(TypeVar(typLoc12), _) =>
                                      typLoc12 should be ===(uLoc)
                                  }
                              }
                          }
                      }
                      combTypeParams should be ('empty)
                  }
                  inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("h"))).flatMap(treeInfo2.typeTable.types.get)) {
                    case Some(InferredType(TupleType(Seq(type1, type2)), Seq())) =>
                      inside(type1) {
                        case GlobalTypeApp(loc1, Seq(), GlobalSymbol(NonEmptyList("T"))) =>
                          loc1 should be ===(tLoc)
                      }
                      inside(type2) {
                        case GlobalTypeApp(loc2, Seq(), GlobalSymbol(NonEmptyList("U"))) =>
                          loc2 should be ===(uLoc)
                      }
                  }
              }
          }
      }
    }
    
    it should "transform the string of the term with the type inference" in {
      val res = Typer.transformTermStringWithTypeInference("(\\x y => #zXor x y) true")(NameTree.empty, emptyEnv)(h)
      inside(res) {
        case Success((App(fun1, args1, _), typ)) =>
          inside(fun1) {
            case Simple(Lambda(args11, body11, LambdaInfo(lambdaInfo11, 0, typeTable11, None, combTypeParams11)), _) =>
              inside(args11) {
                case NonEmptyList(arg111, arg112) =>
                  inside(arg111) { case Arg(Some("x"), None, _) => () }
                  inside(arg112) { case Arg(Some("y"), None, _) => () }
              }
              inside(body11) {
                case App(Simple(Literal(BuiltinFunValue(BuiltinFunction.ZXor)), _), args111, _) =>
                  inside(args111) {
                    case NonEmptyList(arg1111, arg1112) =>
                      inside(arg1111) { 
                        case Simple(Var(loc1111, LambdaInfo(lambdaInfo1111, 1, typeTable1111, None, combTypeParams1111)), _) =>
                          some(loc1111) should be ===(localSymTabular.getLocalLocationFromTable(lambdaInfo11)(LocalSymbol("x")))
                          typeTable1111.types should be ('empty)
                          combTypeParams1111 should be ('empty)
                      }
                      inside(arg1112) { 
                        case Simple(Var(loc1112, LambdaInfo(lambdaInfo1112, 2, typeTable1112, None, combTypeParams1112)), _) =>
                          some(loc1112) should be ===(localSymTabular.getLocalLocationFromTable(lambdaInfo11)(LocalSymbol("y")))
                          typeTable1112.types should be ('empty)
                          combTypeParams1112 should be ('empty)
                      }
                  }
              }
              val syms11 = Set(LocalSymbol("x"), LocalSymbol("y"))
              val locs11 = syms11.flatMap(localSymTabular.getLocalLocationFromTable(lambdaInfo11))
              locs11 should have size(2)
              typeTable11.types.keySet should be ===(locs11)
              inside(localSymTabular.getLocalLocationFromTable(lambdaInfo11)(LocalSymbol("x")).flatMap(typeTable11.types.get)) {
                case Some(InferredType(BuiltinType(TypeBuiltinFunction.Boolean, Seq()), Seq())) =>
                  // #Boolean
              }
              inside(localSymTabular.getLocalLocationFromTable(lambdaInfo11)(LocalSymbol("y")).flatMap(typeTable11.types.get)) {
                case Some(InferredType(BuiltinType(TypeBuiltinFunction.Boolean, Seq()), Seq())) =>
                  // #Boolean
              }
              combTypeParams11 should be ('empty)
          }
          inside(args1) { case NonEmptyList(Simple(Literal(BooleanValue(true)), _)) => () }
          inside(typ) {
            case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(argType1, retType1)), Seq()) =>
              // #Boolean #-> #Boolean
              inside(argType1) { case BuiltinType(TypeBuiltinFunction.Boolean, Seq()) => () }
              inside(retType1) { case BuiltinType(TypeBuiltinFunction.Boolean, Seq()) => () }              
          }
      }
    }

    it should "transform the string of the term with the type inference and the global variables" in {
      val s = """
unittype 0 T
f = construct 0: T
"""
      val (typeEnv, res) = Typer.transformString(s)(NameTree.empty, kindTableFromData(initData), InferredTypeTable.empty)(f3)(g3).run(emptyTypeEnv)
      val res2 = makeData(s)
      val nameTree = NameTree.fromTypeGlobalSymbol(GlobalSymbol(NonEmptyList("T"))) |+| NameTree.fromGlobalSymbol(GlobalSymbol(NonEmptyList("f")))
      inside((res |@| res2) { (t, d) => (t, d) }) {
        case Success((Tree(_, treeInfo), data)) =>
          val typeTree = treeInfoExtractor.typeTreeFromTreeInfo(treeInfo.treeInfo)
          val typeTreeInfo = typeTree.treeInfo
          val (typeEnv2, env) = g3(kindTableFromData(data), treeInfo.typeTable).run(typeEnv)
          val res3 = Typer.transformTermStringWithTypeInference("tuple 2 f true")(nameTree, env)(h2(data))
          inside(typeGlobalSymTabular.getGlobalLocationFromTable(typeTreeInfo.treeInfo)(GlobalSymbol(NonEmptyList("T")))) {
            case Some(tLoc) =>
              inside(res3) {
                case Success((App(Simple(Literal(TupleFunValue(2)), _), args1, _), typ)) =>
                  inside(args1) {
                    case NonEmptyList(arg11, arg12) =>
                      inside(arg11) {
                        case Simple(Var(loc11, LambdaInfo(lambdaInfo11, 0, typeTable11, polyFunType11, combTypeParams11)), _) =>
                          some(loc11) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))))
                          typeTable11.types should be ('empty)
                          inside(polyFunType11) {
                            case Some(InferredType(GlobalTypeApp(loc111, Seq(), GlobalSymbol(NonEmptyList("T"))), Seq())) =>
                              loc111 should be ===(tLoc)
                          }
                          combTypeParams11 should be ('empty)
                      }
                      inside(arg12) { case Simple(Literal(BooleanValue(true)), _) => () }
                  }
                  inside(typ) {
                    case InferredType(TupleType(Seq(type1, type2)), Seq()) =>
                      // (T, #Boolean)
                      inside(type1) {
                        case GlobalTypeApp(loc1, Seq(), GlobalSymbol(NonEmptyList("T"))) =>
                          loc1 should be ===(tLoc)
                      }
                      inside(type2) { case BuiltinType(TypeBuiltinFunction.Boolean, Seq()) => () }
                  }
              }
          }
      }
    }

    it should "complain on transformation of the incorrect string" in {
      val (typeEnv, res) = Typer.transformString("""
f = #iAdd true 'a'
g x y = #zXor (x y) x
""")(NameTree.empty, kindTableFromData(initData), InferredTypeTable.empty)(f3)(g3).run(emptyTypeEnv)
      inside(res) {
        case Failure(errs) =>
          errs.map { _.msg } should be ===(NonEmptyList(
              "couldn't match type (#Zero #| #NonZero) #& #Int with type #Boolean",
              "couldn't match type (#Zero #| #NonZero) #& #Int with type #Char",
              "couldn't match type #Boolean with type \\(t1: *) => t1 #-> #Boolean"))
      }
    }
    
    it should "set the instantiation fields of the lambda informations for the non-recursive type combinators" in {
      val (env, res) = Typer.inferTypesFromTreeString("""
poly (f: \t1 t2 => tuple 3 #Boolean t1 t2)
poly (g: \t1 => tuple 2 t1 #Float)
h = f
i x = tuple 3 g h x
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      // f
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("f")))) {
        case InferredType(TupleType(Seq(BuiltinType(TypeBuiltinFunction.Boolean, Seq()), TypeParamApp(param1, Seq(), 0), TypeParamApp(param2, Seq(), 0))), argKinds) =>
          // \t1 t2 => (#Boolean, t1, t2)
          List(param1, param2).toSet should have size(2)
          inside(argKinds) {
            case Seq(
                InferredKind(Star(KindType, _)) /* * */,
                InferredKind(Star(KindType, _)) /* * */) =>
              ()
          }
      }
      // g
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("g")))) {
        case InferredType(TupleType(Seq(TypeParamApp(_, Seq(), 0), BuiltinType(TypeBuiltinFunction.Float, Seq()))), argKinds) =>
          // \t1 => (t1, #Float)
          inside(argKinds) {
            case Seq(
                InferredKind(Star(KindType, _)) /* * */) =>
              ()
          }
      }
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("h")))) {
        case InferredType(TupleType(Seq(BuiltinType(TypeBuiltinFunction.Boolean, Seq()), TypeParamApp(param1, Seq(), 0), TypeParamApp(param2, Seq(), 0))), argKinds) =>
          // \t1 t2 => (#Boolean, t1, t2)
          List(param1, param2).toSet should have size(2)
          inside(argKinds) {
            case Seq(
                InferredKind(Star(KindType, _)) /* * */,
                InferredKind(Star(KindType, _)) /* * */) =>
              ()
          }
          inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("h")))).get(0)) {
            case Some(InferenceLambdaInfo(_, None, combTypeParams1)) =>
              inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("h")))).get(1)) {
                case Some(InferenceLambdaInfo(_, polyFunType2, combTypeParams2)) =>
                  inside(polyFunType2) {
                    case Some(InferredType(TupleType(Seq(BuiltinType(TypeBuiltinFunction.Boolean, Seq()), TypeParamApp(param21, Seq(), 0), TypeParamApp(param22, Seq(), 0))), argKinds2)) =>
                      List(param21, param22).toSet should have size(2)
                      combTypeParams1 should have size(2)
                      combTypeParams1.get(param21) should be ===(some(param1))
                      combTypeParams1.get(param22) should be ===(some(param2))
                      inside(argKinds2) {
                        case Seq(
                            InferredKind(Star(KindType, _)) /* * */,
                            InferredKind(Star(KindType, _)) /* * */) =>
                          ()
                      }
                  }
                  combTypeParams2 should have size(0)
              }
          }
      }
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("i")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(arg1, ret1)), argKinds) =>
          // \t1 t2 t3 t4 => ((t1, #Float), (#Boolean, t2, t3), t4)
          inside(arg1) {
            case TypeParamApp(param1, Seq(), 0) =>
              inside(ret1) {
                case TupleType(Seq(type11, type12, type13)) =>
                  inside(type11) {
                    case TupleType(Seq(TypeParamApp(param111, Seq(), 0), BuiltinType(TypeBuiltinFunction.Float, Seq()))) =>
                      inside(type12) {
                        case TupleType(Seq(BuiltinType(TypeBuiltinFunction.Boolean, Seq()), TypeParamApp(param121, Seq(), 0), TypeParamApp(param122, Seq(), 0))) =>
                          inside(type13) {
                            case TypeParamApp(param13, Seq(), 0) =>
                              List(param1, param13).toSet should have size(1)
                              List(param1, param111, param121, param122, param13).toSet should have size(4)
                              inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("i")))).get(0)) {
                                case Some(InferenceLambdaInfo(_, None, combTypeParams1)) =>
                                  inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("i")))).get(1)) {
                                    case Some(InferenceLambdaInfo(_, polyFunType2, combTypeParams2)) =>
                                      inside(polyFunType2) {
                                        case Some(InferredType(TupleType(Seq(TypeParamApp(param21, Seq(), 0), BuiltinType(TypeBuiltinFunction.Float, Seq()))), argKinds2)) =>
                                          inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("i")))).get(2)) {
                                            case Some(InferenceLambdaInfo(_, polyFunType3, combTypeParams3)) =>
                                              inside(polyFunType3) {
                                                case Some(InferredType(TupleType(Seq(BuiltinType(TypeBuiltinFunction.Boolean, Seq()), TypeParamApp(param31, Seq(), 0), TypeParamApp(param32, Seq(), 0))), argKinds3)) =>
                                                  List(param31, param32).toSet should have size(2)
                                                  combTypeParams1 should have size(4)
                                                  combTypeParams1.get(param21) should be ===(some(param111))
                                                  combTypeParams1.get(param31) should be ===(some(param121))
                                                  combTypeParams1.get(param32) should be ===(some(param122))
                                                  combTypeParams1.keySet should be ===((0 until 4).toSet)
                                                  combTypeParams1.values.toSet should be ===((0 until 4).toSet)
                                                  inside(argKinds3) {
                                                    case Seq(
                                                        InferredKind(Star(KindType, _)) /* * */,
                                                        InferredKind(Star(KindType, _)) /* * */,
                                                        InferredKind(Star(KindType, _)) /* * */) =>
                                                      ()
                                                  }
                                              }
                                              combTypeParams3 should have size(0)
                                          }
                                          inside(argKinds2) {
                                            case Seq(
                                                InferredKind(Star(KindType, _)) /* * */,
                                                InferredKind(Star(KindType, _)) /* * */,
                                                InferredKind(Star(KindType, _)) /* * */) =>
                                              ()
                                          }
                                      }
                                      combTypeParams2 should have size(0)
                                  }
                              }
                          }
                      }
                  }
              }
          }
          inside(argKinds) {
            case Seq(
                InferredKind(Star(KindType, _)) /* * */,
                InferredKind(Star(KindType, _)) /* * */,
                InferredKind(Star(KindType, _)) /* * */,
                InferredKind(Star(KindType, _)) /* * */) =>
              ()
          }
      }
    }
    
    it should "set the instantiation fields of the lambda informations for the recursive type combinators" in {
      val (env, res) = Typer.inferTypesFromTreeString("""
poly (f: \t1 t2 => tuple 2 t1 t2)
poly (g: \t1 => tuple 2 t1 #Float)
h x = #cond (\_ => j true) (\_ => k g) x
i = h
j x = #cond (\_ => f) (\_ => i true) x
k _ = f
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      // f
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("f")))) {
        case InferredType(TupleType(_), Seq(_, _)) => ()
      }
      // g
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("g")))) {
        case InferredType(TupleType(_), Seq(_)) => ()
      }
      // h
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("h")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(arg11, ret11)), argKinds1) =>
          // \t1 t2 => #Boolean #-> (t1, t2)
          inside(arg11) { case BuiltinType(TypeBuiltinFunction.Boolean, Seq()) => () }
          inside(ret11) {
            case TupleType(Seq(TypeParamApp(param111, Seq(), 0), TypeParamApp(param112, Seq(), 0))) =>
              inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("h")))).get(0)) {
                case Some(InferenceLambdaInfo(_, None, combTypeParams1)) =>
                  inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("h")))).get(2)) {
                    case Some(InferenceLambdaInfo(_, None, combTypeParams2)) =>
                  }
                  inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("h")))).get(4)) {
                    case Some(InferenceLambdaInfo(_, polyFunType3, combTypeParams3)) =>
                      inside(polyFunType3) {
                        case Some(InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(arg31, ret31)), argKinds3)) =>
                          inside(arg31) {
                            case TupleType(Seq(TypeParamApp(param311, Seq(), 0), BuiltinType(TypeBuiltinFunction.Float, Seq()))) =>
                              inside(ret31) {
                                case TupleType(Seq(TypeParamApp(param312, Seq(), 0), TypeParamApp(param313, Seq(), 0))) =>
                                  inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("h")))).get(5)) {
                                    case Some(InferenceLambdaInfo(_, polyFunType4, combTypeParams4)) =>
                                      inside(polyFunType4) {
                                        case Some(InferredType(TupleType(Seq(TypeParamApp(param41, Seq(), 0), BuiltinType(TypeBuiltinFunction.Float, Seq()))), argKinds4)) =>
                                          // i
                                          inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("i")))) {
                                            case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(arg51, ret51)), argKinds5) =>
                                              // \t1 t2 => #Boolean #-> (t1, t2)
                                              inside(arg51) { case BuiltinType(TypeBuiltinFunction.Boolean, Seq()) => () }
                                              inside(ret51) {
                                                case TupleType(Seq(TypeParamApp(param511, Seq(), 0), TypeParamApp(param512, Seq(), 0))) =>
                                                  inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("i")))).get(0)) {
                                                    case Some(InferenceLambdaInfo(_, None, combTypeParams5)) =>
                                                      // j
                                                      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("j")))) {
                                                        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(arg61, ret61)), argKinds6) =>
                                                          // \t1 t2 => #Boolean #-> (t1, t2)
                                                          inside(arg61) { case BuiltinType(TypeBuiltinFunction.Boolean, Seq()) => () }
                                                          inside(ret61) {
                                                            case TupleType(Seq(TypeParamApp(param611, Seq(), 0), TypeParamApp(param612, Seq(), 0))) =>
                                                              inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("j")))).get(0)) {
                                                                case Some(InferenceLambdaInfo(_, None, combTypeParams6)) =>
                                                                  inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("j")))).get(2)) {
                                                                    case Some(InferenceLambdaInfo(_, polyFunType7, combTypeParams7)) =>
                                                                      inside(polyFunType7) {
                                                                        case Some(InferredType(TupleType(Seq(TypeParamApp(param71, Seq(), 0), TypeParamApp(param72, Seq(), 0))), argKinds7)) =>
                                                                          List(param111, param112).toSet should have size(2)
                                                                          List(param611, param612).toSet should have size(2)
                                                                          List(param312, param71).toSet should have size(1)
                                                                          List(param313, param72).toSet should have size(1)
                                                                          List(param312, param313).toSet should have size(2)
                                                                          combTypeParams1 should have size(3)
                                                                          combTypeParams1.get(param312) should be ===(some(param111))
                                                                          combTypeParams1.get(param313) should be ===(some(param112))
                                                                          combTypeParams1.keySet should be ===((0 until 3).toSet)
                                                                          combTypeParams1.keySet should be ===((0 until 3).toSet)
                                                                          combTypeParams5 should have size(3)
                                                                          combTypeParams5.get(param312) should be ===(some(param511))
                                                                          combTypeParams5.get(param313) should be ===(some(param512))
                                                                          combTypeParams5.keySet should be ===((0 until 3).toSet)
                                                                          combTypeParams5.values.toSet should be ===((0 until 3).toSet)
                                                                          combTypeParams6 should have size(3)
                                                                          combTypeParams6.get(param312) should be ===(some(param611))
                                                                          combTypeParams6.get(param313) should be ===(some(param612))
                                                                          combTypeParams6.keySet should be ===((0 until 3).toSet)
                                                                          combTypeParams6.values.toSet should be ===((0 until 3).toSet)
                                                                          inside(argKinds7) {
                                                                            case Seq(
                                                                                InferredKind(Star(KindType, _)) /* * */,
                                                                                InferredKind(Star(KindType, _)) /* * */,
                                                                                InferredKind(Star(KindType, _)) /* * */) =>
                                                                              ()
                                                                          }
                                                                      }
                                                                      combTypeParams7 should have size(0)
                                                                  }
                                                                  inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("j")))).get(4)) {
                                                                    case Some(InferenceLambdaInfo(_, None, combTypeParams8)) =>
                                                                      combTypeParams8 should have size(0)
                                                                  }
                                                              }
                                                          }
                                                          inside(argKinds6) {
                                                            case Seq(
                                                                InferredKind(Star(KindType, _)) /* * */,
                                                                InferredKind(Star(KindType, _)) /* * */) =>
                                                              ()
                                                          }
                                                      }
                                                  }
                                              }
                                              inside(argKinds5) {
                                                case Seq(
                                                    InferredKind(Star(KindType, _)) /* * */,
                                                    InferredKind(Star(KindType, _)) /* * */) =>
                                                  ()
                                              }
                                          }
                                          inside(argKinds4) {
                                            case Seq(
                                                InferredKind(Star(KindType, _)) /* * */,
                                                InferredKind(Star(KindType, _)) /* * */,
                                                InferredKind(Star(KindType, _)) /* * */) =>
                                              ()
                                          }
                                      }
                                      combTypeParams4 should have size(0)
                                  }
                              }
                          }
                          inside(argKinds3) {
                            case Seq(
                                InferredKind(Star(KindType, _)) /* * */,
                                InferredKind(Star(KindType, _)) /* * */,
                                InferredKind(Star(KindType, _)) /* * */) =>
                              ()
                          }
                      }
                      combTypeParams3 should have size(0)
                  }
              }
          }
          inside(argKinds1) {
            case Seq(
                InferredKind(Star(KindType, _)) /* * */,
                InferredKind(Star(KindType, _)) /* * */) =>
              ()
          }
      }
      // k
      inside(enval.globalVarTypeFromEnvironment(env)(GlobalSymbol(NonEmptyList("k")))) {
        case InferredType(BuiltinType(TypeBuiltinFunction.Fun, Seq(arg1, ret1)), argKinds) =>
          // \t1 t2 t3 => #t1 #-> (t2, t3)
          inside(arg1) { 
            case TypeParamApp(param1, Seq(), 0) =>
              inside(ret1) {
                case TupleType(Seq(TypeParamApp(param11, Seq(), 0), TypeParamApp(param12, Seq(), 0))) =>
                  inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("k")))).get(0)) {
                    case Some(InferenceLambdaInfo(_, None, combTypeParams1)) =>
                      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("k")))).get(1)) {
                        case Some(InferenceLambdaInfo(_, polyFunType2, combTypeParams2)) =>
                           inside(polyFunType2) {
                             case Some(InferredType(TupleType(Seq(TypeParamApp(param21, Seq(), 0), TypeParamApp(param22, Seq(), 0))), argKinds2)) =>
                               List(param1, param11, param12).toSet should have size(3)
                               List(param21, param22).toSet should have size(2)
                               combTypeParams1 should have size(3)
                               combTypeParams1.get(param21) should be ===(some(param11))
                               combTypeParams1.get(param22) should be ===(some(param12))
                               combTypeParams1.keySet should be ===((0 until 3).toSet)
                               combTypeParams1.values.toSet should be ===((0 until 3).toSet)
                               inside(argKinds2) {
                                 case Seq(
                                     InferredKind(Star(KindType, _)) /* * */,
                                     InferredKind(Star(KindType, _)) /* * */) =>
                                  ()
                               }
                           }
                      }
                  }
              }
          }
          inside(argKinds) {
            case Seq(
                InferredKind(Star(KindType, _)) /* * */,
                InferredKind(Star(KindType, _)) /* * */,
                InferredKind(Star(KindType, _)) /* * */) =>
              ()
          }
      }
    }
  
    it should "transform the string with the polymorphic combinator" in {
      val (typeEnv, res) = Typer.transformString("""
f = g
poly g
""")(NameTree.empty, kindTableFromData(initData), InferredTypeTable.empty)(f3)(g3).run(emptyTypeEnv)
      inside(res) {
        case Success(Tree(combs, treeInfo)) =>
          val combSyms = Set(GlobalSymbol(NonEmptyList("f")), GlobalSymbol(NonEmptyList("g")))
          val combLocs = combSyms.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo))
          combLocs should have size(2)
          treeInfo.typeTable.types.keySet should be ===(combLocs)
          // f
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))).flatMap(combs.get)) {
            case Some(Combinator(None, Nil, body, LambdaInfo(lambdaInfo, 0, typeTable, None, combTypeParams), _)) =>
              inside(body) {
                case Simple(Var(loc1, LambdaInfo(lambdaInfo1, 1, typeTable1, polyFunType1, combTypeParams1)), _) =>
                  some(loc1) should be ===(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("g"))))
                  typeTable.types should be ('empty)
                  inside(polyFunType1) {
                    case Some(InferredType(TypeParamApp(_, Seq(), _), argKinds1)) =>
                      // \t1 => t1
                      inside(argKinds1) {
                        case Seq(
                            InferredKind(Star(KindType, _))) =>
                          ()
                      }
                  }
                  combTypeParams1 should be ('empty)
              }
              typeTable.types should be ('empty)
              combTypeParams should have size(1)
          }
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("f"))).flatMap(treeInfo.typeTable.types.get)) {
            case Some(InferredType(TypeParamApp(_, Seq(), _), argKinds)) =>
              // \t1 => t1
              inside(argKinds) {
                case Seq(
                    InferredKind(Star(KindType, _))) =>
                  ()
              }
          }
          // g
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("g"))).flatMap(combs.get)) {
            case Some(PolyCombinator(None, _)) => ()
          }
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfo.treeInfo)(GlobalSymbol(NonEmptyList("g"))).flatMap(treeInfo.typeTable.types.get)) {
            case Some(InferredType(TypeParamApp(_, Seq(), _), argKinds)) =>
              // \t1 => t1
              inside(argKinds) {
                case Seq(
                    InferredKind(Star(KindType, _))) =>
                  ()
              }
          }
      }
    }
  }
  
  "A Typer" should behave like typer(SymbolTypeInferenceEnvironment.empty[parser.LambdaInfo, parser.TypeLambdaInfo], SymbolTypeEnvironment.empty[TypeLambdaInfo[parser.TypeLambdaInfo, LocalSymbol]], InferredKindTable.empty[GlobalSymbol])(makeInferredKindTable)(identity)((kt1, kt2) => InferredKindTable(kt1.kinds ++ kt2.kinds))(Typer.transformToSymbolTree2)(Typer.statefullyMakeSymbolTypeInferenceEnvironment3)(Typer.transformToSymbolTerm2)
}