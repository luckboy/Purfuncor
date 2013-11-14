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
        case Some(InferenceLambdaInfo(TypeTable(types), Seq())) =>
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
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("f")))).get(1)) {
        case Some(InferenceLambdaInfo(TypeTable(types), Seq())) =>
          types should have size(2)
          inside(types.get(LocalSymbol("a"))) {
            case Some(InferredType(TypeParamApp(_, Seq(), 0), Seq(InferredKind(Star(KindType, _))))) =>
              ()
          }
          inside(types.get(LocalSymbol("b"))) {
            case Some(InferredType(TupleType(Seq(TypeParamApp(_, Seq(), 0), BuiltinType(TypeBuiltinFunction.Boolean, Seq()))), Seq(InferredKind(Star(KindType, _))))) =>
              ()
          }
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
        case Some(InferenceLambdaInfo(TypeTable(types), Seq())) =>
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
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("f")))).get(1)) {
        case Some(InferenceLambdaInfo(TypeTable(types), Seq())) =>
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
        case Some(InferenceLambdaInfo(TypeTable(types), Seq())) =>
          types should have size(0)
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("f")))).get(1)) {
        case Some(InferenceLambdaInfo(TypeTable(types), instanceTypes)) =>
          types should have size (0)
          inside(instanceTypes) {
            case Seq(InferredType(TypeConjunction(types1), argKinds)) =>
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
                    case Some(InferenceLambdaInfo(TypeTable(types), Seq())) =>
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
                  }
                  inside(enval.lambdaInfosFromEnvironment(env2)(Some(GlobalSymbol(NonEmptyList("f")))).get(1)) {
                    case Some(InferenceLambdaInfo(TypeTable(types), instanceTypes)) =>
                      types should have size(0)
                      inside(instanceTypes) {
                        case Seq(InferredType(TypeDisjunction(types1), argKinds)) =>
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
                  }
                  inside(enval.lambdaInfosFromEnvironment(env2)(Some(GlobalSymbol(NonEmptyList("f")))).get(2)) {
                    case Some(InferenceLambdaInfo(TypeTable(types), instanceTypes)) =>
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
                      inside(instanceTypes) {
                        case Seq(InferredType(GlobalTypeApp(loc1, Seq(type1, type2, type3), GlobalSymbol(NonEmptyList("T"))), argKinds)) =>
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
                  }
                  inside(enval.lambdaInfosFromEnvironment(env2)(Some(GlobalSymbol(NonEmptyList("f")))).get(3)) {
                    case Some(InferenceLambdaInfo(TypeTable(types), instanceTypes)) =>
                      types should have size(1)
                      inside(types.get(LocalSymbol("y"))) {
                        case Some(InferredType(GlobalTypeApp(loc1, Seq(), GlobalSymbol(NonEmptyList("U"))), Seq())) =>
                          loc1 should be ===(uLoc)
                      }
                      inside(instanceTypes) {
                        case Seq(InferredType(GlobalTypeApp(loc1, Seq(), GlobalSymbol(NonEmptyList("U"))), Seq())) =>
                          loc1 should be ===(uLoc)
                      }
                  }
                  inside(enval.lambdaInfosFromEnvironment(env2)(Some(GlobalSymbol(NonEmptyList("f")))).get(4)) {
                    case Some(InferenceLambdaInfo(TypeTable(types), instanceTypes)) =>
                      types should have size(1)
                      inside(types.get(LocalSymbol("y"))) {
                        case Some(InferredType(GlobalTypeApp(loc1, Seq(type1), GlobalSymbol(NonEmptyList("V"))), Seq())) =>
                          loc1 should be ===(vLoc)
                          inside(type1) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Boolean, Seq())) => () }
                      }
                      inside(instanceTypes) {
                        case Seq(InferredType(GlobalTypeApp(loc1, Seq(type1), GlobalSymbol(NonEmptyList("V"))), Seq())) =>
                          loc1 should be ===(vLoc)
                          inside(type1) { case TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Boolean, Seq())) => () }
                      }
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
        case Some(InferenceLambdaInfo(TypeTable(types), Seq())) =>
          types should have size(1)
          inside(types.get(LocalSymbol("x"))) {
            case Some(InferredType(TupleType(Seq(BuiltinType(TypeBuiltinFunction.Float, Seq()), BuiltinType(TypeBuiltinFunction.Double, Seq()), TypeParamApp(_, Seq(), 0))), argKinds)) =>
              inside(argKinds) {
                case Seq(InferredKind(Star(KindType, _))) =>
                  ()
              }
          }
      }
      inside(enval.lambdaInfosFromEnvironment(env)(Some(GlobalSymbol(NonEmptyList("f")))).get(1)) {
        case Some(InferenceLambdaInfo(TypeTable(types), instanceTypes)) =>
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
          inside(instanceTypes) {
            case Seq(InferredType(TupleType(Seq(BuiltinType(TypeBuiltinFunction.Float, Seq()), BuiltinType(TypeBuiltinFunction.Double, Seq()), TypeParamApp(_, Seq(), 0))), argKinds)) =>
              inside(argKinds) {
                case Seq(InferredKind(Star(KindType, _))) =>
                  ()
              }
          }
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
    
    it should "unify the two unit types" is (pending)
    
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

    it should "unify the two global type applications which are recursive" is (pending)
    
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
    
    it should "unify the two types which are the logical expression with the type parameters" is (pending)
    
    it should "unfiy the supertype with the type for the Any type" is (pending)
    
    it should "unify the supertype with the type for the Nothing type" is (pending)
    
    it should "unify the supertype with the type for the same logical expressions" is (pending)

    it should "unify the supertype with the type for the different logical expressions" is (pending)
    
    it should "unify the supertype with the type for the logical expressions with the type parameters" is (pending)
    
    it should "normalize the type applications before the unification of the types" is (pending)
  }
  
  "A Typer" should behave like typer(SymbolTypeInferenceEnvironment.empty[parser.LambdaInfo, parser.TypeLambdaInfo], SymbolTypeEnvironment.empty[TypeLambdaInfo[parser.TypeLambdaInfo, LocalSymbol]], InferredKindTable.empty[GlobalSymbol])(makeInferredKindTable)(identity)((_, kt) => kt)(Typer.transformToSymbolTree2)(Typer.statefullyMakeSymbolTypeInferenceEnvironment3)(Typer.transformToSymbolTerm2)
}