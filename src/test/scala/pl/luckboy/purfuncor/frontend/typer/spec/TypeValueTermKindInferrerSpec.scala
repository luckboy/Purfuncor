package pl.luckboy.purfuncor.frontend.typer.spec
import scala.util.parsing.input.NoPosition
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
import pl.luckboy.purfuncor.common.Arrow
import pl.luckboy.purfuncor.frontend.typer.TypeBuiltinFunction

class TypeValueTermKindInferrerSpec extends FlatSpec with ShouldMatchers with Inside
{
  def typeValueTermKindInferrer[T, U, V, W, X, Y, Z, TT, TU, TV, E](emptyEnv: E)(f: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, lmbdindexer.LambdaInfo[V], TypeSimpleTerm[W, lmbdindexer.TypeLambdaInfo[X]]], Y]])(implicit init: Initializer[NoKind, Z, AbstractTypeCombinator[W, lmbdindexer.TypeLambdaInfo[X]], E], inferrer: Inferrer[TU, E, Kind], unifier: Unifier[NoKind, KindTerm[StarKindTerm[Int]], E, Int], envSt2: KindInferrenceEnvironmentState[E, Z], treeInfoExtractor: TreeInfoExtractor[Y, Tree[Z, AbstractTypeCombinator[W, lmbdindexer.TypeLambdaInfo[X]], TT]], globalSymTabular: GlobalSymbolTabular[TT, Z])
  {
    it should "infer the kind from the type value term" in {
      val typeValueTerm = BuiltinType[Z](TypeBuiltinFunction.Any, Seq())
      val (env, kind) = TypeValueTermKindInferrer.inferTypeValueTermKindS(typeValueTerm)(emptyEnv)
      val (env2, instantiatedKind) = kind.instantiatedKindS(env)
      inside(instantiatedKind) {
        case InferredKind(Star(KindType, _)) =>
          // *
          ()
      }
    }
    
    it should "infer the kind from the built-in type" in {
      // #Int #-> #Char
      val typeValueTerm = BuiltinType[Z](TypeBuiltinFunction.Fun, Seq(
          BuiltinType(TypeBuiltinFunction.Int, Seq()),
          BuiltinType(TypeBuiltinFunction.Char, Seq())
          ))
      val (env, kind) = TypeValueTermKindInferrer.inferTypeValueTermKindS(typeValueTerm)(emptyEnv)
      val (env2, instantiatedKind) = kind.instantiatedKindS(env)
      inside(instantiatedKind) {
        case InferredKind(Star(KindType, _)) =>
          // *
          ()
      }
    }
    
    it should "infer the kind from the tuple type" in {
      // (#Long, (#Int, #Boolean), #Float)
      val typeValueTerm = TupleType[Z](Seq(
          BuiltinType(TypeBuiltinFunction.Long, Seq()),
          TupleType(Seq(
              BuiltinType(TypeBuiltinFunction.Int, Seq()),
              BuiltinType(TypeBuiltinFunction.Int, Seq())
              )),
          BuiltinType(TypeBuiltinFunction.Float, Seq())))
      val (env, kind) = TypeValueTermKindInferrer.inferTypeValueTermKindS(typeValueTerm)(emptyEnv)
      val (env2, instantiatedKind) = kind.instantiatedKindS(env)
      inside(instantiatedKind) {
        case InferredKind(Star(KindType, _)) =>
          // *
          ()
      }
    }
    
    it should "infer the kind from the unit type" in {
      val s = "unittype 3 T"
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          val (env, _) = kinder.Kinder.inferKindsFromTreeString(s)(NameTree.empty)(f).run(emptyEnv)
          inside(globalSymTabular.getGlobalLocationFromTable(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo).treeInfo)(GlobalSymbol(NonEmptyList("T")))) {
            case Some(loc) =>
              // T #Any #Boolean #Int
              val typeValueTerm = Unittype[Z](loc, Seq(
                  BuiltinType(TypeBuiltinFunction.Any, Seq()),
                  BuiltinType(TypeBuiltinFunction.Boolean, Seq()),
                  BuiltinType(TypeBuiltinFunction.Int, Seq())
                  ), GlobalSymbol(NonEmptyList("T")))
              val (env2, kind) = TypeValueTermKindInferrer.inferTypeValueTermKindS(typeValueTerm)(env)
              val (env3, instantiatedKind) = kind.instantiatedKindS(env2)
              inside(instantiatedKind) {
                case InferredKind(Star(KindType, _)) =>
                  // *
                  ()
              }
          }
      }
    }
    
    it should "infer the kind from the global type application" in {
      val s = """
type T t1 t2 = t2 t1
type U t1 t2 = tuple 2 t1 t2
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          val (env, _) = kinder.Kinder.inferKindsFromTreeString(s)(NameTree.empty)(f).run(emptyEnv)
          val syms = List(GlobalSymbol(NonEmptyList("T")), GlobalSymbol(NonEmptyList("U")))
          inside(syms.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo).treeInfo))) {
            case List(loc1, loc2) =>
              // T #Int U
              val typeValueTerm = GlobalTypeApp[Z](loc1, Seq(
                  TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Int, Seq())),
                  TypeValueLambda(Seq(), GlobalTypeApp(loc2, Seq(), GlobalSymbol(NonEmptyList("U"))))
                  ), GlobalSymbol(NonEmptyList("T")))
              val (env2, kind) = TypeValueTermKindInferrer.inferTypeValueTermKindS(typeValueTerm)(env)
              val (env3, instantiatedKind) = kind.instantiatedKindS(env2)
              inside(instantiatedKind) {
                case InferredKind(Arrow(Star(KindType, _), Star(KindType, _), _)) =>
                  // * -> *
                  ()
              }
          }
      }
    }
    
    it should "infer the kind from the type parameter application" in {
      // \t1 => t1 (\t2 t3 => tuple 2 #Int t3) #Any
      // t1 has inferring kind: (k1 -> k2 -> k3) -> k3 -> k1 -> k2      
      val typeValueTerm = TypeParamApp[Z](0, Seq(
          TypeValueLambda(Seq(1, 2), TupleType(Seq(
              BuiltinType(TypeBuiltinFunction.Int, Seq()),
              TypeParamApp(2, Seq(), 0)))),
          TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Any, Seq()))
          ), 0)
      val kind1 = InferredKind(Arrow(
          Arrow(Star(KindParam(0), NoPosition), Arrow(Star(KindParam(1), NoPosition), Star(KindParam(2), NoPosition), NoPosition), NoPosition),
          Arrow(Star(KindParam(2), NoPosition), Arrow(Star(KindParam(0), NoPosition), Star(KindParam(1), NoPosition), NoPosition), NoPosition), NoPosition))
      val kind2 = InferredKind(Star(KindParam(0), NoPosition))
      val (env, uninstantiatedKind1) = kind1.uninstantiatedKindS(emptyEnv)
      val (env2, _) = envSt2.addTypeParamKindS(0, uninstantiatedKind1)(env)
      val (env3, uninstantiatedKind2) = kind2.uninstantiatedKindS(env2)
      val (env4, _) = envSt2.addTypeParamKindS(1, uninstantiatedKind2)(env3)
      val (env5, uninstantiatedKind3) = kind2.uninstantiatedKindS(env4)
      val (env6, _) = envSt2.addTypeParamKindS(2, uninstantiatedKind3)(env5)
      val (env7, kind) = TypeValueTermKindInferrer.inferTypeValueTermKindS(typeValueTerm)(env6)
      val (env8, instantiatedKind) = kind.instantiatedKindS(env7)
      inside(instantiatedKind) {
        case InferredKind(Arrow(Star(KindParam(_), _), Star(KindType, _), _)) =>
          // k1 -> *
          ()
      }
      val (env9, instantiatedKind1) = uninstantiatedKind1.instantiatedKindS(env8)
      inside(instantiatedKind1) {
        case InferredKind(Arrow(arg1, ret1, _)) =>
          // (k1 -> * -> *) -> * -> k1 -> *
          inside(arg1) {
            case Arrow(Star(KindParam(param11), _), ret11, _) =>
              inside(ret11) {
                case Arrow(Star(KindType, _), Star(KindType, _), _) =>
                  inside(ret1) {
                    case Arrow(Star(KindType, _), ret2, _) =>
                      inside(ret2) {
                        case Arrow(Star(KindParam(param2), _), Star(KindType, _), _) =>
                          List(param11, param2).toSet should have size(1)
                      }
                  }
              }
          }
      }
      val (env10, instantiatedKind2) = uninstantiatedKind2.instantiatedKindS(env9)
      inside(instantiatedKind2) {
        case InferredKind(Star(KindParam(_), _)) =>
          // k1
          ()
      }
      val (env11, instantiatedKind3) = uninstantiatedKind3.instantiatedKindS(env10)
      inside(instantiatedKind3) {
        case InferredKind(Star(KindType, _)) =>
          // *
          ()
      }
    }
    
    it should "infer the kind from the type conjunction" in {
      // \t1 => t1 #& #Int #& #Any
      val typeValueTerm = TypeConjunction[Z](Set(
          TypeParamApp(0, Seq(), 0),
          BuiltinType(TypeBuiltinFunction.Int, Seq()),
          BuiltinType(TypeBuiltinFunction.Any, Seq())))
      val kind1 = InferredKind(Star(KindParam(0), NoPosition))
      val (env, uninstantiatedKind1) = kind1.uninstantiatedKindS(emptyEnv)
      val (env2, _) = envSt2.addTypeParamKindS(0, uninstantiatedKind1)(env)
      val (env3, kind) = TypeValueTermKindInferrer.inferTypeValueTermKindS(typeValueTerm)(env2)
      val (env4, instantiatedKind) = kind.instantiatedKindS(env3)
      inside(instantiatedKind) {
        case InferredKind(Star(KindType, _)) =>
          // *
          ()
      }
      val (env5, instantiatedKind1) = uninstantiatedKind1.instantiatedKindS(env4)
      inside(instantiatedKind1) {
        case InferredKind(Star(KindType, _)) =>
          // *
          ()
      }
    }

    it should "infer the kind from the type disjunction" in {
      // \t1 t2 => #Int #| #| t1 #| #Float #| t2
      val typeValueTerm = TypeConjunction[Z](Set(
          BuiltinType(TypeBuiltinFunction.Int, Seq()),
          TypeParamApp(0, Seq(), 0),
          BuiltinType(TypeBuiltinFunction.Float, Seq()),
          TypeParamApp(1, Seq(), 0)))
      val kind1 = InferredKind(Star(KindParam(0), NoPosition))
      val kind2 = kind1
      val (env, uninstantiatedKind1) = kind1.uninstantiatedKindS(emptyEnv)
      val (env2, _) = envSt2.addTypeParamKindS(0, uninstantiatedKind1)(env)
      val (env3, uninstantiatedKind2) = kind2.uninstantiatedKindS(env2)
      val (env4, _) = envSt2.addTypeParamKindS(1, uninstantiatedKind2)(env3)
      val (env5, kind) = TypeValueTermKindInferrer.inferTypeValueTermKindS(typeValueTerm)(env4)
      val (env6, instantiatedKind) = kind.instantiatedKindS(env5)
      inside(instantiatedKind) {
        case InferredKind(Star(KindType, _)) =>
          // *
          ()
      }
      val (env7, instantiatedKind1) = uninstantiatedKind1.instantiatedKindS(env6)
      inside(instantiatedKind1) {
        case InferredKind(Star(KindType, _)) =>
          // *
          ()
      }
      val (env8, instantiatedKind2) = uninstantiatedKind2.instantiatedKindS(env7)
      inside(instantiatedKind2) {
        case InferredKind(Star(KindType, _)) =>
          // *
          ()
      }
    }
    
    it should "infer the kind from the type value term with the type applications" in {
      val s = """
type T t1 t2 t3 = tuple 2 (t1 #Byte t2) t3
type U t1 t2 = tuple 3 t1 t2 #Int
"""
      inside(resolver.Resolver.transformString(s)(NameTree.empty).flatMap(f)) {
        case Success(tree) =>
          val (env, _) = kinder.Kinder.inferKindsFromTreeString(s)(NameTree.empty)(f).run(emptyEnv)
          val syms = List(GlobalSymbol(NonEmptyList("T")), GlobalSymbol(NonEmptyList("U")))
          inside(syms.flatMap(globalSymTabular.getGlobalLocationFromTable(treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo).treeInfo))) {
            case List(loc1, loc2) =>
              // \t1 t2 => (t1 (U #Int), T (\t3 t4 => (t3, t4)) #Boolean #Char, t2 (\t5 => #Int), U #Int #Long)
              val typeValueTerm = TupleType[Z](Seq(
                  TypeParamApp(0, Seq(
                      TypeValueLambda(Seq(), GlobalTypeApp(loc2, Seq(
                          TypeValueLambda[Z](Seq(), BuiltinType(TypeBuiltinFunction.Int, Seq()))
                          ), GlobalSymbol(NonEmptyList("U"))))
                      ), 0),
                  GlobalTypeApp(loc1, Seq(
                      TypeValueLambda(Seq(2, 3), TupleType(Seq(TypeParamApp(2, Seq(), 0), TypeParamApp(3, Seq(), 0)))),
                      TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Boolean, Seq())),
                      TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Char, Seq()))
                      ), GlobalSymbol(NonEmptyList("T"))),
                  TypeParamApp(1, Seq(
                      TypeValueLambda(Seq(4), BuiltinType(TypeBuiltinFunction.Int, Seq()))
                      ), 0),
                  GlobalTypeApp(loc2, Seq(
                      TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Int, Seq())),
                      TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Long, Seq()))
                      ), GlobalSymbol(NonEmptyList("U")))))
              val starKind = InferredKind(Star(KindParam(0), NoPosition))
              val (env2, uninstantiatedKind1) = starKind.uninstantiatedKindS(env)
              val (env3, _) = envSt2.addTypeParamKindS(0, uninstantiatedKind1)(env2)
              val (env4, uninstantiatedKind2) = starKind.uninstantiatedKindS(env3)
              val (env5, _) = envSt2.addTypeParamKindS(1, uninstantiatedKind2)(env4)
              val (env6, uninstantiatedKind3) = starKind.uninstantiatedKindS(env5)
              val (env7, _) = envSt2.addTypeParamKindS(2, uninstantiatedKind3)(env6)
              val (env8, uninstantiatedKind4) = starKind.uninstantiatedKindS(env7)
              val (env9, _) = envSt2.addTypeParamKindS(3, uninstantiatedKind4)(env8)
              val (env10, uninstantiatedKind5) = starKind.uninstantiatedKindS(env9)
              val (env11, _) = envSt2.addTypeParamKindS(4, uninstantiatedKind5)(env10)
              val (env12, kind) = TypeValueTermKindInferrer.inferTypeValueTermKindS(typeValueTerm)(env11)
              val (env13, instantiatedKind) = kind.instantiatedKindS(env12)
              inside(instantiatedKind) {
                case InferredKind(Star(KindType, _)) =>
                  // *
                  ()
              }
              val (env14, instantiatedKind1) = uninstantiatedKind1.instantiatedKindS(env13)
              inside(instantiatedKind1) {
                case InferredKind(Arrow(arg1, Star(KindType, _), _)) =>
                  // (* -> *) -> *
                  inside(arg1) { case Arrow(Star(KindType, _), Star(KindType, _), _) => () }
              }
              val (env15, instantiatedKind2) = uninstantiatedKind2.instantiatedKindS(env14)
              inside(instantiatedKind2) {
                case InferredKind(Arrow(arg1, Star(KindType, _), _)) =>
                  // (k1 -> *) -> *
                  inside(arg1) { case Arrow(Star(KindParam(_), _), Star(KindType, _), _) => () }
              }
              val (env16, instantiatedKind3) = uninstantiatedKind3.instantiatedKindS(env15)
              inside(instantiatedKind3) {
                case InferredKind(Star(KindType, _)) =>
                  // *
                  ()
              }
              val (env17, instantiatedKind4) = uninstantiatedKind4.instantiatedKindS(env16)
              inside(instantiatedKind4) {
                case InferredKind(Star(KindType, _)) =>
                  // *
                  ()
              }
              val (env18, instantiatedKind5) = uninstantiatedKind5.instantiatedKindS(env17)
              inside(instantiatedKind5) {
                case InferredKind(Star(KindParam(_), _)) =>
                  // k1
                  ()
              }
          }
      }
    }
  }
  
  "A TypeValueTermKindInferrer" should behave like typeValueTermKindInferrer(kinder.SymbolKindInferenceEnvironment.empty[parser.TypeLambdaInfo])(kinder.Kinder.transformToSymbolTree)
}