/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
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
import pl.luckboy.purfuncor.common.Evaluator._
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.frontend.typer.TypeBuiltinFunction

class TypeInterpreterSpec extends FlatSpec with ShouldMatchers with Inside with TyperSpecUtils
{
  //TODO: add a test for the type built-in function value that is passed to the type function.
  
  def typer[T, U, V, W, X, Y, Z, TT, C, E, D](emptyEnv: E, initData: D)(makeData: String => ValidationNel[AbstractError, D])(f2: D => Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]] => State[E, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, V, TypeSimpleTerm[W, X]], Y]]])(g2: D => Term[TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]] => ValidationNel[AbstractError, Term[TypeSimpleTerm[W, X]]])(implicit init: Initializer[NoTypeValue[Z, W, X, C], Z, AbstractTypeCombinator[W, X], E], eval: Evaluator[TypeSimpleTerm[W, X], E, TypeValue[Z, W, X, C]], envSt: TypeEnvironmentState[E, Z, TypeValue[Z, W, X, C]], enval: TypeEnvironmental[E, TypeValue[Z, W, X, C]], treeInfoExtractor: TreeInfoExtractor[Y, Tree[Z, AbstractTypeCombinator[W, X], TT]], globalSymTabular: GlobalSymbolTabular[E, Z])
  {
    val f = f2(initData)
    val g = g2(initData)
    
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
    
    it should "interpret the type term with the global type variables" in {
      val s = """
type T = #Int
type U t1 t2 = ##& t1 t2
type V = #Float
"""
      val (env, res) = Typer.interpretTypeTreeFromTreeString(s)(NameTree.empty)(f).run(emptyEnv)
      val res2 = makeData(s)
      val nameTree = NameTree.fromTypeGlobalSymbols(Set(
          GlobalSymbol(NonEmptyList("T")),
          GlobalSymbol(NonEmptyList("U")),
          GlobalSymbol(NonEmptyList("V"))))
      inside((res |@| res2) { (_, d) => d }) {
        case Success(data) =>
          val (env2, res3) = Typer.interpretTypeTermString("tuple 2 (U T #Char) V")(nameTree)(g2(data)).run(env)
          inside(res3) {
            case Success(EvaluatedTypeValue(term)) =>
              term should be ===(TupleType(Seq[TypeValueTerm[Z]](
                  TypeConjunction(Set[TypeValueTerm[Z]](
                      BuiltinType(TypeBuiltinFunction.Int, Seq[TypeValueTerm[Z]]()),
                      BuiltinType(TypeBuiltinFunction.Char, Seq[TypeValueTerm[Z]]()))),
                  BuiltinType(TypeBuiltinFunction.Float, Seq[TypeValueTerm[Z]]()))))
          }
      }
    }
    
    it should "interpret the type term for the type parameters" in {
      val s = "type T t1 t2 = tuple 2 (t1 t2) #Int"
      val (env, res) = Typer.interpretTypeTreeFromTreeString(s)(NameTree.empty)(f).run(emptyEnv)
      val res2 = makeData(s)
      val nameTree = NameTree.fromTypeGlobalSymbol(GlobalSymbol(NonEmptyList("T")))
      inside((res |@| res2) { (_, d) => d }) {
        case Success(data) =>
          val (env2, res3) = Typer.interpretTypeTermString("""
\t1 t2 => tuple 2 t1 (T t2 (\t3 t4 => ##& (##& t1 t3) t4))
""")(nameTree)(g2(data)).run(env)
          inside(res3) {
            case Success(funValue) =>
              val (env3, res4) = envSt.withTypeParamsS(2) {
                case (_, _, newEnv2) =>
                  app[TypeSimpleTerm[W, X], E, TypeValue[Z, W, X, C]](funValue, Seq(
                      EvaluatedTypeValue(TypeParamApp(0, Nil, 0)),
                      EvaluatedTypeValue(TypeParamApp(1, Nil, 0)))).run(newEnv2)
              } (env2)
              inside(res4) {
                case EvaluatedTypeValue(term) =>
                  term should be ===(TupleType(Seq[TypeValueTerm[Z]](
                      TypeParamApp(0, Seq[TypeValueLambda[Z]](), 0),
                      TupleType(Seq[TypeValueTerm[Z]](
                          TypeParamApp(1, Seq[TypeValueLambda[Z]](
                              TypeValueLambda(Seq(2, 3), TypeConjunction(Set[TypeValueTerm[Z]](
                                  TypeParamApp(0, Seq[TypeValueLambda[Z]](), 0),
                                  TypeParamApp(2, Seq[TypeValueLambda[Z]](), 0),
                                  TypeParamApp(3, Seq[TypeValueLambda[Z]](), 0))))), 0),
                          BuiltinType(TypeBuiltinFunction.Int, Seq[TypeValueTerm[Z]]()))))))
              }
          }
      }      
    }
    
    it should "interpret the type term for the type value lambda" in {
      val s = """
type T t1 t2 = ##| t1 t2
type U = (\t1 t2 => ##-> t1 t2) #Int
"""
      val (env, res) = Typer.interpretTypeTreeFromTreeString(s)(NameTree.empty)(f).run(emptyEnv)
      val res2 = makeData(s)
      val nameTree = NameTree.fromTypeGlobalSymbols(Set(
          GlobalSymbol(NonEmptyList("T")),
          GlobalSymbol(NonEmptyList("U"))))
      inside((res |@| res2) { (_, d) => d }) {
        case Success(data) =>
          val (env2, res3) = Typer.interpretTypeTermString("\\t1 => t1 (\\t2 t3 => ##& t2 t3) T U")(nameTree)(g2(data)).run(env)
          inside(res3) {
            case Success(funValue) =>
              val (env3, res4) = envSt.withTypeParamsS(1) {
                case (_, _, newEnv2) =>
                  app[TypeSimpleTerm[W, X], E, TypeValue[Z, W, X, C]](funValue, Seq(EvaluatedTypeValue(TypeParamApp(0, Nil, 0)))).run(newEnv2)
              } (env2)
              inside(res4) {
                case EvaluatedTypeValue(term) =>
                  term should be ===(TypeParamApp(0, Seq[TypeValueLambda[Z]](
                      TypeValueLambda(Seq(1, 2), TypeConjunction(Set[TypeValueTerm[Z]](
                          TypeParamApp(1, Seq[TypeValueLambda[Z]](), 0),
                          TypeParamApp(2, Seq[TypeValueLambda[Z]](), 0)))),
                      TypeValueLambda(Seq(1, 2), TypeDisjunction(Set[TypeValueTerm[Z]](
                          TypeParamApp(1, Seq[TypeValueLambda[Z]](), 0),
                          TypeParamApp(2, Seq[TypeValueLambda[Z]](), 0)))),
                      TypeValueLambda(Seq(1), BuiltinType(TypeBuiltinFunction.Fun, Seq[TypeValueTerm[Z]](
                          BuiltinType(TypeBuiltinFunction.Int, Seq[TypeValueTerm[Z]]()),
                          TypeParamApp(1, Seq[TypeValueLambda[Z]](), 0))))), 0))
              }
          }
      }
    }
    
    it should "initialize the recusive type that is the type function" in {
      val (env, res) = Typer.interpretTypeTreeFromTreeString("""
type T = U #Int
type U = \t => ##| #Int (U t)
""")(NameTree.empty)(f).run(emptyEnv)
      res should be ===(().success.success)
      inside(enval.globalTypeVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("T")))) {
        case EvaluatedTypeValue(term) =>
          inside(globalSymTabular.getGlobalLocationFromTable(env)(GlobalSymbol(NonEmptyList("U")))) {
            case Some(loc) =>
              term should be ===(TypeDisjunction(Set[TypeValueTerm[Z]](
                  BuiltinType(TypeBuiltinFunction.Int, Seq[TypeValueTerm[Z]]()),
                  GlobalTypeApp(loc,
                      Seq[TypeValueLambda[Z]](
                          TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Int, Seq[TypeValueTerm[Z]]()))),
                      GlobalSymbol(NonEmptyList("U"))))))
          }
      }
    }

    it should "interpret the type tree with the unit type" in {
      val (env, res) = Typer.interpretTypeTreeFromTreeString("""
type T = U #Int #Long #Float
unittype 3 U
""")(NameTree.empty)(f).run(emptyEnv)
	  res should be ===(().success.success)
	  inside(enval.globalTypeVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("T")))) {
        case EvaluatedTypeValue(term) =>
          inside(globalSymTabular.getGlobalLocationFromTable(env)(GlobalSymbol(NonEmptyList("U")))) {
            case Some(loc) =>
              term should be ===(Unittype(loc,
                  Seq[TypeValueTerm[Z]](
                      BuiltinType(TypeBuiltinFunction.Int, Seq[TypeValueTerm[Z]]()),
                      BuiltinType(TypeBuiltinFunction.Long, Seq[TypeValueTerm[Z]]()),
                      BuiltinType(TypeBuiltinFunction.Float, Seq[TypeValueTerm[Z]]())),
                  GlobalSymbol(NonEmptyList("U"))))
          }
      }
	  inside(enval.globalTypeVarValueFromEnvironment(env)(GlobalSymbol(NonEmptyList("U")))) {
	    case TypeCombinatorValue(_, _, _) => ()
	  }
    }
    
    it should "partially interpret the string of the type term" in {
      val s = """
type T t = #Array t
unittype 2 U
"""
      val (env, res) = Typer.interpretTypeTreeFromTreeString(s)(NameTree.empty)(f).run(emptyEnv)
      val res2 = makeData(s)
      val nameTree = NameTree.fromTypeGlobalSymbols(Set(
          GlobalSymbol(NonEmptyList("T")),
          GlobalSymbol(NonEmptyList("U"))))
      inside((res |@| res2) { (_, d) => d }) {
        case Success(data) =>
          val (env2, res3) = enval.withPartialEvaluation(env)(true) {
            Typer.interpretTypeTermString("tuple 2 (T #Int) (U #Int #Long)")(nameTree)(g2(data)).run
          }
          inside(res3) {
            case Success(EvaluatedTypeValue(term)) =>
              val syms = List(GlobalSymbol(NonEmptyList("T")), GlobalSymbol(NonEmptyList("U")))
              inside(syms.flatMap(globalSymTabular.getGlobalLocationFromTable(env2))) {
                case List(loc1, loc2) =>
                  term should be ===(TupleType(Seq[TypeValueTerm[Z]](
                      GlobalTypeApp(loc1, Seq(
                          TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Int, Seq()))
                          ), GlobalSymbol(NonEmptyList("T"))),
                      GlobalTypeApp(loc2, Seq(
                          TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Int, Seq())),
                          TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Long, Seq()))
                          ), GlobalSymbol(NonEmptyList("U"))))))
              }
          }
      }
    }
    
    it should "interpret the evaluated type lambda value" in {
      val s = """
type T t1 t2 t3 = tuple 3 t1 t2 t3
unittype 1 U
"""
      val (env, res) = Typer.interpretTypeTreeFromTreeString(s)(NameTree.empty)(f).run(emptyEnv)
      val res2 = makeData(s)
      val nameTree = NameTree.fromTypeGlobalSymbols(Set(
          GlobalSymbol(NonEmptyList("T")),
          GlobalSymbol(NonEmptyList("U"))))
      inside((res |@| res2) { (_, d) => d }) {
        case Success(data) =>
          val syms = List(GlobalSymbol(NonEmptyList("T")), GlobalSymbol(NonEmptyList("U")))
          inside(syms.flatMap(globalSymTabular.getGlobalLocationFromTable(env))) {
            case List(loc1, loc2) =>
              // \t1 t2 => (T t1 t2 t3, U t2, t1, t4)
              val funValue = EvaluatedTypeLambdaValue[Z, W, X, C](TypeValueLambda(Seq(0, 1), TupleType(Seq(
                  GlobalTypeApp(loc1, Seq(
                      TypeValueLambda(Nil, TypeParamApp(0, Nil, 0)),
                      TypeValueLambda(Nil, TypeParamApp(1, Nil, 0)),
                      TypeValueLambda(Nil, TypeParamApp(2, Nil, 0))
                      ), GlobalSymbol(NonEmptyList("T"))),
                  GlobalTypeApp(loc2, Seq(
                      TypeValueLambda(Nil, TypeParamApp(1, Nil, 0))
                      ), GlobalSymbol(NonEmptyList("U"))),
                  TypeParamApp(0, Nil, 0),
                  TypeParamApp(3, Nil, 0)))))
              val (env2, res3) = app[TypeSimpleTerm[W, X], E, TypeValue[Z, W, X, C]](funValue, Seq(
                  EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Int, Nil)),
                  EvaluatedTypeValue(BuiltinType(TypeBuiltinFunction.Long, Nil)))).run(env)
              inside(res3) {
                case EvaluatedTypeValue(term) =>
                  term should be ===(TupleType(Seq[TypeValueTerm[Z]](
                      GlobalTypeApp(loc1, Seq(
                          TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Int, Seq())),
                          TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Long, Seq())),
                          TypeValueLambda(Seq(), TypeParamApp(2, Seq(), 0))
                          ), GlobalSymbol(NonEmptyList("T"))),
                      GlobalTypeApp(loc2, Seq(
                          TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Long, Seq()))
                          ), GlobalSymbol(NonEmptyList("U"))),
                      BuiltinType(TypeBuiltinFunction.Int, Seq()),
                      TypeParamApp(3, Nil, 0))))
             }
          }
      }
    }
    
    it should "interpret the evaluated type lambda value with the type parameter applications" in {
      val s = "type T t1 t2 = tuple 2 (t1 t2) t2"
      val (env, res) = Typer.interpretTypeTreeFromTreeString(s)(NameTree.empty)(f).run(emptyEnv)
      val res2 = makeData(s)
      val nameTree = NameTree.fromTypeGlobalSymbols(Set(GlobalSymbol(NonEmptyList("T"))))
      inside((res |@| res2) { (_, d) => d }) {
        case Success(data) =>
          val syms = List(GlobalSymbol(NonEmptyList("T")), GlobalSymbol(NonEmptyList("U")))
          inside(globalSymTabular.getGlobalLocationFromTable(env)(GlobalSymbol(NonEmptyList("T")))) {
            case Some(loc) =>
              // \t1 t2 => (t1 t2 #Int, t2 #Long #Float #Double, T (t1 t2) #Byte, t1 (\t3 t4 => t2 (t3, t4, #Short) #Int) #Long)
              val funValue = EvaluatedTypeLambdaValue[Z, W, X, C](TypeValueLambda(Seq(0, 1), TupleType(Seq(
                  TypeParamApp(0, Seq(
                      TypeValueLambda(Nil, TypeParamApp(1, Nil, 0)),
                      TypeValueLambda(Nil, BuiltinType(TypeBuiltinFunction.Int, Nil))
                      ), 0),
                  TypeParamApp(1, Seq(
                      TypeValueLambda(Nil, BuiltinType(TypeBuiltinFunction.Long, Nil)),
                      TypeValueLambda(Nil, BuiltinType(TypeBuiltinFunction.Float, Nil)),
                      TypeValueLambda(Nil, BuiltinType(TypeBuiltinFunction.Double, Nil))
                      ), 0),
                  GlobalTypeApp(loc, Seq(
                      TypeValueLambda(Nil, TypeParamApp(0, Seq(
                          TypeValueLambda(Nil, TypeParamApp(1, Nil, 0))
                          ), 0)),
                      TypeValueLambda(Nil, BuiltinType(TypeBuiltinFunction.Byte, Nil))
                      ), GlobalSymbol(NonEmptyList("T"))),
                  TypeParamApp(0, Seq(
                      TypeValueLambda(Seq(2, 3), TypeParamApp(1, Seq(
                          TypeValueLambda(Nil, TupleType(Seq(
                              TypeParamApp(2, Nil, 0),
                              TypeParamApp(3, Nil, 0),
                              BuiltinType(TypeBuiltinFunction.Short, Nil)))),
                          TypeValueLambda(Nil, BuiltinType(TypeBuiltinFunction.Int, Nil))
                          ), 0)),
                      TypeValueLambda(Nil, BuiltinType(TypeBuiltinFunction.Long, Nil))
                      ), 0)))))
              // \t1 t2 => t1 t2 #Boolean #Char
              val arg1 = EvaluatedTypeLambdaValue[Z, W, X, C](TypeValueLambda(Seq(0, 1), TypeParamApp(0, Seq(
                  TypeValueLambda(Nil, TypeParamApp(1, Nil, 0)),
                  TypeValueLambda(Nil, BuiltinType(TypeBuiltinFunction.Boolean, Nil)),
                  TypeValueLambda(Nil, BuiltinType(TypeBuiltinFunction.Char, Nil))
                  ), 0)))
              // \t1 t2 t3 => (t1, t2, t3)
              val arg2 = EvaluatedTypeLambdaValue[Z, W, X, C](TypeValueLambda(Seq(0, 1, 2), TupleType(Seq(
                  TypeParamApp(0, Nil, 0),
                  TypeParamApp(1, Nil, 0),
                  TypeParamApp(2, Nil, 0)))))
              val (env2, res3) = app[TypeSimpleTerm[W, X], E, TypeValue[Z, W, X, C]](funValue, Seq(arg1, arg2)).run(env)
              inside(res3) {
                case EvaluatedTypeValue(term) =>
                  // ((#Int, #Boolean, #Char), (#Long, #Float, #Double), T (\t1 => (t1, #Boolean, #Char)) #Byte, ((#Long, #Boolean, #Short), #Int, #Char))
                  term should be ===(TupleType(Seq[TypeValueTerm[Z]](
                      TupleType(Seq(
                          BuiltinType(TypeBuiltinFunction.Int, Seq()),
                          BuiltinType(TypeBuiltinFunction.Boolean, Seq()),
                          BuiltinType(TypeBuiltinFunction.Char, Seq()))),
                      TupleType(Seq(
                          BuiltinType(TypeBuiltinFunction.Long, Seq()),
                          BuiltinType(TypeBuiltinFunction.Float, Seq()),
                          BuiltinType(TypeBuiltinFunction.Double, Seq()))),
                      GlobalTypeApp(loc, Seq(
                          TypeValueLambda(Seq(0), TupleType(Seq(
                              TypeParamApp(0, Seq(), 0),
                              BuiltinType(TypeBuiltinFunction.Boolean, Seq()),
                              BuiltinType(TypeBuiltinFunction.Char, Seq())))),
                          TypeValueLambda(Seq(), BuiltinType(TypeBuiltinFunction.Byte, Seq()))
                          ), GlobalSymbol(NonEmptyList("T"))),
                      TupleType(Seq(
                          TupleType(Seq(
                              BuiltinType(TypeBuiltinFunction.Long, Seq()),
                              BuiltinType(TypeBuiltinFunction.Boolean, Seq()),
                              BuiltinType(TypeBuiltinFunction.Short, Seq()))),
                          BuiltinType(TypeBuiltinFunction.Int, Seq()),
                          BuiltinType(TypeBuiltinFunction.Char, Seq()))))))
              }
          }
      }
    }
    
    it should "interpret the evaluated type lambda value for the type arguments with the free type parameters" in {
      // \t1 t2 => t1 t2 t3
      val funValue = EvaluatedTypeLambdaValue[Z, W, X, C](TypeValueLambda(Seq(0, 1), TypeParamApp(0, Seq(
          TypeValueLambda(Nil, TypeParamApp(1, Nil, 0)),
          TypeValueLambda(Nil, TypeParamApp(2, Nil, 0))
          ), 0)))
      // \t1 t2 => (t1 t2, t4)
      val arg1 = EvaluatedTypeLambdaValue[Z, W, X, C](TypeValueLambda(Seq(0, 1), TupleType(Seq(
          TypeParamApp(0, Seq(
              TypeValueLambda(Nil, TypeParamApp(1, Nil, 0))
              ), 0),
          TypeParamApp(3, Nil, 0)))))
      // \t1 => (t1, t2)
      val arg2 = EvaluatedTypeLambdaValue[Z, W, X, C](TypeValueLambda(Seq(0), TupleType(Seq(
          TypeParamApp(0, Nil, 0),
          TypeParamApp(1, Nil, 0)))))
      val (env, res3) = app[TypeSimpleTerm[W, X], E, TypeValue[Z, W, X, C]](funValue, Seq(arg1, arg2)).run(emptyEnv)
      inside(res3) {
        case EvaluatedTypeValue(term) =>
          // ((t3, t2), t4)
          term should be ===(TupleType(Seq[TypeValueTerm[Z]](
              TupleType(Seq(TypeParamApp(2, Nil, 0), TypeParamApp(1, Nil, 0))),
              TypeParamApp(3, Nil, 0))))
      }
    }
  }

  "A Typer" should behave like typer(SymbolTypeEnvironment.empty[kinder.TypeLambdaInfo[parser.TypeLambdaInfo, LocalSymbol]], InferredKindTable.empty[GlobalSymbol])(makeInferredKindTable)(Typer.statefullyTransformToSymbolTree2)(Typer.transformToSymbolTypeTerm2)
}
