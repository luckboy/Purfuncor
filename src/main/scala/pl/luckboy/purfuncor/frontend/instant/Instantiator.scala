/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.instant
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol
import pl.luckboy.purfuncor.frontend.kinder.InferredKindTable
import pl.luckboy.purfuncor.frontend.kinder.TypeLambdaInfo
import pl.luckboy.purfuncor.frontend.kinder.TypeTreeInfo
import pl.luckboy.purfuncor.frontend.kinder.SymbolKindInferenceEnvironment
import pl.luckboy.purfuncor.frontend.typer.Type
import pl.luckboy.purfuncor.frontend.typer.InferredType
import pl.luckboy.purfuncor.frontend.typer.InferredTypeTable
import pl.luckboy.purfuncor.frontend.typer.SymbolTypeEnvironment
import pl.luckboy.purfuncor.frontend.typer.SymbolTypeInferenceEnvironment
import pl.luckboy.purfuncor.frontend
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.frontend.Bind
import pl.luckboy.purfuncor.common.Initializer._
import pl.luckboy.purfuncor.common.Result._
import PolyFunInstantiator._
import TermUtils._

object Instantiator 
{
  def transformLambdaInfo[T, U, V, W, E](lambdaInfo: typer.LambdaInfo[T, U, V])(env: E)(implicit enval: InstantiationEnvironmental[E, W, V]) =
    lambdaInfo match {
      case typer.LambdaInfo(lambdaInfo2, lambdaIdx, typeTable, _, _) =>
        enval.getLambdaInfoFromEnvironment(env)(lambdaIdx).map {
          instantiationLambdaInfo =>
            LambdaInfo(lambdaInfo2, lambdaIdx, typeTable, instantiationLambdaInfo.insts).successNel
        }.getOrElse(FatalError("incorrect lambda index", none, NoPosition).failureNel)
    }
  
  private def transformTermNel1[T, U, E](terms: NonEmptyList[T])(env: E)(transform: (T, E) => ValidationNel[AbstractError, U]) =
    terms.tail.foldLeft(transform(terms.head, env).map { NonEmptyList(_) }) {
      case (Success(ts), t)   => transform(t, env).map { _ <:: ts }
      case (Failure(errs), _) => errs.failure
    }.map { _.reverse }
  
  def transformTermNel[T, U, V, W, X, Y, Z, TT, E](terms: NonEmptyList[Term[SimpleTerm[T, typer.LambdaInfo[U, V, W], TypeSimpleTerm[X, TypeLambdaInfo[Y, Z]]]]])(env: E)(implicit enval: InstantiationEnvironmental[E, TT, W]) =
    transformTermNel1(terms)(env)(transformTerm(_)(_))
  
  def transformBindNel[T, U, V, W, X, Y, Z, TT, E](binds: NonEmptyList[Bind[T, typer.LambdaInfo[U, V, W], TypeSimpleTerm[X, TypeLambdaInfo[Y, Z]]]])(env: E)(implicit enval: InstantiationEnvironmental[E, TT, W]) =
    transformTermNel1(binds)(env)(transformBind(_)(_))

  def transformCaseNel[T, U, V, W, X, Y, Z, TT, E](cases: NonEmptyList[Case[T, typer.LambdaInfo[U, V, W], TypeSimpleTerm[X, TypeLambdaInfo[Y, Z]]]])(env: E)(implicit enval: InstantiationEnvironmental[E, TT, W]) =
    transformTermNel1(cases)(env)(transformCase(_)(_))
  
  def transformBind[T, U, V, W, X, Y, Z, TT, E](bind: Bind[T, typer.LambdaInfo[U, V, W], TypeSimpleTerm[X, TypeLambdaInfo[Y, Z]]])(env: E)(implicit enval: InstantiationEnvironmental[E, TT, W]) =
    transformTerm(bind.body)(env).map { Bind(bind.name, _, bind.pos) }
  
  def transformCase[T, U, V, W, X, Y, Z, TT, E](cas: Case[T, typer.LambdaInfo[U, V, W], TypeSimpleTerm[X, TypeLambdaInfo[Y, Z]]])(env: E)(implicit enval: InstantiationEnvironmental[E, TT, W]) =
    cas match {
      case Case(name, typ, body, lambdaInfo) =>
        for {
          body2 <- transformTerm(body)(env)
          lambdaInfo2 <- transformLambdaInfo(lambdaInfo)(env)
        } yield Case(name, typ, body2, lambdaInfo2)
    }
  
  def transformTerm[T, U, V, W, X, Y, Z, TT, E](term: Term[SimpleTerm[T, typer.LambdaInfo[U, V, W], TypeSimpleTerm[X, TypeLambdaInfo[Y, Z]]]])(env: E)(implicit enval: InstantiationEnvironmental[E, TT, W]): ValidationNel[AbstractError, Term[SimpleTerm[T, LambdaInfo[U, V, W, TT], TypeSimpleTerm[X, TypeLambdaInfo[Y, Z]]]]] =
    term match {
      case App(fun, args, pos) =>
        (transformTerm(fun)(env) |@| transformTermNel(args)(env)) { App(_, _, pos) }
      case Simple(Let(binds, body, lambdaInfo), pos) =>
        for {
          binds2 <- transformBindNel(binds)(env)
          body2 <- transformTerm(body)(env)
          lambdaInfo2 <- transformLambdaInfo(lambdaInfo)(env)
        } yield Simple(Let(binds2, body2, lambdaInfo2), pos)
      case Simple(Lambda(args, body, lambdaInfo), pos) =>
        for {
          body2 <- transformTerm(body)(env)
          lambdaInfo2 <- transformLambdaInfo(lambdaInfo)(env)
        } yield Simple(Lambda(args, body2, lambdaInfo2), pos)
      case Simple(Var(loc, lambdaInfo), pos) =>
        transformLambdaInfo(lambdaInfo)(env).map { li => Simple(Var(loc, li), pos) }
      case Simple(Literal(value), pos) =>
        Simple(Literal(value), pos).successNel
      case Simple(TypedTerm(term, typ), pos) =>
        transformTerm(term)(env).map { t => Simple(TypedTerm(t, typ), pos) }
      case Simple(Construct(n, lambdaInfo), pos) =>
        transformLambdaInfo(lambdaInfo)(env).map { li => Simple(Construct(n, li), pos) }
      case Simple(Select(term, cases, lambdaInfo), pos) =>
        for {
          term2 <- transformTerm(term)(env)
          cases2 <- transformCaseNel(cases)(env)
          lambdaInfo2 <- transformLambdaInfo(lambdaInfo)(env)
        } yield Simple(Select(term2, cases2, lambdaInfo2), pos)
      case Simple(Extract(term, args, body, lambdaInfo), pos) =>
        for {
          term2 <- transformTerm(term)(env)
          body2 <- transformTerm(body)(env)
          lambdaInfo2 <- transformLambdaInfo(lambdaInfo)(env)
        } yield Simple(Extract(term2, args, body2, lambdaInfo2), pos)
    }
  
  def transformTree[T, U, V, W, X, Y, Z, TT, TU, E](tree: Tree[T, AbstractCombinator[U, typer.LambdaInfo[V, W, X], TypeSimpleTerm[Y, TypeLambdaInfo[Z, TT]]], typer.TreeInfo[TU, T, X]])(env: E)(implicit enval: InstantiationEnvironmental[E, T, X]) =
    tree.combs.foldLeft(Map[T, AbstractCombinator[U, LambdaInfo[V, W, X, T], TypeSimpleTerm[Y, TypeLambdaInfo[Z, TT]]]]().successNel[AbstractError]) {
      case (res, (loc, Combinator(typ, args, body, lambdaInfo, file))) =>
        val env2 = enval.withCurrentCombinatorLocation(env)(some(loc))
        val res2 = for {
          body2 <- transformTerm(body)(env2)
          lambdaInfo2 <- transformLambdaInfo(lambdaInfo)(env2)
        } yield Combinator(typ, args, body2, lambdaInfo2, file)
        (res |@| resultForFile(res2, file)) { (cs, c) => cs + (loc -> c) }
      case (res, (loc, PolyCombinator(typ, file))) =>
        res.map { _ + (loc -> PolyCombinator(typ, file)) }
    }.map {
      combs =>
        Tree(combs, TreeInfo(tree.treeInfo.treeInfo, tree.treeInfo.typeTable, enval.treeGlobalInstanceTreeFromEnvironment(env), InstanceArgTable(enval.instanceArgTableFromFromEnvironment(env).instArgs.filterKeys(combs.keySet.contains))))
    }
  
  def addInstancesFromTreeInfoS[T, U, V, W, X, Y, E](treeInfo: typer.TreeInfo[T, U, V])(env: E)(implicit polyFunInstantiator: PolyFunInstantiator[U, X, V, TypeLambdaInfo[W, Y], E], instTreeInfoExtractor: InstantiationTreeInfoExtractor[T, U, frontend.Instance[U], SelectConstructInstance[X, TypeLambdaInfo[W, Y]]]) = {
    val insts = instTreeInfoExtractor.instancesFromTreeInfo(treeInfo.treeInfo)
    val selectConstructInsts = instTreeInfoExtractor.selectConstructInstancesFromTreeInfo(treeInfo.treeInfo)
    polyFunInstantiator.withSaveS {
      env2 =>
        val (env3, res) = insts.foldLeft((env, ().successNel[AbstractError])) {
          case ((newEnv, newRes), (loc, insts2)) =>
            insts2.foldLeft((newEnv, newRes)) {
              case ((newEnv2, newRes2), inst) =>
                polyFunInstantiator.addInstanceS(loc, inst)(newEnv2).mapElements(identity, newRes2 |+| _)
            }
        }
        selectConstructInsts.foldLeft((env3, res)) {
          case ((newEnv, newRes), selectConstructInst) =>
            polyFunInstantiator.addSelectConstructInstanceS(selectConstructInst)(newEnv).mapElements(identity, newRes |+| _)
        }
    } (env)
  }
  
  def addInstancesFromTreeS[T, U, V, W, X, Y, Z, TT, TU, TV, E, TE](tree: Tree[T, AbstractCombinator[U, typer.LambdaInfo[V, W, X], TypeSimpleTerm[Y, TypeLambdaInfo[Z, TT]]], typer.TreeInfo[TU, T, X]])(env: E)(implicit polyFunInstantiator: PolyFunInstantiator[T, Y, X, TypeLambdaInfo[Z, TT], E], treeInfoExtractor: TreeInfoExtractor[TU, Tree[X, AbstractTypeCombinator[Y, TypeLambdaInfo[Z, TT]], TypeTreeInfo[TV, X]]], instTreeInfoExtractor: InstantiationTreeInfoExtractor[TU, T, frontend.Instance[T], SelectConstructInstance[Y, TypeLambdaInfo[Z, TT]]]) = {
    val polyCombLocs = tree.combs.flatMap {
      case (loc, PolyCombinator(_, _)) => some(loc)
      case _                           => Set()
    }.toSet
    val (env2, _) = polyFunInstantiator.addPolyCombinatorsS(polyCombLocs)(env)
    addInstancesFromTreeInfoS(tree.treeInfo)(env2)
  }
  
  def transformTermWithInstantiation[T, U, V, W, X, Y, Z, TT, E](term: Term[SimpleTerm[T, typer.LambdaInfo[U, V, W], TypeSimpleTerm[X, TypeLambdaInfo[Y, Z]]]])(env: E)(implicit polyFunInstantiator: PolyFunInstantiator[TT, X, W, TypeLambdaInfo[Y, Z], E], enval: InstantiationEnvironmental[E, TT, W], locational: Locational[T, TT, V]) = {
    val (newEnv, res) = instantiatePolyFunctionFromTermS(term)(enval.copyEnvironment(env))
    res.flatMap { _ => transformTerm(term)(newEnv) }
  }
  
  def addInstancesFromTreeInfo[T, U, V, W, X, Y, E](treeInfo: typer.TreeInfo[T, U, V])(implicit polyFunInstantiator: PolyFunInstantiator[U, X, V, TypeLambdaInfo[W, Y], E], treeExtractor: InstantiationTreeInfoExtractor[T, U, frontend.Instance[U], SelectConstructInstance[X, TypeLambdaInfo[W, Y]]]) =
    State(addInstancesFromTreeInfoS[T, U, V, W, X, Y, E](treeInfo))

  def instantiatePolyFunctionFromTermS[T, U, V, W, X, Y, Z, TT, E](term: Term[SimpleTerm[T, typer.LambdaInfo[U, V, W], TypeSimpleTerm[X, TypeLambdaInfo[Y, Z]]]])(env: E)(implicit polyFunInstantiator: PolyFunInstantiator[TT, X, W, TypeLambdaInfo[Y, Z], E], locational: Locational[T, TT, V]) =
    instantiatePolyFunctionsS(Map(none[TT] -> preinstantiationLambdaInfosFromTerm(term)))(none)(env)
    
  def instantiatePolyFunctionFromTerm[T, U, V, W, X, Y, Z, TT, E](term: Term[SimpleTerm[T, typer.LambdaInfo[U, V, W], TypeSimpleTerm[X, TypeLambdaInfo[Y, Z]]]])(implicit polyFunInstantiator: PolyFunInstantiator[TT, X, W, TypeLambdaInfo[Y, Z], E], locational: Locational[T, TT, V]) =
    State(instantiatePolyFunctionFromTermS[T, U, V, W, X, Y, Z, TT, E](term))
  
  def instantiatePolyFunctionsFromTreeS[E, L, C, I, F](tree: Tree[L, C, I])(env: F)(implicit init: Initializer[E, L, C, F]) =
    initializeS(tree)(env)
  
  def instantiatePolyFunctionsFromTree[E, L, C, I, F](tree: Tree[L, C, I])(implicit init: Initializer[E, L, C, F]) =
    State(instantiatePolyFunctionsFromTreeS[E, L, C, I, F](tree))
      
  def transformS[T, U, V, W, X, Y, Z, TT, TU, TV, E, TE](tree: Tree[T, AbstractCombinator[U, typer.LambdaInfo[V, W, X], TypeSimpleTerm[Y, TypeLambdaInfo[Z, TT]]], typer.TreeInfo[TU, T, X]])(kindTable: InferredKindTable[X], typeTable: InferredTypeTable[T, X], instTree: InstanceTree[AbstractPolyFunction[T], X, GlobalInstance[T]], instArgTable: InstanceArgTable[T, X])(f: (InferredKindTable[X], InferredTypeTable[T, X], InstanceTree[AbstractPolyFunction[T], X, GlobalInstance[T]], InstanceArgTable[T, X]) => State[TE, E])(typeEnv: TE)(implicit init: Initializer[NonEmptyList[AbstractError], T, AbstractCombinator[U, typer.LambdaInfo[V, W, X], TypeSimpleTerm[Y, TypeLambdaInfo[Z, TT]]], E], polyFunInstantiator: PolyFunInstantiator[T, Y, X, TypeLambdaInfo[Z, TT], E], enval: InstantiationEnvironmental[E, T, X], treeInfoExtractor: TreeInfoExtractor[TU, Tree[X, AbstractTypeCombinator[Y, TypeLambdaInfo[Z, TT]], TypeTreeInfo[TV, X]]], instTreeInfoExtractor: InstantiationTreeInfoExtractor[TU, T, frontend.Instance[T], SelectConstructInstance[Y, TypeLambdaInfo[Z, TT]]]) = {
    val typeTree = treeInfoExtractor.typeTreeFromTreeInfo(tree.treeInfo.treeInfo)
    val kindTable2 = InferredKindTable(typeTree.treeInfo.kindTable.kinds ++ kindTable.kinds)
    val typeTable2 = InferredTypeTable(tree.treeInfo.typeTable.types ++ typeTable.types)
    f(kindTable2, typeTable2, instTree, instArgTable).map {
      env =>
        val (env2, res) = addInstancesFromTreeS(tree)(env)
        res.flatMap {
          _ =>
            val (env3, res2) = instantiatePolyFunctionsFromTreeS(tree)(env2)
            res2.flatMap { _ => transformTree(tree)(env3) }
        }
    }.run(typeEnv)
  }
  
  def transform[T, U, V, W, X, Y, Z, TT, TU, TV, E, TE](tree: Tree[T, AbstractCombinator[U, typer.LambdaInfo[V, W, X], TypeSimpleTerm[Y, TypeLambdaInfo[Z, TT]]], typer.TreeInfo[TU, T, X]])(kindTable: InferredKindTable[X], typeTable: InferredTypeTable[T, X], instTree: InstanceTree[AbstractPolyFunction[T], X, GlobalInstance[T]], instArgTable: InstanceArgTable[T, X])(f: (InferredKindTable[X], InferredTypeTable[T, X], InstanceTree[AbstractPolyFunction[T], X, GlobalInstance[T]], InstanceArgTable[T, X]) => State[TE, E])(implicit init: Initializer[NonEmptyList[AbstractError], T, AbstractCombinator[U, typer.LambdaInfo[V, W, X], TypeSimpleTerm[Y, TypeLambdaInfo[Z, TT]]], E], polyFunInstantiator: PolyFunInstantiator[T, Y, X, TypeLambdaInfo[Z, TT], E], enval: InstantiationEnvironmental[E, T, X], treeInfoExtractor: TreeInfoExtractor[TU, Tree[X, AbstractTypeCombinator[Y, TypeLambdaInfo[Z, TT]], TypeTreeInfo[TV, X]]], instTreeInfoExtractor: InstantiationTreeInfoExtractor[TU, T, frontend.Instance[T], SelectConstructInstance[Y, TypeLambdaInfo[Z, TT]]]) =
    State(transformS[T, U, V, W, X, Y, Z, TT, TU, TV, E, TE](tree)(kindTable, typeTable, instTree, instArgTable)(f))
  
  def transformStringS[T, U, V, W, X, Y, Z, TT, TU, TV, E, TE](s: String)(nameTree: resolver.NameTree, kindTable: InferredKindTable[X], typeTable: InferredTypeTable[T, X], instTree: InstanceTree[AbstractPolyFunction[T], X, GlobalInstance[T]], instArgTable: InstanceArgTable[T, X])(f: (Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]], InferredKindTable[X], InferredTypeTable[T, X]) => State[TE, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, typer.LambdaInfo[V, W, X], TypeSimpleTerm[Y, TypeLambdaInfo[Z, TT]]], typer.TreeInfo[TU, T, X]]]])(g: (InferredKindTable[X], InferredTypeTable[T, X], InstanceTree[AbstractPolyFunction[T], X, GlobalInstance[T]], InstanceArgTable[T, X]) => State[TE, E])(typeEnv: TE)(implicit init: Initializer[NonEmptyList[AbstractError], T, AbstractCombinator[U, typer.LambdaInfo[V, W, X], TypeSimpleTerm[Y, TypeLambdaInfo[Z, TT]]], E], polyFunInstantiator: PolyFunInstantiator[T, Y, X, TypeLambdaInfo[Z, TT], E], enval: InstantiationEnvironmental[E, T, X], treeInfoExtractor: TreeInfoExtractor[TU, Tree[X, AbstractTypeCombinator[Y, TypeLambdaInfo[Z, TT]], TypeTreeInfo[TV, X]]], instTreeInfoExtractor: InstantiationTreeInfoExtractor[TU, T, frontend.Instance[T], SelectConstructInstance[Y, TypeLambdaInfo[Z, TT]]]) =
    resolver.Resolver.transformString(s)(nameTree).map {
      tree =>
        (for {
          res <- f(tree, kindTable, typeTable)
          res2 <- res.map {
            tree2 => transform(tree2)(kindTable, typeTable, instTree, instArgTable)(g)
          }.getOrElse { State((_: TE, FatalError("result is failure", none, NoPosition).failureNel)) }
        } yield res2).run(typeEnv)
    }.valueOr { errs => (typeEnv, errs.failure) }
  
  def transformString[T, U, V, W, X, Y, Z, TT, TU, TV, E, TE](s: String)(nameTree: resolver.NameTree, kindTable: InferredKindTable[X], typeTable: InferredTypeTable[T, X], instTree: InstanceTree[AbstractPolyFunction[T], X, GlobalInstance[T]], instArgTable: InstanceArgTable[T, X])(f: (Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]], InferredKindTable[X], InferredTypeTable[T, X]) => State[TE, ValidationNel[AbstractError, Tree[T, AbstractCombinator[U, typer.LambdaInfo[V, W, X], TypeSimpleTerm[Y, TypeLambdaInfo[Z, TT]]], typer.TreeInfo[TU, T, X]]]])(g: (InferredKindTable[X], InferredTypeTable[T, X], InstanceTree[AbstractPolyFunction[T], X, GlobalInstance[T]], InstanceArgTable[T, X]) => State[TE, E])(implicit init: Initializer[NonEmptyList[AbstractError], T, AbstractCombinator[U, typer.LambdaInfo[V, W, X], TypeSimpleTerm[Y, TypeLambdaInfo[Z, TT]]], E], polyFunInstantiator: PolyFunInstantiator[T, Y, X, TypeLambdaInfo[Z, TT], E], enval: InstantiationEnvironmental[E, T, X], treeInfoExtractor: TreeInfoExtractor[TU, Tree[X, AbstractTypeCombinator[Y, TypeLambdaInfo[Z, TT]], TypeTreeInfo[TV, X]]], instTreeExtractor: InstantiationTreeInfoExtractor[TU, T, frontend.Instance[T], SelectConstructInstance[Y, TypeLambdaInfo[Z, TT]]]) =
    State(transformStringS[T, U, V, W, X, Y, Z, TT, TU, TV, E, TE](s)(nameTree, kindTable, typeTable, instTree, instArgTable)(f)(g))

  def transformTermStringWithInstantiation[T, U, V, W, X, Y, Z, TT, E](s: String)(nameTree: resolver.NameTree, env: E)(f: Term[SimpleTerm[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]]] => ValidationNel[AbstractError, (Term[SimpleTerm[T, typer.LambdaInfo[U, V, W], TypeSimpleTerm[X, TypeLambdaInfo[Y, Z]]]], Type[W])])(implicit polyFunInstantiator: PolyFunInstantiator[TT, X, W, TypeLambdaInfo[Y, Z], E], enval: InstantiationEnvironmental[E, TT, W], locational: Locational[T, TT, V]) =
    for {
      term <- resolver.Resolver.transformTermString(s)(resolver.Scope.fromNameTree(nameTree))
      pair <- f(term)
      term2 <- transformTermWithInstantiation(pair._1)(env)
    } yield (term2, pair._2)
  
  val statefullyTransformToSymbolTree3 = {
    (tree: Tree[GlobalSymbol, AbstractCombinator[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]], resolver.TreeInfo[parser.TypeLambdaInfo, resolver.TypeTreeInfo]], kindTable: InferredKindTable[GlobalSymbol], typeTable: InferredTypeTable[GlobalSymbol, GlobalSymbol]) =>
      State({
        (typeEnv: SymbolTypeEnvironment[TypeLambdaInfo[parser.TypeLambdaInfo, LocalSymbol]]) =>
          (for {
            tree2 <- lmbdindexer.LambdaIndexer.transform(tree)
            tree3 <- kinder.Kinder.transform(tree2)(kindTable) { (kt: InferredKindTable[GlobalSymbol]) => SymbolKindInferenceEnvironment.fromInferredKindTable[parser.TypeLambdaInfo](kt) }
          } yield typer.Typer.transform(tree3)(kindTable, typeTable)(typer.Typer.statefullyMakeSymbolTypeInferenceEnvironment3).run(typeEnv)).valueOr { errs => (typeEnv, errs.failure) }
      })
  }
    
  val statefullyMakeSymbolInstantiationEnvironment3 = {
    (kindTable: InferredKindTable[GlobalSymbol], typeTable: InferredTypeTable[GlobalSymbol, GlobalSymbol], instTree: InstanceTree[AbstractPolyFunction[GlobalSymbol], GlobalSymbol, GlobalInstance[GlobalSymbol]], instArgTable: InstanceArgTable[GlobalSymbol, GlobalSymbol]) =>
      typer.Typer.statefullyMakeSymbolTypeInferenceEnvironment3(kindTable, typeTable).map(SymbolInstantiationEnvironment.fromInstanceTree[parser.LambdaInfo, parser.TypeLambdaInfo](instTree).withInstArgs(instArgTable.instArgs).withTypeInferenceEnv)
  }
  
  def transformToSymbolTerm2(kindTable: InferredKindTable[GlobalSymbol], typeTable: InferredTypeTable[GlobalSymbol, GlobalSymbol], typeEnv: SymbolTypeEnvironment[TypeLambdaInfo[parser.TypeLambdaInfo, LocalSymbol]]) = {
    (term: Term[SimpleTerm[Symbol, parser.LambdaInfo, TypeSimpleTerm[Symbol, parser.TypeLambdaInfo]]]) =>
      for {
        term2 <- lmbdindexer.LambdaIndexer.transformTerm(term)
        term3 <- kinder.Kinder.transformTerm(term2)(SymbolKindInferenceEnvironment.fromInferredKindTable[parser.TypeLambdaInfo](kindTable))
        pair <- typer.Typer.transformTermWithTypeInference(term3)(SymbolTypeInferenceEnvironment.fromInferredTypeTable[parser.LambdaInfo, parser.TypeLambdaInfo](typeTable).withTypeEnv(typeEnv).withKindInferenceEnv(kinder.SymbolKindInferenceEnvironment.fromInferredKindTable(kindTable)))
      } yield pair
  }
  
  val emptySymbolInstanceTree = InstanceTree.empty[AbstractPolyFunction[GlobalSymbol], GlobalSymbol, GlobalInstance[GlobalSymbol]]
}
