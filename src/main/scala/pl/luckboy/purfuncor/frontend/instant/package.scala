/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend
import scala.util.parsing.input.Position
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.Symbol
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbol
import pl.luckboy.purfuncor.frontend.resolver.LocalSymbol
import pl.luckboy.purfuncor.frontend.kinder.TypeLambdaInfo
import pl.luckboy.purfuncor.frontend.typer.DefinedType
import pl.luckboy.purfuncor.frontend.typer.Type
import pl.luckboy.purfuncor.frontend.typer.NoType
import pl.luckboy.purfuncor.frontend.typer.InferredType
import pl.luckboy.purfuncor.frontend.typer.InferringType
import pl.luckboy.purfuncor.frontend.typer.TypeConjunction
import pl.luckboy.purfuncor.frontend.typer.TupleType
import pl.luckboy.purfuncor.frontend.typer.SymbolTypeInferenceEnvironment
import pl.luckboy.purfuncor.frontend.typer.symbolSimpleTermTypeInferrer
import pl.luckboy.purfuncor.frontend
import pl.luckboy.purfuncor.common.Tree
import pl.luckboy.purfuncor.common.RecursiveInitializer._
import pl.luckboy.purfuncor.common.Result._
import pl.luckboy.purfuncor.frontend.resolver.TermUtils._
import pl.luckboy.purfuncor.frontend.typer.TypeValueTermUnifier._
import pl.luckboy.purfuncor.frontend.typer.TypeResult._
import pl.luckboy.purfuncor.frontend.instant.PolyFunInstantiator._
import pl.luckboy.purfuncor.frontend.instant.TermUtils._

package object instant
{
  implicit def symbolTypeInferenceEnvironmentState[T, U]: TypeInferenceEnvironmentState[SymbolTypeInferenceEnvironment[T, U], GlobalSymbol, GlobalSymbol] = new TypeInferenceEnvironmentState[SymbolTypeInferenceEnvironment[T, U], GlobalSymbol, GlobalSymbol] {
    override def globalVarTypeFromEnvironmentS(loc: GlobalSymbol)(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env, env.varType(loc))
    
    override def notFoundInstanceNoTypeS(instArg: InstanceArg[GlobalSymbol, GlobalSymbol])(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env, NoType.fromError[GlobalSymbol](Error("couldn't find instance for " + instArg.polyFun + " with type " + instArg.typ, none, NoPosition)))
  
    override def ambiguousInstanceNoTypeS(instArg: InstanceArg[GlobalSymbol, GlobalSymbol])(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env, NoType.fromError[GlobalSymbol](Error("ambiguous instance for " + instArg.polyFun + " with type " + instArg.typ, none, NoPosition)))
    
    override def withInstanceTypeClearingS[V](f: SymbolTypeInferenceEnvironment[T, U] => (SymbolTypeInferenceEnvironment[T, U], V))(env: SymbolTypeInferenceEnvironment[T, U]): (SymbolTypeInferenceEnvironment[T, U], V) = {
      val (_, res) = f(env.withInstTypeMatching(true))
      (env, res)
    }
    
    override def definedTypesFromEnvironmentS(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env, env.definedTypes)
    
    override def addDefinedTypeS(definedType: DefinedType[GlobalSymbol])(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env.withDefinedType(definedType), ())
    
    override def reverseTypeParamMapS(paramMap: Map[Int, Int])(env: SymbolTypeInferenceEnvironment[T, U]) =
      (env, env.typeParamForest.reverseParamMap(paramMap).toSuccess(NoType.fromError[GlobalSymbol](FatalError("can't reverse type parameter map", none, NoPosition))))
  }
  
  implicit def symbolPolyFunInstantiator[T, U](implicit envSt: TypeInferenceEnvironmentState[SymbolTypeInferenceEnvironment[T, U], GlobalSymbol, GlobalSymbol]): PolyFunInstantiator[GlobalSymbol, Symbol, GlobalSymbol, TypeLambdaInfo[U, LocalSymbol], SymbolInstantiationEnvironment[T, U]] = new PolyFunInstantiator[GlobalSymbol, Symbol, GlobalSymbol, TypeLambdaInfo[U, LocalSymbol], SymbolInstantiationEnvironment[T, U]] {
    override def instantiatePolyFunctionS(lambdaInfo: PreinstantiationLambdaInfo[GlobalSymbol, GlobalSymbol], instArgs: Seq[InstanceArg[GlobalSymbol, GlobalSymbol]])(localInstTree: Option[InstanceTree[AbstractPolyFunction[GlobalSymbol], GlobalSymbol, LocalInstance[GlobalSymbol]]])(env: SymbolInstantiationEnvironment[T, U]) = {
      val (typeInferenceEnv, res) = PolyFunInstantiator.instantiatePolyFunctionS(lambdaInfo, instArgs, env.globalInstTree)(localInstTree)(env.typeInferenceEnv)
      (env.withTypeInferenceEnv(typeInferenceEnv), resultFromTypeResult(res))
    }
    
    override def getLambdaInfosFromEnvironmentS(loc: Option[GlobalSymbol])(env: SymbolInstantiationEnvironment[T, U]) =
      (env, env.lambdaInfos.get(loc))
    
    override def addLambdaInfosS(loc: Option[GlobalSymbol], lambdaInfos: Map[Int, InstantiationLambdaInfo[GlobalSymbol]])(env: SymbolInstantiationEnvironment[T, U]) =
      (env.withLambdaInfos(env.lambdaInfos + (loc -> lambdaInfos)), ())
    
    override def getInstanceArgsFromEnvironmentS(loc: GlobalSymbol)(env: SymbolInstantiationEnvironment[T, U]): (SymbolInstantiationEnvironment[T, U], Option[Seq[InstanceArg[GlobalSymbol, GlobalSymbol]]]) =
      (env, env.instArgs.get(loc))
  
    override def addInstanceArgsS(loc: GlobalSymbol, instArgs: Seq[InstanceArg[GlobalSymbol, GlobalSymbol]])(env: SymbolInstantiationEnvironment[T, U]): (SymbolInstantiationEnvironment[T, U], Unit) =
      (env.withInstArgs(env.instArgs + (loc -> instArgs)), ())
    
    private def addGlobalInstanceS(polyFun: AbstractPolyFunction[GlobalSymbol], typ: InferredType[GlobalSymbol], inst: GlobalInstance[GlobalSymbol])(env: SymbolInstantiationEnvironment[T, U]) =
      env.globalInstTree.addInstS(polyFun, GlobalInstanceType(typ), inst)(env.typeInferenceEnv) match {
        case (typeInferenceEnv, Success(Some((instTree, None)))) =>
          (env.withTypeInferenceEnv(typeInferenceEnv).withGlobalInstTree(instTree), ().successNel)
        case (typeInferenceEnv, Success(_))                      =>
          (env.withTypeInferenceEnv(typeInferenceEnv), Error("already defined instance for " + polyFun + " with " + typ, none, NoPosition).failureNel)
        case (typeInferenceEnv, Failure(noType))                 =>
          (env.withTypeInferenceEnv(typeInferenceEnv), resultFromTypeResult(noType.failure))
      }
      
    override def addInstanceS(loc: GlobalSymbol, inst: frontend.Instance[GlobalSymbol])(env: SymbolInstantiationEnvironment[T, U]) =
      inst match {
        case frontend.Instance(instCombLoc, pos, file) =>
          if(env.hasPolyComb(loc)) {
            val (env3, res) = env.typeInferenceEnv.varType(instCombLoc) match {
              case instCombType: InferredType[GlobalSymbol] =>
                val polyCombType = env.typeInferenceEnv.varType(loc)
                val (typeInferenceEnv, unifiedType) = symbolSimpleTermTypeInferrer.unifyArgInfosS(polyCombType, instCombType)(env.typeInferenceEnv)
                val env2 = env.withTypeInferenceEnv(typeInferenceEnv)
                unifiedType match {
                  case noType: NoType[GlobalSymbol] =>
                    (env2, resultFromTypeResult(noType.failure))
                  case _                            =>
                    addGlobalInstanceS(PolyFunction(loc), instCombType, PolyFunInstance(instCombLoc, pos, file))(env2)
                }
              case noType: NoType[GlobalSymbol]             =>
                (env, resultFromTypeResult(noType.failure))
              case _                                        =>
                (env, FatalError("uninferred type", none, NoPosition).failureNel)
            }
            (env3, resultForFile(resultWithPos(res, pos), file))
          } else
            (env, Error("combinator " + loc + " isn't ad-hoc polimorphic", file, pos).failureNel)
      }
    
    private def checkConstructTypeTermS(typeTerm: Term[TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]])(typeInferenceEnv: SymbolTypeInferenceEnvironment[T, U]) = {
      val (typeInferenceEnv2, res) = typeInferenceEnv.definedTypeFromTypeTerm(typeTerm)
      res.map {
        dt =>
          val (typeInferenceEnv4, res2) = checkConstructInferringTypeS(InferringType(dt.term))(typeInferenceEnv2) match {
            case (typeInferenceEnv3, typ: InferringType[GlobalSymbol]) =>
              (typeInferenceEnv3, (dt, typ).success)
            case (typeInferenceEnv3, noType: NoType[GlobalSymbol])     =>
              (typeInferenceEnv3, noType.failure)
            case (typeInferenceEnv3, _)                                =>
              (typeInferenceEnv3, NoType.fromError[GlobalSymbol](FatalError("uninferring type", none, NoPosition)).failure)
          }
          (typeInferenceEnv4, res2.swap.map { _.withPos(dt.pos) }.swap)
      }.valueOr { nt => (typeInferenceEnv2, nt.failure) }
    }

    private def checkConstructTypeTerm(typeTerm: Term[TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]]) =
      State(checkConstructTypeTermS(typeTerm))
    
    override def addSelectConstructInstanceS(selectConstructInst: SelectConstructInstance[Symbol, TypeLambdaInfo[U, LocalSymbol]])(env: SymbolInstantiationEnvironment[T, U]) =
      selectConstructInst match {
        case SelectConstructInstance(supertype, types, file) =>
          val (typeInferenceEnv5, res11) = env.typeInferenceEnv.withClear {
            typeInferenceEnv =>
              (for {
                res <- State((_: SymbolTypeInferenceEnvironment[T, U]).definedTypeFromTypeTerm(supertype))
                res2 <- checkConstructTypeTerm(types.head)
                res3 <- State({
                  (typeInferenceEnv2: SymbolTypeInferenceEnvironment[T, U]) =>
                    types.tail.foldLeft((typeInferenceEnv2, res2.map { NonEmptyList(_) })) {
                      case ((newTypeInferenceEnv, newRes), typ) =>
                        val (newTypeInferenceEnv2, newRes2) = checkConstructTypeTermS(typ)(newTypeInferenceEnv)
                        (newTypeInferenceEnv2, (newRes |@| newRes2) { (ts, t) => t <:: ts })
                    }.mapElements(identity, _.map { _.reverse })
                })
                res10 <- (res |@| res3) {
                  (definedSupertype, pairs) =>
                    val tmpTypeValueTerm = pairs.tail.foldLeft(pairs.head._1.term) { _ | _._1.term }
                    val tmpType = InferringType(tmpTypeValueTerm)
                    for {
                      _ <- State({
                        (typeInferenceEnv2: SymbolTypeInferenceEnvironment[T, U]) =>
                          (pairs.foldLeft(typeInferenceEnv2) {
                            case (newTypeInferenceEnv, (dt, _)) => newTypeInferenceEnv.withDefinedType(dt)
                          }, ())
                      })
                      unifiedSupertype <- State(symbolSimpleTermTypeInferrer.unifyInfosS(InferringType(definedSupertype.term), tmpType)(_: SymbolTypeInferenceEnvironment[T, U]))
                      res9 <- unifiedSupertype match {
                        case noType: NoType[GlobalSymbol] =>
                          State((_: SymbolTypeInferenceEnvironment[T, U], noType.withPos(definedSupertype.pos).failure))
                        case _                            =>
                          for {
                            res4 <- State((typeInferenceEnv2: SymbolTypeInferenceEnvironment[T, U]) => checkDefinedTypesS(typeInferenceEnv2.definedTypes)(typeInferenceEnv2))
                            res8 <- res4.swap.map { _.withPos(definedSupertype.pos) }.swap.map {
                              _ =>
                                for {
                                  res5 <- State(InferringType(definedSupertype.term).instantiatedTypeValueTermWithKindsS(_: SymbolTypeInferenceEnvironment[T, U]))
                                  res7 <- res5.map {
                                    case (supertypeValueTerm, supertypeArgKinds) =>
                                      val selectInstPair = (InferredType(supertypeValueTerm, supertypeArgKinds), SelectInstance[GlobalSymbol](pairs.size, definedSupertype.pos, file))
                                      State({
                                        (typeInferenceEnv2: SymbolTypeInferenceEnvironment[T, U]) =>
                                          val (typeInferenceEnv3, res6) = pairs.toList.zipWithIndex.foldLeft((typeInferenceEnv2, Seq[(InferredType[GlobalSymbol], ConstructInstance[GlobalSymbol])]().success[NoType[GlobalSymbol]])) {
                                            case ((newTypeInfernceEnv, Success(newConstructInstPairs)), ((definedType, _), i)) =>
                                              val (newTypeInfernceEnv2, newRes) = InferringType(definedType.term).instantiatedTypeValueTermWithKindsS(newTypeInfernceEnv)
                                              (newTypeInfernceEnv2, newRes.map { 
                                                case (typeValueTerm, argKinds) =>
                                                  newConstructInstPairs :+ ((InferredType(typeValueTerm, argKinds), ConstructInstance[GlobalSymbol](i, definedType.pos, file)))
                                              })
                                          }
                                          (typeInferenceEnv3, res6.map { (selectInstPair, _) })
                                      })
                                  }.valueOr { nt => State((_: SymbolTypeInferenceEnvironment[T, U], nt.failure)) }
                                } yield res7
                            }.valueOr { nt => State((_: SymbolTypeInferenceEnvironment[T, U], nt.failure)) }
                          } yield res8
                      }
                    } yield res9
                }.valueOr { nt => State((_: SymbolTypeInferenceEnvironment[T, U], nt.failure)) }
              } yield res10).run(typeInferenceEnv)
          }
          val env2 = env.withTypeInferenceEnv(typeInferenceEnv5)
          val (env4, res13) = resultFromTypeResult(res11.swap.map { _.withPos(NoPosition) }.swap).map {
            case ((selectInstType, selectInst), constructInstTriples) =>
              val (env3, res12) = addGlobalInstanceS(SelectFunction, selectInstType, selectInst)(env2)
              constructInstTriples.foldLeft((env3, resultWithPos(res12, selectInst.pos))) {
                case ((newEnv, newRes), (constructInstType, constructInst)) =>
                  val (newEnv2, newRes2) = addGlobalInstanceS(ConstructFunction, constructInstType, constructInst)(newEnv)
                  (newEnv2, newRes |+| resultWithPos(newRes2, constructInst.pos))
              }
          }.valueOr { es => (env, es.failure) }
          (env4, resultForFile(res13, file))
      }
    
    override def addPolyCombinatorsS(locs: Set[GlobalSymbol])(env: SymbolInstantiationEnvironment[T, U]) =
      (env.withPolyCombs(locs), ())
    
    override def withSaveS[V, W](f: SymbolInstantiationEnvironment[T, U] => (SymbolInstantiationEnvironment[T, U], Validation[V, W]))(env: SymbolInstantiationEnvironment[T, U]) =  {
      val (env2, res) = f(env)
      res.map { x => (env2, x.success) }.valueOr { e => (env, e.failure ) }        
    }
  }
  
  implicit def symbolCombinatorInstanceRecursiveInitialzer[T, U]: RecursiveInitializer[NonEmptyList[AbstractError], GlobalSymbol, AbstractCombinator[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]], CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol], SymbolInstantiationEnvironment[T, U]] = new RecursiveInitializer[NonEmptyList[AbstractError], GlobalSymbol, AbstractCombinator[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]], CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol], SymbolInstantiationEnvironment[T, U]] {
    override def combinatorFromNode(node: CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]) = node.comb
    
    override def recursiveCombinatorsFromNode(node: CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]) = node.recursiveCombSyms
    
    override def markedRecursiveCombinatorsFromNode(node: CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]) = node.markedRecCombSyms
    
    override def createNode(comb: AbstractCombinator[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]], recursiveCombLocs: Set[GlobalSymbol], markedRecCombLocs: Set[GlobalSymbol]) =
      CombinatorNode(comb, recursiveCombLocs, markedRecCombLocs)
    
    override def addNodeS(loc: GlobalSymbol, node: CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol])(env: SymbolInstantiationEnvironment[T, U]): (SymbolInstantiationEnvironment[T, U], Unit) =
      (env.withComb(loc, node), ())
    
    override def isRecursiveFromEnvironmentS(env: SymbolInstantiationEnvironment[T, U]) = (env, env.isRecursive)
    
    override def isUninitializedGlobalVarS(loc: GlobalSymbol)(env: SymbolInstantiationEnvironment[T, U]) = 
      (env, !(env.instArgs.contains(loc) && !env.uninitializedCombSyms.contains(loc)))
    
    private def failInitializationS(errs: NonEmptyList[AbstractError], locs: Set[GlobalSymbol])(env: SymbolInstantiationEnvironment[T, U]) =
      if(errs.toList.forall { _.isInstanceOf[Error] })
        (env.withErrs(errs).withUninitializedCombSyms(env.uninitializedCombSyms -- locs), ().successNel)
      else
        (env, errs.failure)
      
    override def nonRecursivelyInitializeGlobalVarS(loc: GlobalSymbol, comb: AbstractCombinator[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]])(env: SymbolInstantiationEnvironment[T, U]) =
      if(!env.isRecursive) {
        val (env2, res) = comb match {
          case Combinator(_, _, body, lambdaInfo, file) =>
            val lambdaInfos = Map(some(loc) -> (preinstantiationLambdaInfosFromTerm(body).mapValues { _.copy(file = file) } + (0 -> PreinstantiationLambdaInfo.fromLambdaInfo(lambdaInfo))))
            instantiatePolyFunctionsS(lambdaInfos)(some(InstanceTree.empty))(env)
          case PolyCombinator(_, _)                     =>
            env.typeInferenceEnv.varType(loc) match {
              case typ: InferredType[GlobalSymbol] =>
                (env.withInstArgs(env.instArgs + (loc -> Seq(InstanceArg(PolyFunction(loc), typ)))), ().successNel)
              case noType: NoType[GlobalSymbol]    =>
                (env, resultFromTypeResult(noType.failure))
              case _                               =>
                (env, FatalError("uninferred type", none, NoPosition).failureNel)
            }
        }
        res.map { 
          u => (env2.withUninitializedCombSyms(env2.uninitializedCombSyms - loc), u.successNel)
        }.valueOr { failInitializationS(_, Set(loc))(env2) }
      } else
        (env, ().successNel)
    
    override def checkInitializationS(res: ValidationNel[AbstractError, Unit], combLocs: Set[GlobalSymbol], oldNodes: Map[GlobalSymbol, CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]])(env: SymbolInstantiationEnvironment[T, U]) = {
      val lambdaInfos = oldNodes.map {
        case (loc, oldNode) =>
          oldNode.comb match {
            case Combinator(_, _, body, lambdaInfo, file) =>
              (some(loc), preinstantiationLambdaInfosFromTerm(body).mapValues { _.copy(file = file) } + (0 -> PreinstantiationLambdaInfo.fromLambdaInfo(lambdaInfo)))
            case PolyCombinator(_, _)            =>
              (some(loc), Map[Int, PreinstantiationLambdaInfo[GlobalSymbol, GlobalSymbol]]())
          }
      }
      val (env2, res4) = (for {
        res2 <- State(instantiatePolyFunctionsS(lambdaInfos)(some(InstanceTree.empty))(_: SymbolInstantiationEnvironment[T, U]))
        res3 <- res2.map {
          _ => State(instantiateRecursivePolyFunctionsS(oldNodes.keySet, lambdaInfos)(_: SymbolInstantiationEnvironment[T, U]))
        }.valueOr { errs => State((_: SymbolInstantiationEnvironment[T, U], errs.failure)) }
      } yield (res3)).run(env)
      val (env3, res5) = (res |@| res4) { (_, _) => (env2.withUninitializedCombSyms(env2.uninitializedCombSyms -- oldNodes.keySet), ().successNel) }.valueOr { es => (env2, es.failure) }
      res5.map { u => (env3, u.successNel) }.valueOr { failInitializationS(_, oldNodes.keySet)(env3) }
    }
    
    override def nodesFromEnvironmentS(env: SymbolInstantiationEnvironment[T, U]) = (env, env.combNodes)
    
    override def withRecursiveS[V](combLocs: Set[GlobalSymbol], newNodes: Map[GlobalSymbol, CombinatorNode[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]], GlobalSymbol]])(f: SymbolInstantiationEnvironment[T, U] => (SymbolInstantiationEnvironment[T, U], V))(env: SymbolInstantiationEnvironment[T, U]): (SymbolInstantiationEnvironment[T, U], V) = {
      val (env2, res) = f(env.withRecursive(true).withInstArgs(env.instArgs -- combLocs).withLambdaInfos(env.lambdaInfos -- combLocs.map(some)).withRecursiveCombSyms(combLocs))
      (env2.withRecursive(false).withCombNodes(newNodes).withRecursiveCombSyms(combLocs), res)
    }
    
    override def withClearS[V](f: SymbolInstantiationEnvironment[T, U] => (SymbolInstantiationEnvironment[T, U], V))(env: SymbolInstantiationEnvironment[T, U]) =
      f(env)
  }
  
  implicit def symbolCombinatorInstanceInitializer[T, U]: Initializer[NonEmptyList[AbstractError], GlobalSymbol, AbstractCombinator[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]], SymbolInstantiationEnvironment[T, U]] = new Initializer[NonEmptyList[AbstractError], GlobalSymbol, AbstractCombinator[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]], SymbolInstantiationEnvironment[T, U]] {
    override def globalVarsFromEnvironmentS(env: SymbolInstantiationEnvironment[T, U]) = (env, env.instArgs.keySet)
    
    override def usedGlobalVarsFromCombinator(comb: AbstractCombinator[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]]) =
      comb match {
        case Combinator(_, _, body, _, _) => usedGlobalVarsFromTerm(body)
        case PolyCombinator(_, _)         => Set()
      }
    
    override def prepareGlobalVarS(loc: GlobalSymbol)(env: SymbolInstantiationEnvironment[T, U]) =
      (env.withInstArgs(env.instArgs + (loc -> Nil)).withUninitializedCombSyms(env.uninitializedCombSyms + loc), ())
    
    override def initializeGlobalVarS(loc: GlobalSymbol, comb: AbstractCombinator[Symbol, typer.LambdaInfo[T, LocalSymbol, GlobalSymbol], TypeSimpleTerm[Symbol, TypeLambdaInfo[U, LocalSymbol]]])(env: SymbolInstantiationEnvironment[T, U]) = {
      val (env2, res) = recursivelyInitializeGlobalVarS(loc, comb)(resolver.TreeInfo(Tree(Map[GlobalSymbol, AbstractTypeCombinator[Symbol, TypeLambdaInfo[U, LocalSymbol]]](), resolver.TypeTreeInfo), Map(), Nil))(env)
      (env2, resultForFile(res, comb.file))
    }
    
    override def checkEnvironmentS(env: SymbolInstantiationEnvironment[T, U]) = {
      val errs = env.globalInstTree.insts.flatMap {
        case PolyFunInstance(loc, pos, file) =>
          env.instArgs.getOrElse(loc, Seq()).map {
            ia => Error("combinator " + loc + " requires instance for " + ia.polyFun + " with type " +ia.typ, file, pos): AbstractError
          }
        case _                               =>
          Nil
      }
      (env, (env.errs ++ errs).toNel.toFailure(()))
    }
    
    override def undefinedGlobalVarError = NonEmptyList(FatalError("undefined global variable", none, NoPosition))
    
    override def withSaveS[V, W](f: SymbolInstantiationEnvironment[T, U] => (SymbolInstantiationEnvironment[T, U], Validation[V, W]))(env: SymbolInstantiationEnvironment[T, U]) =  {
      val (env2, res) = f(env)
      res.map { x => (env2, x.success) }.valueOr { e => (env, e.failure ) }        
    }
  }
  
  implicit def symbolInstantiationEnvironmental[T, U]: InstantiationEnvironmental[SymbolInstantiationEnvironment[T, U], GlobalSymbol, GlobalSymbol] = new InstantiationEnvironmental[SymbolInstantiationEnvironment[T, U], GlobalSymbol, GlobalSymbol] {
    override def copyEnvironment(env: SymbolInstantiationEnvironment[T, U]) = env
    
    override def getLambdaInfoFromEnvironment(env: SymbolInstantiationEnvironment[T, U])(lambdaIdx: Int) = env.currentLambdaInfos.get(lambdaIdx)
    
    override def withCurrentCombinatorLocation(env: SymbolInstantiationEnvironment[T, U])(loc: Option[GlobalSymbol]) = env.withCurrentCombSym(loc)
    
    override def treeGlobalInstanceTreeFromEnvironment(env: SymbolInstantiationEnvironment[T, U]) =
      InstanceTree.fromInstanceTables(env.globalInstTree.instTables.map { case (pf, it) => (pf, env.firstGlobalInstCounts.get(pf).map { it.withoutFirstInsts(_) }.getOrElse(it)) })
    
    override def instanceArgTableFromFromEnvironment(env: SymbolInstantiationEnvironment[T, U]) = InstanceArgTable(env.instArgs)
  }

  implicit def instanceTableSemigroup[T, U]: Semigroup[InstanceTable[T, U]] = new Semigroup[InstanceTable[T, U]] {
    override def append(f1: InstanceTable[T, U], f2: => InstanceTable[T, U]) =
      InstanceTable[T, U](f1.pairs ++ f2.pairs)
  }
  
  implicit def instanceTreeSemigroup[T, U, V]: Semigroup[InstanceTree[T, U, V]] = new Semigroup[InstanceTree[T, U, V]] {
    override def append(f1: InstanceTree[T, U, V], f2: => InstanceTree[T, U, V]) =
      InstanceTree.fromInstanceTables(f1.instTables |+| f2.instTables)
  }
}
