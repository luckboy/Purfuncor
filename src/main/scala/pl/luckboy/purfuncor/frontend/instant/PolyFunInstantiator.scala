/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.instant
import scala.collection.immutable.IntMap
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbolTabular
import pl.luckboy.purfuncor.frontend.kinder.InferredKind
import pl.luckboy.purfuncor.frontend.typer.TypeBuiltinFunction
import pl.luckboy.purfuncor.frontend.typer.DefinedType
import pl.luckboy.purfuncor.frontend.typer.Type
import pl.luckboy.purfuncor.frontend.typer.NoType
import pl.luckboy.purfuncor.frontend.typer.InferredType
import pl.luckboy.purfuncor.frontend.typer.InferringType
import pl.luckboy.purfuncor.frontend.typer.TypeValueTerm
import pl.luckboy.purfuncor.frontend.typer.BuiltinType
import pl.luckboy.purfuncor.frontend.typer.TupleType
import pl.luckboy.purfuncor.frontend.typer.Unittype
import pl.luckboy.purfuncor.frontend.typer.Grouptype
import pl.luckboy.purfuncor.frontend.typer.TypeParamApp
import pl.luckboy.purfuncor.frontend.typer.GlobalTypeApp
import pl.luckboy.purfuncor.frontend.typer.TypeConjunction
import pl.luckboy.purfuncor.frontend.typer.TypeDisjunction
import pl.luckboy.purfuncor.frontend.typer.TypeValueLambda
import pl.luckboy.purfuncor.frontend.typer.TypeMatching
import pl.luckboy.purfuncor.frontend
import pl.luckboy.purfuncor.common.Inferrer._
import pl.luckboy.purfuncor.frontend.typer.TypeInferrer._
import pl.luckboy.purfuncor.frontend.typer.TypeValueTermUnifier._
import pl.luckboy.purfuncor.frontend.typer.TypeValueTermUtils._

trait PolyFunInstantiator[L, M, N, I, E]
{
  def instantiatePolyFunctionS(lambdaInfo: PreinstantiationLambdaInfo[L, N], instArgs: Seq[InstanceArg[L, N]])(localInstTree: Option[InstanceTree[AbstractPolyFunction[L], N, LocalInstance[L]]])(env: E): (E, ValidationNel[AbstractError, (InstantiationLambdaInfo[L], Option[InstanceTree[AbstractPolyFunction[L], N, LocalInstance[L]]])])
  
  def getLambdaInfosFromEnvironmentS(loc: Option[L])(env: E): (E, Option[Map[Int, InstantiationLambdaInfo[L]]])
  
  def addLambdaInfosS(loc: Option[L], lambdaInfos: Map[Int, InstantiationLambdaInfo[L]])(env: E): (E, Unit)

  def getInstanceArgsFromEnvironmentS(loc: L)(env: E): (E, Option[Seq[InstanceArg[L, N]]])
  
  def addInstanceArgsS(loc: L, instArgs: Seq[InstanceArg[L, N]])(env: E): (E, Unit)
  
  def addInstanceS(loc: L, inst: frontend.Instance[L])(env: E): (E, ValidationNel[AbstractError, Unit])
  
  def addSelectConstructInstanceS(selectConstructInst: SelectConstructInstance[M, I])(env: E): (E, ValidationNel[AbstractError, Unit])

  def addPolyCombinatorsS(locs: Set[L])(env: E): (E, Unit)
  
  def withSaveS[T, U](f: E => (E, Validation[T, U]))(env: E): (E, Validation[T, U])
}

object PolyFunInstantiator {
  def instantiatePolyFunctionsS[L, M, N, I, E](lambdaInfoMaps: Map[Option[L], Map[Int, PreinstantiationLambdaInfo[L, N]]])(localInstTree: Option[InstanceTree[AbstractPolyFunction[L], N, LocalInstance[L]]])(env: E)(implicit polyFunInstantiator: PolyFunInstantiator[L, M, N, I, E]) = {
    val (env2, res) = justInstantiatePolyFunctionsS(lambdaInfoMaps)(localInstTree)(env)
    res.map {
      case (lambdaInfoMaps2, localInstTree) =>
        val (env3, _) = lambdaInfoMaps2.foldLeft((env2, ())) {
          case ((newEnv, _), (l, lis)) => polyFunInstantiator.addLambdaInfosS(l, lis)(newEnv)
        }
        localInstTree.map {
          tmpInstTree =>
            combinatorInstanceArgsS(lambdaInfoMaps)(tmpInstTree).map {
              instArgs =>
                val (env4, _) = instArgs.foldLeft((env3, ())) {
                  case ((newEnv, _), (l, ias)) => polyFunInstantiator.addInstanceArgsS(l, ias)(newEnv)
                }
                (env4, ().successNel)
            }.valueOr { errs => (env2, errs.failure) }
        }.getOrElse((env3, ().successNel))
    }.valueOr { errs => (env2, errs.failure) }
  }
  
  private def instantiatePolyFunctions2[L, M, N, I, E](lambdaInfoMaps: Map[Option[L], Map[Int, PreinstantiationLambdaInfo[L, N]]])(localInstTree: Option[InstanceTree[AbstractPolyFunction[L], N, LocalInstance[L]]])(implicit polyFunInstantiator: PolyFunInstantiator[L, M, N, I, E]) =
    State(instantiatePolyFunctionsS[L, M, N, I, E](lambdaInfoMaps)(localInstTree))

  def instantiatePolyFunctions[L, M, N, I, E](lambdaInfoMaps: Map[Option[L], Map[Int, PreinstantiationLambdaInfo[L, N]]])(localInstTree: Option[InstanceTree[AbstractPolyFunction[L], N, LocalInstance[L]]])(implicit polyFunInstantiator: PolyFunInstantiator[L, M, N, I, E]) =
    instantiatePolyFunctions2[L, M, N, I, E](lambdaInfoMaps)(localInstTree)
  
  def instantiateRecursivePolyFunctionsS[L, M, N, I, E](locs: Set[L], lambdaInfoMaps: Map[Option[L], Map[Int, PreinstantiationLambdaInfo[L, N]]])(env: E)(implicit polyFunInstantiator: PolyFunInstantiator[L, M, N, I, E]) =
    locs.foldLeft((env, ().successNel[AbstractError])) {
      case ((newEnv, Success(_)), loc) =>
        lambdaInfoMaps.get(some(loc)).map {
          lambdaInfos =>
            val lambdaIdxs = lambdaInfos.flatMap { 
              case (lambdaIdx, lambdaInfo) =>
                lambdaInfo.polyFun match {
                  case Some(PolyFunction(polyFunLoc)) if locs.contains(polyFunLoc) => some(lambdaIdx)
                  case _                                                           => none[Int]
                }
            }
            val (newEnv2, optLambdaInfos2) = polyFunInstantiator.getLambdaInfosFromEnvironmentS(some(loc))(newEnv)
            optLambdaInfos2.map {
              lambdaInfos2 =>
                val (newEnv3, optInstArgs) = polyFunInstantiator.getInstanceArgsFromEnvironmentS(loc)(newEnv2)
                optInstArgs.map {
                  instArgs =>
                    val lambdaInfos3 = lambdaInfos2 ++ lambdaIdxs.map { 
                      (_, InstantiationLambdaInfo((0 until instArgs.size).map { LocalInstance(_) }))
                    }
                    polyFunInstantiator.addLambdaInfosS(some(loc), lambdaInfos3)(newEnv3).mapElements(identity, _.successNel)
                }.getOrElse((newEnv2, FatalError("no instance arguments", none, NoPosition).failureNel))
            }.getOrElse((newEnv2, FatalError("no lambda inforamtions", none, NoPosition).failureNel))
        }.getOrElse((newEnv, FatalError("no lambda inforamtions", none, NoPosition).failureNel))
      case ((newEnv, Failure(errs)), _) =>
        (newEnv, errs.failure)
    }
  
  def instantiateRecursivePolyFunctions[L, M, N, I, E](locs: Set[L], lambdaInfoMaps: Map[Option[L], Map[Int, PreinstantiationLambdaInfo[L, N]]])(implicit polyFunInstantiator: PolyFunInstantiator[L, M, N, I, E], locEqual: Equal[L]) =
    State(instantiateRecursivePolyFunctionsS[L, M, N, I, E](locs, lambdaInfoMaps))
  
  private def justInstantiatePolyFunctionsS[L, M, N, I, E](lambdaInfoMaps: Map[Option[L], Map[Int, PreinstantiationLambdaInfo[L, N]]])(localInstTree: Option[InstanceTree[AbstractPolyFunction[L], N, LocalInstance[L]]])(env: E)(implicit polyFunInstantiator: PolyFunInstantiator[L, M, N, I, E]) = {
    val (env2, (res, localInstTree2)) = lambdaInfoMaps.foldLeft((env, (Map[Option[L], Map[Int, InstantiationLambdaInfo[L]]]().successNel[AbstractError], localInstTree))) {
      case ((newEnv, (newRes, newLocalInstTree)), (polyFun, lambdaInfos)) =>
        val (newEnv6, (newRes5, newLocalInstTree3)) = lambdaInfos.foldLeft((newEnv, (IntMap[InstantiationLambdaInfo[L]]().successNel[AbstractError], newLocalInstTree))) {
          case ((newEnv2, (newRes2, newLocalInstTree2)), (i, lambdaInfo)) =>
            val (newEnv3, newRes3) = lambdaInfo.polyFun.map {
              polyFun =>
                polyFun match {
                  case PolyFunction(polyFunLoc) =>
                    polyFunInstantiator.getInstanceArgsFromEnvironmentS(polyFunLoc)(newEnv2).mapElements(identity, _.toSuccess(NonEmptyList(FatalError("undefined global variable", none, NoPosition))))
                  case _                        =>
                    (newEnv2, Seq().successNel)
                }
            }.getOrElse((newEnv2, Seq().successNel))
            val (newEnv4, newRes4) = newRes3.map {
              polyFunInstantiator.instantiatePolyFunctionS(lambdaInfo, _)(newLocalInstTree2)(newEnv3)
            }.valueOr { errs => (newEnv3, errs.failure) }
            (newEnv4, ((newRes2 |@| newRes4) { case (lis, (li, _)) => lis + (i -> li) }, newRes4.map { _._2 }.getOrElse(newLocalInstTree2)))
        }
        (newEnv6, ((newRes |@| newRes5) { (liMaps, lis) => liMaps + (polyFun -> lis) }, newLocalInstTree3))
    }
    (env2, res.map { (_, localInstTree2).successNel }.valueOr { _.failure })
  }

  private def instanceArgsFromLocalInstanceTree[L, M](localInstTree: InstanceTree[AbstractPolyFunction[L], M, LocalInstance[L]]) = {
    val instArgs = localInstTree.instGroupTables.flatMap {
      case (pf, igt) => igt.instGroups.values.flatMap { _.pairs.map { case (t, LocalInstance(i)) => (i, InstanceArg(pf, t.typ)) } }
    }
    (0 until instArgs.size).foldLeft(some(Vector[InstanceArg[L, M]]())) {
      (optIas, i) => optIas.flatMap { ias => instArgs.get(i).map { ias :+ _ } }
    }.toSuccess(NonEmptyList(FatalError("index of out bounds", none, NoPosition)))
  }

  private def normalizeInstanceArgs[L, N](instArgs: Seq[InstanceArg[L, N]], params: Map[Int, Int]) = 
    instArgs.foldLeft(Vector[InstanceArg[L, N]]().successNel[AbstractError]) {
      case (Success(newInstArgs), InstanceArg(polyFun, typ)) =>
        val typeValueTerm = normalizeTypeParamsForParams(typ.typeValueTerm, params.size)(params)
        val argKindMap = params.foldLeft(IntMap[InferredKind]()) {
          case (ks, (p, p2)) => typ.argKinds.lift(p).map { k => ks + (p2 -> k) }.getOrElse(ks)
        }
        val argKinds = (0 until params.size).foldLeft(Vector[InferredKind]()) {
          case (ks, i) => ks :+ argKindMap.getOrElse(i, InferredKind(Star(KindParam(0), NoPosition)))
        }
        (newInstArgs :+ InstanceArg(polyFun, InferredType(typeValueTerm, argKinds))).successNel
      case (Failure(errs), _)                                =>
        errs.failure
    }
  
  private def combinatorInstanceArgsS[L, N](lambdaInfoMaps: Map[Option[L], Map[Int, PreinstantiationLambdaInfo[L, N]]])(localInstTree: InstanceTree[AbstractPolyFunction[L], N, LocalInstance[L]]) =
    instanceArgsFromLocalInstanceTree(localInstTree).flatMap {
      instArgs =>
        lambdaInfoMaps.foldLeft(Map[L, Vector[InstanceArg[L, N]]]().successNel[AbstractError]) {
          case (Success(newInstArgs), (Some(loc), lambdaInfos)) =>
            lambdaInfos.get(0).map {
              lambdaInfo =>
                normalizeInstanceArgs(instArgs, lambdaInfo.combTypeParams).map {
                  instArgs2 => newInstArgs + (loc -> instArgs2)
                }
            }.getOrElse(FatalError("no lambda information", none, NoPosition).failureNel)
          case (Success(newLambdaInfoMaps), (None, _)) =>
            newLambdaInfoMaps.successNel
          case (Failure(errs), _) =>
            errs.failure
        }
    }
  
  private def instanceArgsFromPreinstantiationLambdaInfoS[L, N, E](lambdaInfo: PreinstantiationLambdaInfo[L, N], instArgs: Seq[InstanceArg[L, N]])(env: E)(implicit unifier: Unifier[NoType[N], TypeValueTerm[N], E, Int], envSt: typer.TypeInferenceEnvironmentState[E, L, N], envSt2: TypeInferenceEnvironmentState[E, L, N]) =
    lambdaInfo.polyFun.map {
      case PolyFunction(polyFunLoc) =>
        lambdaInfo.polyFunType.map {
          polyFunType =>
            (for {
              typ <- State(envSt2.globalVarTypeFromEnvironmentS(polyFunLoc)(_: E))
              res <- State(typ.uninstantiatedTypeValueTermWithTypeParamsS(_: E))
              res11 <- res.map {
                case (inferringTypeValueTerm, typeParams) =>
                  for {
                    res2 <- State({
                      (env2: E) =>
                        instArgs.foldLeft((env2, Vector[InferringType[N]]().success[NoType[N]])) {
                          case ((newEnv, Success(newInstTypes)), instArg) =>
                            val (newEnv2, newRes) = instArg.typ.uninstantiatedTypeValueTermWithTypeParamsS(newEnv)
                            newRes.map {
                              case (instInferringTypeValueTerm, instTypeParams) =>
                                val (newEnv4, newRes2) = typeParams.foldLeft((newEnv2, ().success[NoType[N]])) {
                                  case ((newEnv3, _), (param, param2)) =>
                                    instTypeParams.get(param).map {
                                      unifier.unionParamsS(param2, _)(newEnv3).mapElements(identity, _.map { _ => () })
                                    }.getOrElse((newEnv3, ().success))
                                }
                                (newEnv4, newRes2.map { _ => newInstTypes :+ InferringType(instInferringTypeValueTerm) })
                            }.valueOr { nt => (newEnv2, nt.failure) }
                          case ((newEnv, Failure(noType)), _) =>
                            (newEnv, noType.failure)
                        }
                    })
                    res10 <- res2.map {
                      instInferringTypes =>
                        for {
                          res3 <- State(polyFunType.uninstantiatedTypeValueTermWithTypeParamsS(_: E))
                          res9 <- res3.map {
                            case (polyFunTypeValueTerm, polyFunTypeParams) =>
                              for {
                                _ <- State(envSt.setCurrentTypeMatchingS(TypeMatching.Types)(_: E))
                                unifiedType <- unifyTypes(InferringType(inferringTypeValueTerm), InferringType(polyFunTypeValueTerm))
                                res8 <- (unifiedType match {
                                  case noType: NoType[N] =>
                                    State((_: E, NoType.fromError[N](FatalError("mismatched types", none, NoPosition)).failure))
                                  case _ =>
                                    for {
                                      res4 <- State(envSt2.reverseTypeParamMapS(polyFunTypeParams)(_: E))
                                      res7 <- res4.map {
                                        reversedPolyFunTypeParamMap =>
                                          for {
                                            res5 <- State({
                                              (env2: E) =>
                                                polyFunType match {
                                                 case InferredType(_, argKinds) =>
                                                   argKinds.zipWithIndex.foldLeft((env2, IntMap[(Int, InferredKind)]().success[NoType[N]])) {
                                                     case ((newEnv, Success(newParamsWithKinds)), (kind, i)) =>
                                                       if(reversedPolyFunTypeParamMap.values.toSet.contains(i))
                                                         (newEnv, newParamsWithKinds.success)
                                                       else
                                                         unifier.allocateParamS(newEnv).mapElements(identity, _.map { p => newParamsWithKinds + (i -> ((p, kind))) } )
                                                     case ((newEnv, Failure(noType)), _)                     =>
                                                       (newEnv, noType.failure)
                                                   }
                                                }
                                            })
                                            res6 <- res5.map {
                                              polyFunTypeParamsWithKinds =>
                                                val reversedPolyFunTypeParamMap2 = reversedPolyFunTypeParamMap ++ polyFunTypeParamsWithKinds.mapValues { _._1 }.map { _.swap }
                                                State({
                                                  (env2: E) =>
                                                    val (env3, _) = envSt.setTypeParamKindsS(polyFunTypeParamsWithKinds.map { _._2 }.toMap)(env2)
                                                    instArgs.zip(instInferringTypes).foldLeft((env3, Vector[InstanceArg[L, N]]().success[NoType[N]])) {
                                                      case ((newEnv, Success(newInstArgs)), (instArg, instInferringType)) =>
                                                        val (newEnv2, newInstType) = instInferringType.instantiatedTypeForParamsS(reversedPolyFunTypeParamMap2)(newEnv)
                                                          newInstType match {
                                                            case newInstInferredType: InferredType[N] =>
                                                              (newEnv2, (newInstArgs :+ instArg.copy(typ = newInstInferredType)).success)
                                                            case noType: NoType[N] =>
                                                              (newEnv2, noType.failure)
                                                            case _ =>
                                                              (newEnv2, NoType.fromError[N](FatalError("uninferred type", none, NoPosition)).failure)
                                                         }
                                                    }
                                                })
                                            }.valueOr { nt => State((_: E, nt.failure)) }
                                          } yield res6
                                      }.valueOr { nt => State((_: E, nt.failure)) }
                                    } yield res7
                                })
                              } yield res8
                          }.valueOr { nt => State((_: E, nt.failure)) }
                        } yield res9
                    }.valueOr { nt => State((_: E, nt.failure)) }
                  } yield res10
              }.valueOr { nt => State((_: E, nt.failure)) }
            } yield res11).run(env)
        }.getOrElse((env, Seq().success))
      case polyFun @ (ConstructFunction | SelectFunction) =>
        (env, lambdaInfo.polyFunType.map { pft => Seq(InstanceArg(polyFun, pft)).success }.getOrElse(NoType.fromError[N](FatalError("no polymorphic function type", none, NoPosition)).failure))
    }.getOrElse((env, Seq().success))
  
  def instantiatePolyFunctionS[L, N, E](lambdaInfo: PreinstantiationLambdaInfo[L, N], instArgs: Seq[InstanceArg[L, N]], globalInstTree: InstanceTree[AbstractPolyFunction[L], N, GlobalInstance[L]])(localInstTree: Option[InstanceTree[AbstractPolyFunction[L], N, LocalInstance[L]]])(env: E)(implicit unifier: Unifier[NoType[N], TypeValueTerm[N], E, Int], envSt: typer.TypeInferenceEnvironmentState[E, L, N], envSt2: TypeInferenceEnvironmentState[E, L, N], groupIdentEqual: Equal[GroupIdentity[N]]) =
    envSt2.withInstanceTypeClearingS {
      newEnv =>
        val (newEnv2, newRes) = instanceArgsFromPreinstantiationLambdaInfoS(lambdaInfo, instArgs)(newEnv)
        val (newEnv7, newRes5) = newRes.map {
          instArgs =>
            instArgs.foldLeft((newEnv2, (Vector[Instance[L]]().success[NoType[N]], localInstTree))) {
              case ((newEnv3, (newRes, newLocalInstTree)), instArg @ InstanceArg(polyFun, typ)) =>
                val (newEnv4, newRes2) = globalInstTree.findInstsS(polyFun, GlobalInstanceType(typ))(newEnv3)
                val (newEnv6, (newRes3, newLocalInstTree2)) = newRes2.map {
                  case Seq(inst) =>
                    (newEnv4, (Seq(inst).success, newLocalInstTree))
                  case insts if lambdaInfo.isCase && insts.size > 1 =>
                    (newEnv4, (insts.success, newLocalInstTree))
                  case insts if insts.size > 1 =>
                    envSt2.ambiguousInstanceNoTypeS(instArg)(newEnv4).mapElements(identity, nt => (nt.failure, newLocalInstTree))
                  case insts =>
                    newLocalInstTree.map {
                      tmpInstTree =>
                        val inst = LocalInstance[L](tmpInstTree.instCount)
                        val (newEnv5, newRes4) = tmpInstTree.addInstS(polyFun, LocalInstanceType(typ), inst)(newEnv4)
                        newRes4.map {
                          _.map {
                            case (it, i) => (newEnv5, (i.map { Seq(_) }.getOrElse(Seq(inst)).success, some(it)))
                          }.getOrElse {
                            envSt2.ambiguousInstanceNoTypeS(instArg)(newEnv4).mapElements(identity, nt => (nt.failure, newLocalInstTree))
                          }
                        }.valueOr { nt => (newEnv5, (nt.failure, newLocalInstTree)) }
                    }.getOrElse {
                      envSt2.notFoundInstanceNoTypeS(instArg)(newEnv4).mapElements(identity, nt => (nt.failure, newLocalInstTree))
                    }
                }.valueOr { nt => (newEnv4, (nt.failure, newLocalInstTree)) }
                (newEnv6, ((newRes |@| newRes3) { case (is, is2) => is ++ is2 }, newLocalInstTree2))
            }.mapElements(identity, p => p._1.map { is => (InstantiationLambdaInfo(is), p._2) })
        }.valueOr { nt => (newEnv2, nt.failure) }
        (newEnv7, newRes5.swap.map { _.withPos(lambdaInfo.pos).forFile(lambdaInfo.file) }.swap)
    } (env)
    
  private def findTupleTypesS[L, N, E](term: TypeValueTerm[N])(err: E => (E, NoType[N]))(env: E)(implicit unifier: Unifier[NoType[N], TypeValueTerm[N], E, Int], envSt: typer.TypeInferenceEnvironmentState[E, L, N]): (E, Validation[NoType[N], Seq[TupleType[N]]]) =
    term match {
      case tupleType @ TupleType(_) =>
        (env, Seq(tupleType).success)
      case TypeConjunction(terms) =>
        terms.foldLeft((env, Vector[TupleType[N]]().success[NoType[N]])) {
          case ((newEnv, Success(newTerms)), term2) => 
            findTupleTypesS(term2)(err)(newEnv) match {
              case (newEnv2, Success(terms2)) => (newEnv2, (newTerms ++ terms2).success)
              case (newEnv2, Failure(noType)) => (newEnv2, noType.failure)
            }
          case ((newEnv, newRes), _)             =>
            (newEnv, newRes)
        }
      case TypeDisjunction(terms) =>
        terms.foldLeft((env, Vector[TupleType[N]]().success[NoType[N]])) {
          case ((newEnv, Success(newTerms)), term2) =>
            findTupleTypesS(term2)(err)(newEnv) match {
              case (newEnv2, Success(Seq()))  => err(newEnv).mapElements(identity, _.failure)
              case (newEnv2, Success(terms2)) => (newEnv2, (newTerms ++ terms2).success)
              case (newEnv2, Failure(noType)) => (newEnv2, noType.failure)
            }
          case ((newEnv, Failure(noType)), _)       =>
            (newEnv, noType.failure)
        }
      case GlobalTypeApp(loc, args, _) =>
        envSt.withRecursionCheckingS(Set(loc)) {
          env2 =>
            appForGlobalTypeWithAllocatedTypeParamsS(loc, args)(env2) match {
              case (env3, Success(evaluatedTerm)) =>
                findTupleTypesS(evaluatedTerm)(err)(env3)
              case (env3, Failure(noType))        =>
                (env3, noType.failure)
            }
        } (env)
      case _ =>
        (env, Seq().success)
    }

  private val correctTypeBuiltinFunctions = Set(
      TypeBuiltinFunction.Zero,
      TypeBuiltinFunction.NonZero,
      TypeBuiltinFunction.Empty,
      TypeBuiltinFunction.NonEmpty)
    
  private def containsUnittypeS[L, N, E](term: TypeValueTerm[N])(err: E => (E, NoType[N]))(env: E)(implicit unifier: Unifier[NoType[N], TypeValueTerm[N], E, Int], envSt: typer.TypeInferenceEnvironmentState[E, L, N]): (E, Validation[NoType[N], Boolean]) = {
    term match {
      case BuiltinType(bf, _) if correctTypeBuiltinFunctions.contains(bf) =>
        (env, false.success)
      case Unittype(_, _, _) =>
        (env, true.success)
      case TupleType(_) =>
        (env, false.success)
      case TypeParamApp(_, _, _) =>
        err(env).mapElements(identity, _.failure)
      case TypeConjunction(terms) =>
        terms.foldLeft((env, false.success[NoType[N]])) {
          case ((newEnv, Success(b)), term2)  =>
            term2 match {
              case BuiltinType(TypeBuiltinFunction.Any, Seq()) =>
                (newEnv, b.success)
              case _                                           =>
                containsUnittypeS(term2)(err)(newEnv).mapElements(identity, _.map { _ | b })
            }
          case ((newEnv, Failure(noType)), _) =>
            (newEnv, noType.failure)
        }
      case TypeDisjunction(terms) =>
        terms.foldLeft((env, true.success[NoType[N]])) {
          case ((newEnv, Success(b)), term2)  => 
            containsUnittypeS(term2)(err)(newEnv) match {
              case (newEnv2, Success(true))  => (newEnv2, true.success)
              case (newEnv2, Success(false)) => err(newEnv2).mapElements(identity, _.failure)
              case (newEnv2, Failure(noType)) => (newEnv2, noType.failure)
            }
          case ((newEnv, Failure(noType)), _) => (newEnv, noType.failure)
        }
      case GlobalTypeApp(loc, args, _) =>
        envSt.withRecursionCheckingS(Set(loc)) {
          env2 =>
            appForGlobalTypeWithAllocatedTypeParamsS(loc, args)(env2) match {
              case (env3, Success(evaluatedTerm)) =>
                containsUnittypeS(evaluatedTerm)(err)(env3)
              case (env3, Failure(noType))        =>
                (env3, noType.failure)
            }
        } (env)
      case _ =>
        err(env).mapElements(identity, _.failure)
    }
  }
    
  def checkConstructInferringTypeS[L, N, E](typ: InferringType[N])(env: E)(implicit unifier: Unifier[NoType[N], TypeValueTerm[N], E, Int], envSt: typer.TypeInferenceEnvironmentState[E, L, N], envSt2: TypeInferenceEnvironmentState[E, L, N]) = {
    val (env2, res) = findTupleTypesS(typ.typeValueTerm)(envSt2.incorrectConstructTypeNoTypeS)(env)
    val (env3, res2) = containsUnittypeS(typ.typeValueTerm)(envSt2.incorrectConstructTypeNoTypeS)(env2)
    (for(t <- res; b <- res2) yield (t, b)).map {
      case (typeValueTerms, hasUnittype) =>
        unifier.withSaveS {
          env4 =>
            if(hasUnittype) {
              typeValueTerms.headOption.map {
                firstTypeValueTerm =>
                  (for {
                    _ <- State(envSt2.addDefinedTypeS(DefinedType.fromInferringType(typ))(_: E))
                    savedTypeMatching <- State(envSt.currentTypeMatchingFromEnvironmentS(_: E))
                    _ <- State(envSt.setCurrentTypeMatchingS(TypeMatching.Types)(_: E))
                    unifiedType <- State({
                      (env5: E) =>
                        typeValueTerms.tail.foldLeft((env5, InferringType(firstTypeValueTerm): Type[N])) {
                          case ((newEnv, newType: InferredType[N]), typeValueTerm) =>
                            unifyTypesS(newType, InferringType(typeValueTerm))(newEnv)
                          case ((newEnv, noType: NoType[N]), _)                    =>
                            (newEnv, noType)
                          case ((newEnv, _), _)                                    =>
                            (newEnv, NoType.fromError[N](FatalError("uninferred type", none, NoPosition)))
                        }
                    })
                    _ <- State(envSt.setCurrentTypeMatchingS(savedTypeMatching)(_: E))
                    res4 <- unifiedType match {
                      case inferringType: InferringType[N] =>
                        for {
                          definedTypes <- State(envSt2.definedTypesFromEnvironmentS(_: E))
                          res2 <- checkDefinedTypes(definedTypes)
                          res3 <- res2.map {
                            _ => State((_: E, inferringType.success))
                          }.getOrElse(State((_: E, inferringType.success)))
                        } yield res3
                      case _: NoType[N]                    =>
                        State(envSt2.incorrectConstructTypeNoTypeS(_: E).mapElements(identity, _.failure))
                      case _                               =>
                        State((_: E, NoType.fromError[N](FatalError("uninferring type", none, NoPosition)).failure))
                    }
                  } yield res4).run(env4)
              }.getOrElse(envSt2.incorrectConstructTypeNoTypeS(env4).mapElements(identity, _.failure))
            } else
              envSt2.incorrectConstructTypeNoTypeS(env4).mapElements(identity, _.failure)
        } (env3).mapElements(identity, _.valueOr(identity))
    }.valueOr { (env3, _) }
  }
  
  def checkConstructInferringType[L, N, E](typ: InferringType[N])(implicit unifier: Unifier[NoType[N], TypeValueTerm[N], E, Int], envSt: typer.TypeInferenceEnvironmentState[E, L, N], envSt2: TypeInferenceEnvironmentState[E, L, N]) =
    State(checkConstructInferringTypeS[L, N, E](typ))
    
  private val groupTypeBuiltinFunctions = Map(
      TypeBuiltinFunction.Boolean ->  GroupTypeBuiltinFunction.Boolean,
      TypeBuiltinFunction.Char -> GroupTypeBuiltinFunction.Char,
      TypeBuiltinFunction.Byte -> GroupTypeBuiltinFunction.Byte,
      TypeBuiltinFunction.Short -> GroupTypeBuiltinFunction.Short,
      TypeBuiltinFunction.Int -> GroupTypeBuiltinFunction.Int,
      TypeBuiltinFunction.Long -> GroupTypeBuiltinFunction.Long,
      TypeBuiltinFunction.Float -> GroupTypeBuiltinFunction.Float,
      TypeBuiltinFunction.Double -> GroupTypeBuiltinFunction.Double,
      TypeBuiltinFunction.Array -> GroupTypeBuiltinFunction.Array,
      TypeBuiltinFunction.Fun -> GroupTypeBuiltinFunction.Fun)
  
  private def groupIdentitiesFromTypeValueTermsS[L, N, E](terms: Seq[TypeValueTerm[N]])(err: E => (E, NoType[N]))(env: E)(implicit unifier: Unifier[NoType[N], TypeValueTerm[N], E, Int], envSt: typer.TypeInferenceEnvironmentState[E, L, N], groupIdentEqual: Equal[GroupIdentity[N]]) =
    terms.foldLeft((env, Vector[GroupIdentity[N]]().success[NoType[N]])) {
      case ((newEnv, Success(groupIdents)), term) =>
        groupIdentityFromTypeValueTermS(term)(err)(newEnv).mapElements(identity, _.map { groupIdents :+ _ })
      case ((newEnv, Failure(noType)), _)         =>
        (newEnv, noType.failure)
    }
  
  private def groupIdentitiesFromTypeValueLambdasS[L, N, E](lambdas: Seq[TypeValueLambda[N]])(err: E => (E, NoType[N]))(env: E)(implicit unifier: Unifier[NoType[N], TypeValueTerm[N], E, Int], envSt: typer.TypeInferenceEnvironmentState[E, L, N], groupIdentEqual: Equal[GroupIdentity[N]]) =
    lambdas.foldLeft((env, Vector[GroupIdentity[N]]().success[NoType[N]])) {
      case ((newEnv, Success(groupIdents)), lambda) =>
        groupIdentityFromTypeValueTermS(lambda.body)(err)(newEnv).mapElements(identity, _.map { groupIdents :+ _ })
      case ((newEnv, Failure(noType)), _)           =>
        (newEnv, noType.failure)
    }
  
  private def groupIdentityFromTypeValueTermS[L, N, E](term: TypeValueTerm[N])(err: E => (E, NoType[N]))(env: E)(implicit unifier: Unifier[NoType[N], TypeValueTerm[N], E, Int], envSt: typer.TypeInferenceEnvironmentState[E, L, N], groupIdentEqual: Equal[GroupIdentity[N]]): (E, Validation[NoType[N], GroupIdentity[N]]) =
    term match {
      case BuiltinType(bf, args) =>
        groupTypeBuiltinFunctions.get(bf).map {
          gtbf => groupIdentitiesFromTypeValueTermsS(args)(err)(env).mapElements(identity, _.map { GroupIdentity(BuiltinTypeGroupNodeIdentity(gtbf), _) })
        }.getOrElse((env, GroupIdentity(DefaultGroupNodeIdentity, Nil).success))
      case Grouptype(loc, args, _) =>
        groupIdentitiesFromTypeValueLambdasS(args)(err)(env).mapElements(identity, _.map { GroupIdentity(GrouptypeGroupNodeIdentity(loc), _) })
      case TypeParamApp(_, _, _) =>
        (env, GroupIdentity(TypeParamAppGroupNodeIdentity, Nil).success[NoType[N]])
      case TypeConjunction(terms) =>
        val (env2, res) = groupIdentitiesFromTypeValueTermsS(terms.toVector)(err)(env)
        res.map {
          groupIdents =>
            groupIdents.headOption.map {
              groupIdent =>
                if(groupIdents.tail.forall { _ === groupIdent }) (env2, groupIdent.success) else err(env2).mapElements(identity, _.failure)
            }.getOrElse((env2, NoType.fromError[N](FatalError("no group identities", none, NoPosition)).failure))
        }.valueOr { nt => (env2, nt.failure) }
      case TypeDisjunction(terms) =>
        val (env2, res) = groupIdentitiesFromTypeValueTermsS(terms.toVector)(err)(env)
        res.map {
          groupIdents =>
            groupIdents.headOption.map {
              groupIdent =>
                if(groupIdents.tail.forall { _ === groupIdent }) (env2, groupIdent.success) else err(env2).mapElements(identity, _.failure)
            }.getOrElse((env2, NoType.fromError[N](FatalError("no group identities", none, NoPosition)).failure))
        }.valueOr { nt => (env2, nt.failure) }
      case GlobalTypeApp(loc, args, _) =>
        envSt.withRecursionCheckingS(Set(loc)) {
          env2 =>
            appForGlobalTypeWithAllocatedTypeParamsS(loc, args)(env2) match {
              case (env3, Success(evaluatedTerm)) =>
                groupIdentityFromTypeValueTermS(evaluatedTerm)(err)(env3)
              case (env3, Failure(noType))        =>
                (env3, noType.failure)
            }
        } (env)
      case _ =>
        (env, GroupIdentity(DefaultGroupNodeIdentity, Nil).success)
    }
  
  def groupIdentityFromInferringTypeS[L, N, E](typ: InferringType[N])(env: E)(implicit unifier: Unifier[NoType[N], TypeValueTerm[N], E, Int], envSt: typer.TypeInferenceEnvironmentState[E, L, N], envSt2: TypeInferenceEnvironmentState[E, L, N], groupIdentEqual: Equal[GroupIdentity[N]]) =
    groupIdentityFromTypeValueTermS(typ.typeValueTerm)(envSt2.incorrectInstanceTypeNoTypeS)(env)
    
  def groupIdentityFromInferredTypeS[L, N, E](typ: InferredType[N])(env: E)(implicit unifier: Unifier[NoType[N], TypeValueTerm[N], E, Int], envSt: typer.TypeInferenceEnvironmentState[E, L, N], envSt2: TypeInferenceEnvironmentState[E, L, N], groupIdentEqual: Equal[GroupIdentity[N]]) = {
    typ.uninstantiatedTypeS(env) match {
      case (env2, inferringType @ InferringType(_)) =>
        groupIdentityFromInferringTypeS(inferringType)(env2)
      case (env2, noType: NoType[N])                =>
        (env2, noType.failure)
      case (env2, _)                                =>
        (env2, NoType.fromError[N](FatalError("inferred type or uninferred type", none, NoPosition)).failure)
    }
  }
}
