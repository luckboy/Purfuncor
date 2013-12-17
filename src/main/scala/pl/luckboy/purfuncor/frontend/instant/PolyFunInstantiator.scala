package pl.luckboy.purfuncor.frontend.instant
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbolTabular
import pl.luckboy.purfuncor.frontend.kinder.InferredKind
import pl.luckboy.purfuncor.frontend.typer.Type
import pl.luckboy.purfuncor.frontend.typer.NoType
import pl.luckboy.purfuncor.frontend.typer.InferredType
import pl.luckboy.purfuncor.frontend.typer.InferringType
import pl.luckboy.purfuncor.frontend.typer.TypeValueTerm
import pl.luckboy.purfuncor.frontend.typer.TypeMatching
import pl.luckboy.purfuncor.common.Inferrer._
import pl.luckboy.purfuncor.frontend.typer.TypeInferrer._
import pl.luckboy.purfuncor.frontend.typer.TypeValueTermUtils._

trait PolyFunInstantiator[L, M, E] {
  def instantiatePolyFunctionS(lambdaInfo: PreinstantiationLambdaInfo[L, M], instArgs: Seq[InstanceArg[L, M]])(localInstTree: Option[InstanceTree[AbstractPolyFunction[L], M, LocalInstance[L]]])(env: E): (E, ValidationNel[AbstractError, (InstantiationLambdaInfo[L, M], Option[InstanceTree[AbstractPolyFunction[L], M, LocalInstance[L]]])])
  
  def getLambdaInfosFromEnvironmentS(loc: Option[L])(env: E): (E, Option[Map[Int, InstantiationLambdaInfo[L, M]]])
  
  def addLambdaInfosS(loc: Option[L], lambdaInfos: Map[Int, InstantiationLambdaInfo[L, M]])(env: E): (E, Unit)
}

object PolyFunInstantiator {
  def instantiatePolyFunctionsS[L, M, E](lambdaInfoMaps: Map[Option[L], Map[Int, PreinstantiationLambdaInfo[L, M]]])(localInstTree: Option[InstanceTree[AbstractPolyFunction[L], M, LocalInstance[L]]])(env: E)(implicit polyFunInstantiator: PolyFunInstantiator[L, M, E]) = {
    val (env2, res) = instantiateOnlyPolyFunctionsS(lambdaInfoMaps)(localInstTree)(env)
    res.map {
      case (lambdaInfoMaps2, localInstTree) =>
        localInstTree.map {
          lambdaInfosWithInstanceArgsFromLocalInstanceTree(lambdaInfoMaps2, lambdaInfoMaps, _).map {
            lambdaInfoMaps3 =>
              lambdaInfoMaps3.foldLeft((env2, ().successNel[AbstractError])) {
                case ((newEnv, Success(_)), (l, lis)) => polyFunInstantiator.addLambdaInfosS(l, lis)(newEnv).mapElements(identity, _.successNel)
                case ((newEnv, Failure(es)), _)       => (newEnv, es.failure)
              }
            }.valueOr { es => (env2, es.failure) }
        }.getOrElse((env2, FatalError("no local instance tree", none, NoPosition).failureNel))
    }.valueOr { es => (env2, es.failure) }
  }
  
  private def instantiatePolyFunctions2[L, M, E](lambdaInfoMaps: Map[Option[L], Map[Int, PreinstantiationLambdaInfo[L, M]]])(localInstTree: Option[InstanceTree[AbstractPolyFunction[L], M, LocalInstance[L]]])(implicit polyFunInstantiator: PolyFunInstantiator[L, M, E]) =
    State(instantiatePolyFunctionsS[L, M, E](lambdaInfoMaps)(localInstTree))

  def instantiatePolyFunctions[L, M, E](lambdaInfoMaps: Map[Option[L], Map[Int, PreinstantiationLambdaInfo[L, M]]])(localInstTree: Option[InstanceTree[AbstractPolyFunction[L], M, LocalInstance[L]]])(implicit polyFunInstantiator: PolyFunInstantiator[L, M, E]) =
    instantiatePolyFunctions2[L, M, E](lambdaInfoMaps)(localInstTree)
  
  private def instantiateOnlyPolyFunctionsS[L, M, E](lambdaInfoMaps: Map[Option[L], Map[Int, PreinstantiationLambdaInfo[L, M]]])(localInstTree: Option[InstanceTree[AbstractPolyFunction[L], M, LocalInstance[L]]])(env: E)(implicit polyFunInstantiator: PolyFunInstantiator[L, M, E]) = {
    val (env2, (res, localInstTree2)) = lambdaInfoMaps.foldLeft((env, (Map[Option[L], Map[Int, InstantiationLambdaInfo[L, M]]]().successNel[AbstractError], localInstTree))) {
      case ((newEnv, (newRes, newLocalInstTree)), (polyFun, lambdaInfos)) =>
        val (newEnv6, (newRes4, newLocalInstTree3)) = lambdaInfos.foldLeft((newEnv, (Map[Int, InstantiationLambdaInfo[L, M]]().successNel[AbstractError], newLocalInstTree))) {
          case ((newEnv2, (newRes2, newLocalInstTree2)), (i, lambdaInfo)) =>
            val (newEnv4, instArgs) = lambdaInfo.polyFun match {
              case Some(PolyFunction(polyFunLoc)) =>
                val (newEnv3, optLambdaInfos) = polyFunInstantiator.getLambdaInfosFromEnvironmentS(some(polyFunLoc))(newEnv2)
                (newEnv3, optLambdaInfos.flatMap { _.get(0).map { _.instArgs } }.getOrElse(Nil))
              case _ =>
                (newEnv2, Nil)
            }
            val (newEnv5, newRes3) = polyFunInstantiator.instantiatePolyFunctionS(lambdaInfo, instArgs)(newLocalInstTree2)(newEnv4)
            (newEnv5, ((newRes2 |@| newRes3) { case (lis, (li, _)) => lis + (i -> li) }, newRes3.map { _._2 }.getOrElse(newLocalInstTree2)))
        }
        (newEnv6, ((newRes |@| newRes4) { (liMaps, lis) => liMaps + (polyFun -> lis) }, newLocalInstTree3))
    }
    (env2, res.map { (_, localInstTree2).successNel }.valueOr { _.failure })
  }

  private def instanceArgsFromLocalInstanceTree[L, M](localInstTree: InstanceTree[AbstractPolyFunction[L], M, LocalInstance[L]]) = {
    val instArgs = localInstTree.instTables.flatMap {
      case (pf, it) => it.pairs.map { case (t, LocalInstance(i)) => (i, InstanceArg(pf, t.typ)) }
    }
    (0 until instArgs.size).foldLeft(some(Seq[InstanceArg[L, M]]())) {
      (optIas, i) => optIas.flatMap { ias => instArgs.get(i).map { ias :+ _ } }
    }.toSuccess(NonEmptyList(FatalError("index of out bounds", none, NoPosition)))
  }

  private def normalizeInstanceArgs[L, M](instArgs: Seq[InstanceArg[L, M]], params: Map[Int, Int]) = 
    instArgs.foldLeft(Seq[InstanceArg[L, M]]().successNel[AbstractError]) {
      case (Success(newInstArgs), InstanceArg(polyFun, typ)) =>
        val typeValueTerm = normalizeTypeParamsForParams(typ.typeValueTerm, params.size)(params)
        val argKindMap = params.foldLeft(Map[Int, InferredKind]()) {
          case (ks, (p, p2)) => typ.argKinds.lift(p).map { k => ks + (p2 -> k) }.getOrElse(ks)
        }
        val optArgKinds = (0 until params.size).foldLeft(some(Seq[InferredKind]())) {
          case (oks, i) => oks.flatMap { ks => argKindMap.get(i).map { k => ks :+ k } }
        }
        optArgKinds.map {
          argKinds => (newInstArgs :+ InstanceArg(polyFun, InferredType(typeValueTerm, argKinds))).successNel
        }.getOrElse(FatalError("index of out bounds", none, NoPosition).failureNel)
      case (Failure(errs), _)                                =>
        errs.failure
    }
  
  def lambdaInfosWithInstanceArgsFromLocalInstanceTree[L, M](lambdaInfoMaps: Map[Option[L], Map[Int, InstantiationLambdaInfo[L, M]]], oldLambdaInfoMaps: Map[Option[L], Map[Int, PreinstantiationLambdaInfo[L, M]]], localInstTree: InstanceTree[AbstractPolyFunction[L], M, LocalInstance[L]]) =
    instanceArgsFromLocalInstanceTree(localInstTree).flatMap {
      instArgs =>
        lambdaInfoMaps.foldLeft(Map[Option[L], Map[Int, InstantiationLambdaInfo[L, M]]]().successNel[AbstractError]) {
          case (Success(newLambdaInfoMaps), (Some(loc), lambdaInfos)) =>
            lambdaInfos.get(0).map {
              lambdaInfo =>
                oldLambdaInfoMaps.get(some(loc)).map {
                  oldLambdaInfos =>
                    oldLambdaInfos.get(0).map {
                      oldLambdaInfo =>
                        normalizeInstanceArgs(instArgs, oldLambdaInfo.combTypeParams).map {
                          instArgs2 =>
                            newLambdaInfoMaps + (some(loc) -> (lambdaInfos + (0 -> lambdaInfo.copy(instArgs = instArgs2))))
                        }
                    }.getOrElse(FatalError("no old lambda information", none, NoPosition).failureNel)
                }.getOrElse(FatalError("no old lambda informations", none, NoPosition).failureNel)
            }.getOrElse(FatalError("no lambda information", none, NoPosition).failureNel)
          case (Success(newLambdaInfoMaps), (None, _)) =>
            newLambdaInfoMaps.successNel
          case (Failure(errs), _) =>
            errs.failure
        }
    }
    
  private def instanceArgsFromPreinstantiationLambdaInfoS[L, M, E](lambdaInfo: PreinstantiationLambdaInfo[L, M], instArgs: Seq[InstanceArg[L, M]])(env: E)(implicit unifier: Unifier[NoType[M], TypeValueTerm[M], E, Int], envSt: typer.TypeInferenceEnvironmentState[E, L, M], envSt2: TypeInferenceEnvironmentState[E, L, M]) =
    lambdaInfo.polyFun.map {
      case PolyFunction(polyFunLoc) =>
        lambdaInfo.polyFunType.map {
          polyFunType =>
            (for {
              typ <- State(envSt2.globalVarTypeFromEnvironmentS(polyFunLoc)(_: E))
              res <- State(typ.uninstantiatedTypeValueTermWithTypeParamsS(_: E))
              res7 <- res.map {
                case (inferringTypeValueTerm, typeParams) =>
                  for {
                    res2 <- State({
                      (env2: E) =>
                        instArgs.foldLeft((env2, Seq[InferringType[M]]().success[NoType[M]])) {
                          case ((newEnv, Success(newInstTypes)), instArg) =>
                            val (newEnv2, newRes) = instArg.typ.uninstantiatedTypeValueTermWithTypeParamsS(newEnv)
                            newRes.map {
                              case (instInferringTypeValueTerm, instTypeParams) =>
                                val (newEnv4, newRes2) = typeParams.foldLeft((newEnv2, ().success[NoType[M]])) {
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
                    res6 <- res2.map {
                      instInferringTypes =>
                        for {
                          res3 <- State(polyFunType.uninstantiatedTypeValueTermWithTypeParamsS(_: E))
                          res5 <- res3.map {
                            case (polyFunTypeValueTerm, polyFunTypeParams) =>
                              for {
                                _ <- State(envSt.setCurrentTypeMatchingS(TypeMatching.Types)(_: E))
                                unifiedType <- unifyTypes(InferringType(inferringTypeValueTerm), InferringType(polyFunTypeValueTerm))
                                res4 <- (unifiedType match {
                                  case noType: NoType[M] =>
                                    State((_: E, NoType.fromError[M](FatalError("mismatched types", none, NoPosition)).failure))
                                  case _ =>
                                    State({
                                      (env2: E) =>
                                        instArgs.zip(instInferringTypes).foldLeft((env2, Seq[InstanceArg[L, M]]().success[NoType[M]])) {
                                          case ((newEnv, Success(newInstArgs)), (instArg, instInferringType)) =>
                                            val (newEnv2, newInstType) = instInferringType.instantiatedTypeForParamsS(polyFunTypeParams.map { _.swap }.toMap)(newEnv)
                                            newInstType match {
                                              case newInstInferredType: InferredType[M] =>
                                                (newEnv2, (newInstArgs :+ instArg.copy(typ = newInstInferredType)).success)
                                              case noType: NoType[M] =>
                                                (newEnv2, noType.failure)
                                              case _ =>
                                                (newEnv2, NoType.fromError[M](FatalError("uninferred type", none, NoPosition)).failure)
                                            }
                                        }
                                    })
                                })
                              } yield res4
                          }.valueOr { nt => State((_: E, nt.failure)) }
                        } yield res5
                    }.valueOr { nt => State((_: E, nt.failure)) }
                  } yield res6
              }.valueOr { nt => State((_: E, nt.failure)) }
            } yield res7).run(env)
        }.getOrElse((env, NoType.fromError[M](FatalError("no poly function type", none, NoPosition)).failure))
      case polyFun @ (ConstructFunction | SelectFunction) =>
        (env, lambdaInfo.polyFunType.map { pft => Seq(InstanceArg(polyFun, pft)).success }.getOrElse(NoType.fromError[M](FatalError("no poly function type", none, NoPosition)).failure))
    }.getOrElse((env, Seq().success))
  
  def instantiatePolyFunctionS[L, M, E](lambdaInfo: PreinstantiationLambdaInfo[L, M], instArgs: Seq[InstanceArg[L, M]], globalInstTree: InstanceTree[AbstractPolyFunction[L], M, GlobalInstance[L]])(localInstTree: Option[InstanceTree[AbstractPolyFunction[L], M, LocalInstance[L]]])(env: E)(implicit unifier: Unifier[NoType[M], TypeValueTerm[M], E, Int], envSt: typer.TypeInferenceEnvironmentState[E, L, M], envSt2: TypeInferenceEnvironmentState[E, L, M]): (E, Validation[NoType[M], (InstantiationLambdaInfo[L, M], Option[InstanceTree[AbstractPolyFunction[L], M, LocalInstance[L]]])]) = {
    envSt2.withInstanceTypeClearingS {
      newEnv =>
        val (newEnv2, newRes) = instanceArgsFromPreinstantiationLambdaInfoS(lambdaInfo, instArgs)(newEnv)
        val (newEnv7, newRes5) = newRes.map {
          instArgs =>
            instArgs.foldLeft((newEnv2, (Seq[Instance[L]](), localInstTree).success[NoType[M]])) {
              case ((newEnv3, newRes), instArg @ InstanceArg(polyFun, typ)) =>
                val (newEnv4, newRes2) = globalInstTree.findInstsS(polyFun, GlobalInstanceType(typ))(newEnv3)
                val (newEnv6, newRes3) = newRes2.map {
                  case Seq(inst) =>
                    (newEnv4, (inst, localInstTree).success)
                  case insts =>
                    localInstTree.map {
                      tmpInstTree =>
                        val inst = LocalInstance[L](tmpInstTree.countInsts)
                        val (newEnv5, newRes4) = tmpInstTree.addInstS(polyFun, LocalInstanceType(typ), inst)(newEnv4)
                        newRes4.map {
                          _.map {
                            case (it, _) => (newEnv5, (inst, some(it)).success)
                          }.getOrElse {
                            envSt2.ambiguousInstanceNoTypeS(instArg)(newEnv4).mapElements(identity, _.failure)
                          }
                        }.valueOr { nt => (newEnv5, nt.failure) }
                    }.getOrElse {
                      if (insts.isEmpty)
                        envSt2.notFoundInstanceNoTypeS(instArg)(newEnv4).mapElements(identity, _.failure)
                      else
                        envSt2.ambiguousInstanceNoTypeS(instArg)(newEnv4).mapElements(identity, _.failure)
                    }
                }.valueOr { nt => (newEnv4, nt.failure) }
                (newEnv6, (newRes |@| newRes3) { case ((is, _), (i, it)) => (is :+ i, it) })
            }.mapElements(identity, _.map { case (is, it) => (InstantiationLambdaInfo[L, M](insts = is, Seq()), it) })
        }.valueOr { nt => (newEnv2, nt.failure) }
        (newEnv7, newRes5.swap.map { _.withPos(lambdaInfo.pos).forFile(lambdaInfo.file) }.swap)
    } (env)
  }
}