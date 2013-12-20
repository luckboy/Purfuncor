package pl.luckboy.purfuncor.frontend.instant
import scala.util.parsing.input.NoPosition
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.resolver.GlobalSymbolTabular
import pl.luckboy.purfuncor.frontend.kinder.InferredKind
import pl.luckboy.purfuncor.frontend.typer.DefinedType
import pl.luckboy.purfuncor.frontend.typer.Type
import pl.luckboy.purfuncor.frontend.typer.NoType
import pl.luckboy.purfuncor.frontend.typer.InferredType
import pl.luckboy.purfuncor.frontend.typer.InferringType
import pl.luckboy.purfuncor.frontend.typer.TypeValueTerm
import pl.luckboy.purfuncor.frontend.typer.TupleType
import pl.luckboy.purfuncor.frontend.typer.GlobalTypeApp
import pl.luckboy.purfuncor.frontend.typer.TypeConjunction
import pl.luckboy.purfuncor.frontend.typer.TypeDisjunction
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

  def withSaveS[T, U](f: E => (E, Validation[T, U]))(env: E): (E, Validation[T, U])
}

object PolyFunInstantiator {
  def instantiatePolyFunctionsS[L, M, N, I, E](lambdaInfoMaps: Map[Option[L], Map[Int, PreinstantiationLambdaInfo[L, N]]])(localInstTree: Option[InstanceTree[AbstractPolyFunction[L], N, LocalInstance[L]]])(env: E)(implicit polyFunInstantiator: PolyFunInstantiator[L, M, N, I, E]) = {
    val (env2, res) = justInstantiatePolyFunctionsS(lambdaInfoMaps)(localInstTree)(env)
    res.map {
      case (lambdaInfoMaps2, localInstTree) =>
        localInstTree.map {
          tmpInstTree =>
            combinatorInstanceArgsS(lambdaInfoMaps)(tmpInstTree).map {
              instArgs =>
                val (env3, _) = instArgs.foldLeft((env2, ())) {
                  case ((newEnv, _), (l, ias)) => polyFunInstantiator.addInstanceArgsS(l, ias)(newEnv)
                }
                val (env4, _) = lambdaInfoMaps2.foldLeft((env3, ())) {
                  case ((newEnv, _), (l, lis)) => polyFunInstantiator.addLambdaInfosS(l, lis)(newEnv)
                }
                (env4, ().successNel)
            }.valueOr { errs => (env2, errs.failure) }
        }.getOrElse((env2, FatalError("no local instance tree", none, NoPosition).failureNel))
    }.valueOr { errs => (env2, errs.failure) }
  }
  
  private def instantiatePolyFunctions2[L, M, N, I, E](lambdaInfoMaps: Map[Option[L], Map[Int, PreinstantiationLambdaInfo[L, N]]])(localInstTree: Option[InstanceTree[AbstractPolyFunction[L], N, LocalInstance[L]]])(implicit polyFunInstantiator: PolyFunInstantiator[L, M, N, I, E]) =
    State(instantiatePolyFunctionsS[L, M, N, I, E](lambdaInfoMaps)(localInstTree))

  def instantiatePolyFunctions[L, M, N, I, E](lambdaInfoMaps: Map[Option[L], Map[Int, PreinstantiationLambdaInfo[L, N]]])(localInstTree: Option[InstanceTree[AbstractPolyFunction[L], N, LocalInstance[L]]])(implicit polyFunInstantiator: PolyFunInstantiator[L, M, N, I, E]) =
    instantiatePolyFunctions2[L, M, N, I, E](lambdaInfoMaps)(localInstTree)
  
  private def justInstantiatePolyFunctionsS[L, M, N, I, E](lambdaInfoMaps: Map[Option[L], Map[Int, PreinstantiationLambdaInfo[L, N]]])(localInstTree: Option[InstanceTree[AbstractPolyFunction[L], N, LocalInstance[L]]])(env: E)(implicit polyFunInstantiator: PolyFunInstantiator[L, M, N, I, E]) = {
    val (env2, (res, localInstTree2)) = lambdaInfoMaps.foldLeft((env, (Map[Option[L], Map[Int, InstantiationLambdaInfo[L]]]().successNel[AbstractError], localInstTree))) {
      case ((newEnv, (newRes, newLocalInstTree)), (polyFun, lambdaInfos)) =>
        val (newEnv6, (newRes5, newLocalInstTree3)) = lambdaInfos.foldLeft((newEnv, (Map[Int, InstantiationLambdaInfo[L]]().successNel[AbstractError], newLocalInstTree))) {
          case ((newEnv2, (newRes2, newLocalInstTree2)), (i, lambdaInfo)) =>
            val (newEnv3, newRes3) = lambdaInfo.polyFun.map {
              polyFun =>
                polyFun match {
                  case PolyFunction(polyFunLoc) =>
                    polyFunInstantiator.getInstanceArgsFromEnvironmentS(polyFunLoc)(newEnv2).mapElements(identity, _.toSuccess(NonEmptyList(FatalError("undefined global variable", none, NoPosition))))
                  case _                        => (newEnv2, Seq().successNel)
                }
            }.getOrElse((newEnv2, FatalError("no polynomial function", none, NoPosition).failureNel))
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
    val instArgs = localInstTree.instTables.flatMap {
      case (pf, it) => it.pairs.map { case (t, LocalInstance(i)) => (i, InstanceArg(pf, t.typ)) }
    }
    (0 until instArgs.size).foldLeft(some(Seq[InstanceArg[L, M]]())) {
      (optIas, i) => optIas.flatMap { ias => instArgs.get(i).map { ias :+ _ } }
    }.toSuccess(NonEmptyList(FatalError("index of out bounds", none, NoPosition)))
  }

  private def normalizeInstanceArgs[L, N](instArgs: Seq[InstanceArg[L, N]], params: Map[Int, Int]) = 
    instArgs.foldLeft(Seq[InstanceArg[L, N]]().successNel[AbstractError]) {
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
  
  private def combinatorInstanceArgsS[L, N](lambdaInfoMaps: Map[Option[L], Map[Int, PreinstantiationLambdaInfo[L, N]]])(localInstTree: InstanceTree[AbstractPolyFunction[L], N, LocalInstance[L]]) =
    instanceArgsFromLocalInstanceTree(localInstTree).flatMap {
      instArgs =>
        lambdaInfoMaps.foldLeft(Map[L, Seq[InstanceArg[L, N]]]().successNel[AbstractError]) {
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
              res7 <- res.map {
                case (inferringTypeValueTerm, typeParams) =>
                  for {
                    res2 <- State({
                      (env2: E) =>
                        instArgs.foldLeft((env2, Seq[InferringType[N]]().success[NoType[N]])) {
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
                                  case noType: NoType[N] =>
                                    State((_: E, NoType.fromError[N](FatalError("mismatched types", none, NoPosition)).failure))
                                  case _ =>
                                    State({
                                      (env2: E) =>
                                        instArgs.zip(instInferringTypes).foldLeft((env2, Seq[InstanceArg[L, N]]().success[NoType[N]])) {
                                          case ((newEnv, Success(newInstArgs)), (instArg, instInferringType)) =>
                                            val (newEnv2, newInstType) = instInferringType.instantiatedTypeForParamsS(polyFunTypeParams.map { _.swap }.toMap)(newEnv)
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
                                })
                              } yield res4
                          }.valueOr { nt => State((_: E, nt.failure)) }
                        } yield res5
                    }.valueOr { nt => State((_: E, nt.failure)) }
                  } yield res6
              }.valueOr { nt => State((_: E, nt.failure)) }
            } yield res7).run(env)
        }.getOrElse((env, NoType.fromError[N](FatalError("no poly function type", none, NoPosition)).failure))
      case polyFun @ (ConstructFunction | SelectFunction) =>
        (env, lambdaInfo.polyFunType.map { pft => Seq(InstanceArg(polyFun, pft)).success }.getOrElse(NoType.fromError[N](FatalError("no poly function type", none, NoPosition)).failure))
    }.getOrElse((env, Seq().success))
  
  def instantiatePolyFunctionS[L, N, E](lambdaInfo: PreinstantiationLambdaInfo[L, N], instArgs: Seq[InstanceArg[L, N]], globalInstTree: InstanceTree[AbstractPolyFunction[L], N, GlobalInstance[L]])(localInstTree: Option[InstanceTree[AbstractPolyFunction[L], N, LocalInstance[L]]])(env: E)(implicit unifier: Unifier[NoType[N], TypeValueTerm[N], E, Int], envSt: typer.TypeInferenceEnvironmentState[E, L, N], envSt2: TypeInferenceEnvironmentState[E, L, N]) =
    envSt2.withInstanceTypeClearingS {
      newEnv =>
        val (newEnv2, newRes) = instanceArgsFromPreinstantiationLambdaInfoS(lambdaInfo, instArgs)(newEnv)
        val (newEnv7, newRes5) = newRes.map {
          instArgs =>
            instArgs.foldLeft((newEnv2, (Seq[Instance[L]](), localInstTree).success[NoType[N]])) {
              case ((newEnv3, newRes), instArg @ InstanceArg(polyFun, typ)) =>
                val (newEnv4, newRes2) = globalInstTree.findInstsS(polyFun, GlobalInstanceType(typ))(newEnv3)
                val (newEnv6, newRes3) = newRes2.map {
                  case Seq(inst) =>
                    (newEnv4, (inst, localInstTree).success)
                  case insts =>
                    localInstTree.map {
                      tmpInstTree =>
                        val inst = LocalInstance[L](tmpInstTree.instCount)
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
            }.mapElements(identity, _.map { case (is, it) => (InstantiationLambdaInfo(is), it) })
        }.valueOr { nt => (newEnv2, nt.failure) }
        (newEnv7, newRes5.swap.map { _.withPos(lambdaInfo.pos).forFile(lambdaInfo.file) }.swap)
    } (env)
    
  private def illegalTypeNoType[T] = NoType.fromError[T](FatalError("illegal type", none, NoPosition))
    
  private def findTupleTypesS[L, N, E](term: TypeValueTerm[N])(env: E)(implicit unifier: Unifier[NoType[N], TypeValueTerm[N], E, Int], envSt: typer.TypeInferenceEnvironmentState[E, L, N]): (E, Validation[NoType[N], Seq[TupleType[N]]]) =
    term match {
      case tupleType @ TupleType(_) =>
        (env, Seq(tupleType).success)
      case TypeConjunction(terms) =>
        terms.foldLeft((env, Seq[TupleType[N]]().success[NoType[N]])) {
          case ((newEnv, Success(Seq())), term2) => findTupleTypesS(term2)(newEnv)
          case ((newEnv, Success(_)), _)         => (newEnv, illegalTypeNoType.failure)
          case ((newEnv, newRes), _)             => (newEnv, newRes)
        }
      case TypeDisjunction(terms) =>
        terms.foldLeft((env, Seq[TupleType[N]]().success[NoType[N]])) {
          case ((newEnv, Success(newTerms)), term2) =>
            findTupleTypesS(term2)(newEnv) match {
              case (newEnv2, Success(Seq()))  => (newEnv2, illegalTypeNoType.failure)
              case (newEnv2, Success(terms2)) => (newEnv2, (newTerms ++ terms2).success)
              case (newEnv2, Failure(noType)) => (newEnv2, noType.failure)
            }
          case ((newEnv, Failure(noType)), _)             =>
            (newEnv, noType.failure)
        }
      case GlobalTypeApp(loc, args, _) =>
        envSt.withRecursionCheckingS(Set(loc)) {
          env2 =>
            appForGlobalTypeWithAllocatedTypeParamsS(loc, args)(env2) match {
              case (env3, Success(evaluatedTerm)) =>
                findTupleTypesS(evaluatedTerm)(env3)
              case (env3, Failure(noType))        =>
                (env3, noType.failure)
            }
        } (env)
      case _ =>
        (env, Seq().success)
    }
  
  def checkConstructInferringTypeS[L, N, E](typ: InferringType[N])(env: E)(implicit unifier: Unifier[NoType[N], TypeValueTerm[N], E, Int], envSt: typer.TypeInferenceEnvironmentState[E, L, N], envSt2: TypeInferenceEnvironmentState[E, L, N]) = {
    val (env2, res) = findTupleTypesS(typ.typeValueTerm)(env)
    res.map {
      typeValueTerms =>
        unifier.withSaveS {
          env3 =>
            typeValueTerms.headOption.map {
              firstTypeValueTerm =>
                (for {
                  _ <- State(envSt2.addDefinedTypeS(DefinedType.fromInferringType(typ))(_: E))
                  savedTypeMatching <- State(envSt.currentTypeMatchingFromEnvironmentS(_: E))
                  _ <- State(envSt.setCurrentTypeMatchingS(TypeMatching.Types)(_: E))
                  unifiedType <- State({
                    (env4: E) =>
                      typeValueTerms.tail.foldLeft((env4, InferringType(firstTypeValueTerm): Type[N])) {
                        case ((newEnv, newType: InferredType[N]), typeValueTerm) =>
                          unifyTypesS(newType, InferringType(typeValueTerm))(newEnv)
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
                      State((_: E, illegalTypeNoType[N].failure))
                    case _                               =>
                      State((_: E, NoType.fromError[N](FatalError("uninferring type", none, NoPosition)).failure))
                  }
                } yield res4).run(env3)
            }.getOrElse((env3, illegalTypeNoType[N].failure))            
        } (env2).mapElements(identity, _.valueOr(identity))
    }.valueOr { (env2, _) }
  }
  
  def checkConstructInferringType[L, N, E](typ: InferringType[N])(implicit unifier: Unifier[NoType[N], TypeValueTerm[N], E, Int], envSt: typer.TypeInferenceEnvironmentState[E, L, N], envSt2: TypeInferenceEnvironmentState[E, L, N]) =
    State(checkConstructInferringTypeS[L, N, E](typ))
}