/*******************************************************************************
 * Copyright (c) 2014 Łukasz Szpakowski.
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 ******************************************************************************/
package pl.luckboy.purfuncor.frontend.instant
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import pl.luckboy.purfuncor.frontend.typer.DefinedType
import pl.luckboy.purfuncor.frontend.typer.Type
import pl.luckboy.purfuncor.frontend.typer.NoType
import pl.luckboy.purfuncor.frontend.typer.InferredType
import pl.luckboy.purfuncor.frontend.typer.InferringType
import pl.luckboy.purfuncor.frontend.typer.TypeValueTerm
import pl.luckboy.purfuncor.frontend.typer.TypeMatching
import pl.luckboy.purfuncor.frontend.typer.TypeInferrer._
import pl.luckboy.purfuncor.frontend.typer.TypeValueTermUnifier._

case class InstanceTree[T, U, V](instGroupTables: Map[T, InstanceGroupTable[U, V]])
{
  def findInstsS[W, E](loc: T, typ: InstanceType[U])(env: E)(implicit unifier: Unifier[NoType[U], TypeValueTerm[U], E, Int], envSt: typer.TypeInferenceEnvironmentState[E, W, U], envSt2: TypeInferenceEnvironmentState[E, W, U], groupIdentEqual: Equal[GroupIdentity[U]]) =
    instGroupTables.get(loc).map { _.findInstsS(typ)(env) }.getOrElse((env, Seq().success))
  
  def addInstS[W, E](loc: T, typ: InstanceType[U], inst: V)(env: E)(implicit unifier: Unifier[NoType[U], TypeValueTerm[U], E, Int], envSt: typer.TypeInferenceEnvironmentState[E, W, U], envSt2: TypeInferenceEnvironmentState[E, W, U], groupIdentEqual: Equal[GroupIdentity[U]]) = {
    val (env2, res) = instGroupTables.getOrElse(loc, InstanceGroupTable.empty[U, V]).addInstS(typ, inst)(env)
    (env2, res.map { _.map { case (igt, b) => (InstanceTree(instGroupTables + (loc -> igt)), b) } })
  }
  
  def insts = instGroupTables.values.flatMap { _.insts }
    
  def instCount = instGroupTables.values.foldLeft(0) { _ + _.instCount }
}

object InstanceTree
{
  def empty[T, U, V] = fromInstanceGroupTables[T, U, V](Map())
  
  def fromInstanceGroupTables[T, U, V](instGroupTables: Map[T, InstanceGroupTable[U, V]]) = InstanceTree[T, U, V](instGroupTables)
}

case class InstanceGroupTable[T, U](instGroups: Map[GroupIdentity[T], InstanceGroup[T, U]])
{
  def findInstsS[V, E](typ: InstanceType[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: typer.TypeInferenceEnvironmentState[E, V, T], envSt2: TypeInferenceEnvironmentState[E, V, T], groupIdentEqual: Equal[GroupIdentity[T]]) =  {
    val (env2, res) = PolyFunInstantiator.groupIdentityFromInferredTypeS(typ.typ)(env)
    res.map {
      instGroups.get(_).map { _.findInstsS(typ)(env2) }.getOrElse((env2, Vector().success[NoType[T]]))
    }.valueOr { nt => (env2, nt.failure) }
  }
  
  def addInstS[V, E](typ: InstanceType[T], inst: U)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: typer.TypeInferenceEnvironmentState[E, V, T], envSt2: TypeInferenceEnvironmentState[E, V, T], groupIdentEqual: Equal[GroupIdentity[T]]) = {
    val (env2, res) = PolyFunInstantiator.groupIdentityFromInferredTypeS(typ.typ)(env)
    res.map {
      groupIdent =>
        instGroups.get(groupIdent).map { 
          _.addInstS(typ, inst)(env2).mapElements(identity, _.map { _.map { case (ig, b) => (InstanceGroupTable(instGroups + (groupIdent -> ig)), b) } }) 
        }.getOrElse((env2, none.success))
    }.valueOr { nt => (env2, nt.failure) }
  }
  
  def insts = instGroups.toSeq.flatMap { _._2.pairs.map { _._2 } }
  
  def instCount = instGroups.values.foldLeft(0) { _ + _.instCount }
}

object InstanceGroupTable
{
  def empty[T, U] = InstanceGroupTable[T, U](Map())
  
  def fromInstanceGroups[T, U](instGroups: Map[GroupIdentity[T], InstanceGroup[T, U]]) = InstanceGroupTable(instGroups)
}

case class InstanceGroup[T, U](pairs: Seq[(InstanceType[T], U)])
{
  private def matchesInstTypesS[V, E](typ: InstanceType[T], instType: InstanceType[T], typeMatching: TypeMatching.Value)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: typer.TypeInferenceEnvironmentState[E, V, T], envSt2: TypeInferenceEnvironmentState[E, V, T]) =
    envSt2.withInstanceTypeClearingS {
      newEnv =>
        val (newEnv5, newRes3) = (typ, instType) match {
          case (_: LocalInstanceType[T], _: LocalInstanceType[T]) =>
            val (newEnv2, newRes) = typ.typ.uninstantiatedTypeValueTermWithTypeParamsS(newEnv)
            val (newEnv3, newRes2) = instType.typ.uninstantiatedTypeValueTermWithTypeParamsS(newEnv2)
            (for(p <- newRes; ip <- newRes2) yield {
              val ((tvt, ps), (itvt, ips)) = (p, ip)
              ps.foldLeft((newEnv3, ().success[NoType[T]])) {
                case ((newEnv4, Success(_)), (param, param2)) => 
                  ips.get(param).map {
                    unifier.unionParamsS(param2, _)(newEnv4).mapElements(identity, _.map { _ => () })
                  }.getOrElse((newEnv4, ().success))
                case ((newEnv4, Failure(noType)), _)          =>
                  (newEnv4, noType.failure)
              }.mapElements(identity, _.map { _ => (InferringType(tvt), InferringType(itvt)) })
            }).valueOr { nt => (newEnv3, nt.failure) }
          case (_, _)                                             =>
            val (newEnv2, newRes) = typ.typ.uninstantiatedTypeValueTermS(newEnv)
            val (newEnv3, newRes2) = instType.typ.uninstantiatedTypeValueTermS(newEnv2)
            (newEnv3, for(tvt <- newRes; itvt <- newRes2) yield (InferringType(tvt), InferringType(itvt)))
        }
        newRes3.map {
          case (inferringType, instInferringType) =>
            val (newEnv6, _) = envSt2.addDefinedTypeS(DefinedType.fromInferringType(inferringType))(newEnv5)
            val (newEnv7, _) = envSt.setCurrentTypeMatchingS(typeMatching)(newEnv6)
            unifyTypesS(inferringType, instInferringType)(newEnv7) match {
              case (newEnv8, noType: NoType[T]) =>
                (newEnv8, if(noType.errs.forall { _.isInstanceOf[Error] }) false.success else noType.failure)
              case (newEnv8, _)                 =>
                val (newEnv9, definedTypes) = envSt2.definedTypesFromEnvironmentS(newEnv8)
                val (newEnv10, newRes4) = checkDefinedTypesS(definedTypes)(newEnv9)
                (newEnv10, newRes4.map { _ => true.success }.valueOr {
                  nt => if(nt.errs.forall { _.isInstanceOf[Error] }) false.success else nt.failure
                })
            }
        }.valueOr { nt => (newEnv5, nt.failure)}
    } (env)
  
  private def findInstsWithIndexesS[V, E](typ: InstanceType[T], typeMatching: TypeMatching.Value)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: typer.TypeInferenceEnvironmentState[E, V, T], envSt2: TypeInferenceEnvironmentState[E, V, T]) =
    pairs.zipWithIndex.foldLeft((env, Vector[(U, Int)]().success[NoType[T]])) {
      case ((newEnv, Success(newPairs)), ((instType, inst), i)) =>
        val (newEnv2, newRes) = matchesInstTypesS(typ, instType, typeMatching)(newEnv)
        (newEnv2, newRes.map { if(_) newPairs :+ (inst, i) else newPairs })
      case ((newEnv, Failure(noType)), _)                       =>
        (newEnv, noType.failure)
    }
  
  def findInstsS[V, E](typ: InstanceType[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: typer.TypeInferenceEnvironmentState[E, V, T], envSt2: TypeInferenceEnvironmentState[E, V, T]) =
    findInstsWithIndexesS(typ, TypeMatching.SupertypeWithType)(env).mapElements(identity, _.map { _.map { _._1 } })
  
  def addInstS[V, E](typ: InstanceType[T], inst: U)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: typer.TypeInferenceEnvironmentState[E, V, T], envSt2: TypeInferenceEnvironmentState[E, V, T]) = {
    val (env2, supertypePairListRes) = findInstsWithIndexesS(typ, TypeMatching.TypeWithSupertype)(env)
    val (env3, subtypePairListRes) = findInstsWithIndexesS(typ, TypeMatching.SupertypeWithType)(env2)
    (for { ps1 <- supertypePairListRes; ps2 <- subtypePairListRes } yield (ps1, ps2)) match {
      case Success((Seq(), Seq()))                     =>
        (env3, some((copy(pairs = pairs :+ (typ, inst)), none)).success)
      case Success((Seq((oldInst, i)), Seq()))         =>
        (env3, some((copy(pairs = pairs.updated(i, (typ, oldInst))), some(oldInst))).success)
      case Success((Seq(), Seq((oldInst, _))))         =>
        (env3, some((this, some(oldInst))).success)
      case Success((Seq((oldInst, i1)), Seq((_, i2)))) =>
        if(i1 === i2)
          (env3, some((copy(pairs = pairs.updated(i1, (typ, oldInst))), some(oldInst))).success)
        else
          (env3, none.success)
      case Success(_)                                  =>
        (env3, none.success)
      case Failure(noType)                             =>
        (env3, noType.failure)
    }
  }
  
  def withoutFirstInsts(n: Int) = copy(pairs.drop(n))
  
  def instCount = pairs.size
}

object InstanceGroup
{
  def empty[T, U] = InstanceGroup[T, U](Vector())
  
  def fromTuples[T, U](pairs: Seq[(InstanceType[T], U)]) = InstanceGroup(pairs)
}

sealed trait InstanceType[T]
{
  def typ: InferredType[T]
  
  override def toString =
    this match {
      case GlobalInstanceType(typ) => typ.toString + " /*global*/"
      case LocalInstanceType(typ)  => typ.toString + " /*local*/"
    }
}

case class GlobalInstanceType[T](typ: InferredType[T]) extends InstanceType[T]
case class LocalInstanceType[T](typ: InferredType[T]) extends InstanceType[T]
