package pl.luckboy.purfuncor.frontend.typer
import scalaz._
import scalaz.Scalaz._
import pl.luckboy.purfuncor.common._
import pl.luckboy.purfuncor.frontend._
import TypeInferrer._
import TypeValueTermUnifier._

case class InstanceTree[T, U, V](instTables: Map[T, InstanceTable[U, V]])
{
  def findInstsS[W, E](loc: T, typ: Type[U])(env: E)(implicit unifier: Unifier[NoType[U], TypeValueTerm[U], E, Int], envSt: TypeInferenceEnvironmentState[E, W, U]) =
    instTables.get(loc).map { _.findInstsS(typ)(env) }.getOrElse((env, Seq().success))
  
  def addInstS[W, E](loc: T, typ: Type[U], inst: V)(env: E)(implicit unifier: Unifier[NoType[U], TypeValueTerm[U], E, Int], envSt: TypeInferenceEnvironmentState[E, W, U]) = {
    val (env2, res) = instTables.getOrElse(loc, InstanceTable.empty).addInstS(typ, inst)(env)
    (env2, res.map { _.map { case (it, b) => (InstanceTree(instTables + (loc -> it)), b) } })
  }
  
  def countInsts = instTables.values.foldLeft(0) { _ + _.countInsts }
}

object InstanceTree
{
  def empty[T, U, V] = InstanceTree[T, U, V](Map())
  
  def fromGlobalInstanceTree[T, U, V](instTree: GlobalInstanceTree[T, U, V]) = InstanceTree(instTree.instTables.mapValues(InstanceTable.fromGlobalInstanceTable))
}

case class InstanceTable[T, U](pairs: Seq[(Type[T], U)])
{
  private def matchesInstTypesS[V, E](typ: Type[T], instType: Type[T], typeMatching: TypeMatching.Value)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T]) =
    envSt.withInstanceTypeClearingS {
      newEnv =>
        val (newEnv2, newRes) = typ.uninstantiatedTypeValueTermS(newEnv)
        newRes.map {
          typeValueTerm =>
            val inferringType = InferringType(typeValueTerm)
            val (newEnv3, _) = envSt.addDefinedTypeS(DefinedType.fromInferringType(inferringType))(newEnv2)
            val (newEnv4, _) = instType match {
              case instInferringType: InferringType[T] =>
                envSt.addDefinedTypeS(DefinedType.fromInferringType(instInferringType))(newEnv3)
              case _                                   =>
                (newEnv3, ())
            }
            val (newEnv5, unifiedType) = unifyTypesS(inferringType, instType)(newEnv4)
            unifiedType match {
              case noType: NoType[T] =>
                (newEnv5, if(noType.errs.forall { _.isInstanceOf[Error] }) false.success else noType.failure)
              case _                 =>
                val (newEnv6, definedTypes) = envSt.definedTypesFromEnvironmentS(newEnv5)
                val (newEnv7, newRes2) = checkDefinedTypesS(definedTypes)(newEnv6)
                (newEnv7, newRes.map { _ => true.success }.valueOr {
                  nt => if(nt.errs.forall { _.isInstanceOf[Error] }) false.success else nt.failure
                })
            }
        }.valueOr { nt => (newEnv2, nt.failure) }
    } (env)
  
  private def findInstsWithIndexesS[V, E](typ: Type[T], typeMatching: TypeMatching.Value)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T]) =
    pairs.zipWithIndex.foldLeft((env, Seq[(U, Int)]().success[NoType[T]])) {
      case ((newEnv, Success(newPairs)), ((instType, inst), i)) =>
        val (newEnv2, newRes) = matchesInstTypesS(typ, instType, typeMatching)(newEnv)
        (newEnv2, newRes.map { if(_) newPairs :+ (inst, i) else newPairs })
      case ((newEnv, Failure(noType)), _)                       =>
        (newEnv, noType.failure)
    }
  
  def findInstsS[V, E](typ: Type[T])(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T]) =
    findInstsWithIndexesS(typ, TypeMatching.SupertypeWithType)(env).mapElements(identity, _.map { _.map { _._1 } })
  
  def addInstS[V, E](typ: Type[T], inst: U)(env: E)(implicit unifier: Unifier[NoType[T], TypeValueTerm[T], E, Int], envSt: TypeInferenceEnvironmentState[E, V, T]) = {
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
  
  def countInsts = pairs.size
}

object InstanceTable
{
  def empty[T, U] = InstanceTable[T, U](Seq())
  
  def fromGlobalInstanceTable[T, U](instTable: GlobalInstanceTable[T, U]) = InstanceTable(instTable.pairs)
}